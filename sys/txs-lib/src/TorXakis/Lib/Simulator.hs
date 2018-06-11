{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module TorXakis.Lib.Simulator where

import           Control.Concurrent.STM.TVar (readTVarIO, writeTVar)
import           Control.Monad.Except        (throwError)
import           Control.Monad.State         (lift, liftIO)
import           Control.Monad.STM           (atomically)
import qualified Data.Map.Strict             as Map
import           Data.Semigroup              ((<>))
import qualified Data.Set                    as Set
import           Lens.Micro                  ((^.))
import           Name                        (Name)

import           TorXakis.Lens.TxsDefs       (ix)
import qualified TxsCore                     as Core
import qualified TxsDefs

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Internal
import           TorXakis.Lib.Session
import           TorXakis.Lib.SocketWorld

-- | Start the Simulator
setSim :: Name
       -> Name
       -> Name
       -> Session
       -> IO (Response ())
setSim mdlNm cnctNm mappNm s = runResponse $ do
    tDefs <- liftIO $ runReadOnlyIOC s Core.txsGetTDefs
    mDef <- maybe
                (throwError $ "No model named " <> mdlNm)
                return (tDefs ^. ix mdlNm)
    cDef <- maybe
                (throwError $ "No CnectDef named " <> cnctNm)
                return (tDefs ^. ix cnctNm)
    let mADef = tDefs ^. ix mappNm
    if isConsistent mDef mADef cDef
        then lift $ do
            st <- readTVarIO (s ^. sessionState)
            let fWCh = s ^. fromWorldChan
                prms = st ^. sessionParams
                Just (deltaString,_) = Map.lookup "param_Sut_deltaTime" prms
                deltaTime = read deltaString
            (wcd,tids) <- initSocketWorld s fWCh cDef
            atomically $ do
                writeTVar (s ^. wConnDef) wcd
                writeTVar (s ^. worldListeners) tids
            runIOC s $
                Core.txsSetSim
                    (lift <$> putToW deltaTime fWCh (wcd ^. toWorldMappings))
                    (lift $ getFromW deltaTime fWCh)
                    mDef mADef
        else throwError "Wrong or inconsistent parameters"
      where
        isConsistent :: TxsDefs.ModelDef
                     -> Maybe TxsDefs.MapperDef
                     -> TxsDefs.CnectDef
                     -> Bool
        isConsistent (TxsDefs.ModelDef minsyncs moutsyncs _ _)
                     Nothing
                     (TxsDefs.CnectDef _ conndefs) =
            let { mins   = Set.fromList minsyncs
                ; mouts  = Set.fromList moutsyncs
                ; ctows  = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDtoW  chn _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDfroW chn _ _ _ _ <- conndefs ]
                }
            in  mins == cfrows && ctows == mouts
        isConsistent _
                     (Just (TxsDefs.MapperDef achins achouts asyncsets _))
                     (TxsDefs.CnectDef _ conndefs) =
            let { ctows  = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDtoW  chn _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDfroW chn _ _ _ _ <- conndefs ]
                ; ains   = Set.fromList $ filter (not . Set.null)
                                [ sync `Set.intersection` Set.fromList achins  | sync <- asyncsets ]
                ; aouts  = Set.fromList $ filter (not . Set.null)
                                [ sync `Set.intersection` Set.fromList achouts | sync <- asyncsets ]
                }
            in  cfrows `Set.isSubsetOf` ains && ctows `Set.isSubsetOf` aouts

-- | Simulate for n-steps
sim :: Session -> StepType -> IO (Response ())
sim s (NumberOfSteps n) = runForVerdict s (Core.txsSimN n)
sim _ _                 = return $ Left "Can only simulate a number of steps"
