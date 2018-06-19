{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module TorXakis.Lib.Tester where

import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar   (readTVarIO, writeTVar)
import           Control.Monad.Except          (throwError)
import           Control.Monad.State           (lift, liftIO)
import           Control.Monad.STM             (atomically)
import qualified Data.Map.Strict               as Map
import           Data.Semigroup                ((<>))
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Lens.Micro                    ((^.))
import           Name                          (Name)

import           EnvData                       (Msg (TXS_CORE_USER_INFO))
import           TorXakis.Lens.TxsDefs         (ix)
import qualified TxsCore                       as Core
import qualified TxsDefs

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Session
import           TorXakis.Lib.SocketWorld
import           TorXakis.Lib.World

-- | Start the tester
setTest :: Name
        -> Name
        -> Name
        -> Session
        -> IO (Response ())
setTest mdlNm cnctNm purpMappNms s = runResponse $ do
    tDefs <- liftIO $ runReadOnlyIOC s Core.txsGetTDefs
    mDef <- maybe
                (throwError $ "No model named " <> mdlNm)
                return (tDefs ^. ix mdlNm)
    cDef <- maybe
                (throwError $ "No CnectDef named " <> cnctNm)
                return (tDefs ^. ix cnctNm)
    let purpDefsMap = TxsDefs.purpDefs   tDefs
        mappDefsMap = TxsDefs.mapperDefs tDefs
        purpMapps = T.words purpMappNms
        adefs = [ adef
                | (TxsDefs.MapperId nm _, adef) <- Map.toList mappDefsMap
                , nm `elem` purpMapps
                ]
        pdefs =  [ pdef
                 | (TxsDefs.PurpId  nm _, pdef) <- Map.toList purpDefsMap
                 , nm `elem` purpMapps
                 ]
    mADef <- case adefs of
                 [a] -> return $ Just a
                 []  -> return Nothing
                 _   -> throwError "Wrong or inconsistent parameters"
    mPDef <- case pdefs of
                 [p] -> return $ Just p
                 []  -> return Nothing
                 _   -> throwError "Wrong or inconsistent parameters"
    if isConsistent mDef mADef mPDef cDef
        then lift $ do
            st <- readTVarIO (s ^. sessionState)
            let fWCh = s ^. fromWorldChan
                prms = st ^. sessionParams
                Just (deltaString,_) = Map.lookup "param_Sut_deltaTime" prms
                deltaTime = read deltaString
            atomically $ writeTQueue (s ^. sessionMsgs)
                       $ TXS_CORE_USER_INFO "Tester connecting to SUT..."
            wcd <- initSocketWorld s fWCh cDef
            atomically $ writeTVar (s ^. wConnDef) wcd
            runIOC s $
                Core.txsSetTest
                    (lift <$> putToW deltaTime fWCh (wcd ^. toWorldMappings))
                    (lift $ getFromW deltaTime fWCh)
                    mDef mADef mPDef
        else throwError "Wrong or inconsistent parameters"
      where
        isConsistent :: TxsDefs.ModelDef
                     -> Maybe TxsDefs.MapperDef
                     -> Maybe TxsDefs.PurpDef
                     -> TxsDefs.CnectDef
                     -> Bool
        isConsistent (TxsDefs.ModelDef minsyncs moutsyncs _ _)
                     Nothing _
                     (TxsDefs.CnectDef _ conndefs) =
            let { mins   = Set.fromList minsyncs
                ; mouts  = Set.fromList moutsyncs
                ; ctows  = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDtoW  chn _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList
                                [ Set.singleton chn | TxsDefs.ConnDfroW chn _ _ _ _ <- conndefs ]
                }
            in  mins == ctows && cfrows == mouts
        isConsistent _
                     (Just (TxsDefs.MapperDef achins achouts asyncsets _)) _
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

-- | Test for n-steps or an action
test :: Session -> StepType -> IO (Response ())
test s (NumberOfSteps n) = runForVerdict s (Core.txsTestN n)
test s (AnAction a)      = runForVerdict s (Core.txsTestIn a)

-- | Test step by observing output
testO :: Session -> IO (Response ())
testO s = runForVerdict s Core.txsTestOut
