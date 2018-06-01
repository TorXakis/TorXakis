{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- |
module TorXakis.Lib.Tester where

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar   (readTVarIO)
import           Control.Exception             (try)
import           Control.Monad                 (void)
import           Control.Monad.State           (lift)
import           Control.Monad.STM             (atomically)
import qualified Data.Map.Strict               as Map
import           Lens.Micro                    ((&), (.~), (^.))
import           Name                          (Name)

import           TxsCore                       as Core

import           TorXakis.Lib.Common
import           TorXakis.Lib.CommonCore
import           TorXakis.Lib.Internal
import           TorXakis.Lib.Session

-- | Start the tester
setTest :: Session
        -> Name
        -> IO (Response ())
setTest s mn = runResponse $ do
    mDef <- lookupModel s mn
    lift $ do
        let fWCh = s ^. fromWorldChan
        tids <- (s ^. wConnDef . initWorld) fWCh
        st <- readTVarIO $ s ^. sessionState
        let prms = st ^. sessionParams
            Just (deltaString,_) = Map.lookup "param_Sut_deltaTime" prms
            deltaTime = read deltaString
        let s' = s & worldListeners .~ tids
        runIOC s' $
            Core.txsSetTest
                (putToW deltaTime fWCh (s' ^. wConnDef . toWorldMappings))
                (getFromW deltaTime fWCh)
                mDef Nothing Nothing

-- | Test for n-steps or actions
test :: Session -> StepType -> IO (Response ())
test s (NumberOfSteps n) = do
    void $ forkIO $ do
        verdict <- try $ runIOC s $ Core.txsTestN n
        atomically $ writeTQueue (s ^. verdicts) verdict
    return success
test s (AnAction a) = undefined s a
