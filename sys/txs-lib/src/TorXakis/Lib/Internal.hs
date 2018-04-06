
{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | Module for internal functionality of TorXakis wrapper library.
module TorXakis.Lib.Internal where

import           Control.Arrow                ((|||))
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.Async     (race)
import           Control.Concurrent.STM.TChan (TChan, readTChan, tryReadTChan,
                                               writeTChan)
import           Control.DeepSeq              (force)
import           Control.Exception            (evaluate)
import           Control.Monad.State          (lift)
import           Control.Monad.STM            (atomically)
import           Data.Foldable                (traverse_)
import           Data.Map.Strict              as Map
import           Data.Set                     as Set
import           Lens.Micro                   ((^.))

import           ChanId
import           EnvCore                      (IOC)
import           TxsDDefs                     (Action (Act, ActQui))
import           TxsShow

import           TorXakis.Lib.Session

-- | Generic functionality for putting an 'Action' out to world AKA input to the SUT.
--
-- In case of an 'Action' coming from the world before given action can be put,
-- given 'Action' will be skipped and received 'Action' will be returned.
putToW :: TChan Action -> Map ChanId ToWorldMapping -> Action -> IOC Action
putToW fromWorldCh toWorldMMap act@(Act cs) = lift $
    do  (toWorldMapping, constants) <- (evaluate . force) getWorldMap
        toWFunc <- (evaluate . force) (toWorldMapping ^. sendToW)
        mAct <- atomically $ tryReadTChan fromWorldCh
        case mAct of
            Just sutAct -> return sutAct -- We got an action from the SUT, so we return that.
            Nothing  ->
                do  _ <- forkIO $ do
                        mAct' <- toWFunc constants
                        traverse_ (atomically . writeTChan fromWorldCh) mAct'
                    return act
      where
        getWorldMap =
            case Set.toList cs of
                [(cId, xs)] -> case Map.lookup cId toWorldMMap of
                                    Just twm -> (twm, xs)
                                    Nothing  -> error $ "No mapping to world for ChanId: " ++ fshow cId
                _           -> error $ "No (unique) action: " ++ fshow cs
                -- TODO: look how to do logging properly in Haskell
putToW _ _ ActQui = error "Is it OK to put quiescence into world?" -- TODO: handle this properly.

-- | Generic functionality for getting an action from world AKA output of the SUT.
--
-- If no 'Action' is received during given(TODO) timeout period,
-- a Quiescence 'Action' is returned.
getFromW :: Int -> TChan Action -> IOC Action
getFromW deltaTime fromWorldCh = (id ||| id) <$> lift (sutAct `race` quiAct)
    where
        sutAct = atomically $ readTChan fromWorldCh
        quiAct = threadDelay (deltaTime * (10 ^ (6 :: Int))) >> return ActQui
