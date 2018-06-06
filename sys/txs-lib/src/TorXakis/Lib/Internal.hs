
{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | Module for internal functionality of TorXakis wrapper library.
module TorXakis.Lib.Internal
( putToW
, getFromW
)
where

import           Control.Arrow                ((|||))
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.Async     (race)
import           Control.Concurrent.STM.TChan (TChan, readTChan, tryReadTChan,
                                               writeTChan)
import           Control.DeepSeq              (force)
import           Control.Monad.State          (liftIO)
import           Control.Monad.STM            (atomically)
import           Data.Foldable                (traverse_)
import           Data.Map.Strict              as Map
import           Data.Set                     as Set
import qualified Data.Text                    as T
import           Lens.Micro                   ((^.))

import           ChanId
import           TxsDDefs                     (Action (Act, ActQui))
import           TxsShow

import           TorXakis.Lib.Session

-- | Generic functionality for putting an 'Action' out to world AKA input to the SUT.
--
-- In case of an 'Action' coming from the world before given action can be put,
-- given 'Action' will be skipped and received 'Action' will be returned.
putToW :: Int -> TChan Action -> Map ChanId ToWorldMapping -> Action -> IO Action
putToW _ fromWorldCh toWorldMMap act@(Act cs) = do
    let (toWorldMapping, constants) = getWorldMap
        toWFunc = force (toWorldMapping ^. sendToW)
    actIfNothingRead fromWorldCh $
        do  _ <- forkIO $ do
                rAct <- liftIO $ toWFunc constants
                case rAct of
                    Right mAct -> traverse_ (atomically . writeTChan fromWorldCh) mAct
                    Left  err  -> error $ T.unpack err
            return act
  where
    getWorldMap =
        case Set.toList cs of
            [(cId, cnsts)] -> case Map.lookup cId toWorldMMap of
                                Just twm -> (twm, cnsts)
                                Nothing  -> error $ "No mapping to world for ChanId: " ++ fshow cId
            _           -> error $ "No (unique) action: " ++ fshow cs
putToW deltaTime fromWorldCh _ ActQui =
    actIfNothingRead fromWorldCh $ do
        threadDelay (deltaTime * milliSecond)
        return ActQui

actIfNothingRead :: TChan Action -> IO Action -> IO Action
actIfNothingRead fromWorldCh ioAct = do
    mAct <- atomically $ tryReadTChan fromWorldCh
    case mAct of
        Just sutAct -> return sutAct
        Nothing     -> ioAct

-- | Generic functionality for getting an action from world AKA output of the SUT.
--
-- If no 'Action' is received during given timeout period,
-- a Quiescence 'Action' is returned.
getFromW :: Int -> TChan Action -> IO Action
getFromW deltaTime fromWorldCh = (id ||| id) <$> (sutAct `race` quiAct)
    where
        sutAct = atomically $ readTChan fromWorldCh
        quiAct = threadDelay (deltaTime * milliSecond) >> return ActQui

milliSecond :: Int
milliSecond = 10 ^ (3 :: Int)
