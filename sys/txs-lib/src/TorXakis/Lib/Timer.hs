{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
-- |
module TorXakis.Lib.Timer where


import           Control.Concurrent.STM.TVar (modifyTVar', readTVarIO)
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  (ToJSON)
import qualified Data.Map.Strict             as Map
import           Data.Time                   (diffUTCTime, getCurrentTime,
                                              getCurrentTimeZone,
                                              utcToLocalTime)
import           GHC.Generics                (Generic)
import           Lens.Micro                  ((^.))

import           TorXakis.Lib.Session

newtype TimeResult = TimeResult { currentTime :: String }
    deriving (Generic)

instance ToJSON TimeResult

time :: IO TimeResult
time = do
    tz  <- getCurrentTimeZone
    now <- getCurrentTime
    return $ TimeResult $ show $ utcToLocalTime tz now

data Timer = Timer { timerName :: String
                   , startTime :: String
                   , stopTime  :: String
                   , duration  :: String
                   }
    deriving (Generic)

instance ToJSON Timer

timer :: Session -> String -> IO Timer
timer s nm = do
    tz  <- getCurrentTimeZone
    now <- getCurrentTime
    let timersT = s ^. timers
    timersMap <- readTVarIO timersT
    case Map.lookup nm timersMap of
        Nothing -> do
                    atomically $ modifyTVar' timersT $ Map.insert nm now
                    return $ Timer nm (show $ utcToLocalTime tz now) "" ""
        Just t  -> do
                    atomically $ modifyTVar' timersT $ Map.delete nm
                    return $ Timer nm
                                   (show $ utcToLocalTime tz t)
                                   (show $ utcToLocalTime tz now)
                                   (show $ diffUTCTime now t)
