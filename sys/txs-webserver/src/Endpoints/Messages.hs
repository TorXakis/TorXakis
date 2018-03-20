{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Endpoints.Messages
(
  MessagesEP
, streamMessages
) where

import           Conduit                  (ZipSource (..), getZipSource,
                                           repeatC, runConduit, yield, (.|))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Monad            (void)
import           Data.Aeson               (ToJSON)
import           Data.Conduit.Combinators (map, mapM_)
import           Data.Conduit.TQueue      (sourceTQueue)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Lens.Micro               ((^.))
import           Prelude                  hiding (map, mapM_)
import           Servant                  ((:>), Capture, JSON, NewlineFraming,
                                           StreamGenerator,
                                           StreamGenerator (StreamGenerator),
                                           StreamGet)

import           ChanId                   (ChanId)
import           Common                   (SessionId, TxsHandler, getSession)
import           ConstDefs                (Const)
import           CstrId                   (CstrId)
import           EnvData                  (Msg)
import           Id                       (Id)
import           SortId                   (SortId)
import           TorXakis.Lib             (waitForVerdict)
import           TorXakis.Session         (Session, sessionMsgs)
import           TxsDDefs                 (Action)

type MessagesEP = "session"
               :> Capture "sid" SessionId
               :> "messages"
               :> StreamGet NewlineFraming JSON (StreamGenerator Msg)

streamMessages :: SessionId -> TxsHandler (StreamGenerator Msg)
streamMessages sid = do
    s <- getSession sid
    return $ StreamGenerator
           $ \sendFirst sendRest ->
                race_ (dataSource sendFirst sendRest s)
                      (void (waitForVerdict s) >> wait5)
      where
        dataSource f r s = runConduit $ getZipSource ((,) <$> isFirstSource <*> messagesSource s)
                        .| mapM_ (sendData f r)
        isFirstSource :: ZipSource IO Bool
        isFirstSource = ZipSource $ yield True >> repeatC False
        messagesSource :: Session -> ZipSource IO Msg
        messagesSource s = ZipSource $ sourceTQueue (s ^. sessionMsgs)
        --         .| mapM delayAndPass
        -- delayAndPass a =
        --     threadDelay (10^(4::Int)) >> return a
        sendData :: (a -> IO ()) -> (a -> IO ()) -> (Bool, a) -> IO ()
        sendData f _ (True, a)=
            f a
        sendData _ g (False, a)=
            g a
        -- messagesConduit :: Source IO Text
        -- messagesConduit = yieldMany [(1::Int) .. 10] .| map (T.pack . show)
        wait5 :: IO ()
        wait5 = threadDelay (10^(6::Int) * 5)

-- -- | Get the next N messages in the session.
-- --
-- -- If no messages are available this call will block waiting for new messages.
-- -- Normally one won't use such a function, but it is useful when playing with
-- -- the core 'TorXakis' function in an GHCi session.
-- --
-- getNextNMsgs :: Session -> Int -> IO [Msg]
-- getNextNMsgs s n =
--     runConduit $ sourceTQueue (s ^. sessionMsgs) .| take n .| sinkList
