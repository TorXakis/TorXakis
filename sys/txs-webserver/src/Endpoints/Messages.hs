{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoints.Messages
(
  MessagesEP
, messages
) where

import           Conduit                   (runConduit, (.|))
import           Control.Concurrent.Async  (async, race_)
import           Control.Concurrent.Chan   (Chan, newChan, writeChan)
import           Data.Aeson                (encode)
import           Data.Binary.Builder       (Builder, fromLazyByteString)
import           Data.Conduit.Combinators  (mapM_)
import           Data.Conduit.TQueue       (sourceTQueue)
import           Data.Monoid               ((<>))
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Encoding   as TLE
import           Lens.Micro                ((^.))
import           Network.HTTP.Types.Status (status404)
import           Network.Wai               (responseLBS)
import           Network.Wai.EventSource   (ServerEvent (CloseEvent, ServerEvent),
                                            eventData, eventId, eventName,
                                            eventSourceAppChan)
import           Prelude                   hiding (mapM_)
import           Servant


import           Common                    (Env, SessionId, getSessionIO)
import           EnvData                   (Msg)
import           TorXakis.Lib              (waitForMessageQueue, waitForVerdict)
import           TorXakis.Lib.Session      (sessionMsgs)

type MessagesEP = "sessions"
                 :> Capture "sid" SessionId
                 :> "messages"
                 :> Raw

-- | Server sent messages.
messages :: Env -> SessionId -> Tagged Handler Application
messages env sid = Tagged $ \req respond -> do
    mS <- getSessionIO env sid
    case mS of
        Nothing -> do
            let msg = "Could not find session with id: "
                      <> TLE.encodeUtf8 (TL.pack (show sid))
            respond $ responseLBS status404 [] msg
        Just s  -> do
            ch <- newChan
            _ <- async $ race_ (runConduit $ sourceTQueue (s ^. sessionMsgs) .| mapM_ (sendTo ch))
                               (waitForVerdict s >> waitForMessageQueue s >> writeChan ch CloseEvent)
            eventSourceAppChan ch req respond

sendTo :: Chan ServerEvent -> Msg -> IO ()
sendTo ch msg =
    writeChan ch (asServerEvent msg)

asServerEvent :: Msg -> ServerEvent
asServerEvent msg = ServerEvent
    { eventName = Nothing
    , eventId = Nothing
    , eventData = [msg']
    }
    where
      msg'  :: Builder
      msg'  = fromLazyByteString $ encode msg
