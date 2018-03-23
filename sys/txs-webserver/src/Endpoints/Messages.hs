{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoints.Messages
(
  MessagesEP
, streamMessages
, SSMessagesEP
, ssMessagesEP
) where

import           Conduit                   (ZipSource (..), getZipSource,
                                            repeatC, runConduit, yield, (.|))
-- import           Control.Concurrent        (threadDelay)
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
import           Servant                   ((:>), Application, Capture, Handler,
                                            JSON, NewlineFraming, Raw,
                                            StreamGenerator,
                                            StreamGenerator (StreamGenerator),
                                            StreamGet, Tagged (Tagged))


import           Common                    (Env, SessionId, getSession,
                                            getSessionIO)
import           EnvData                   (Msg)
import           TorXakis.Lib              (waitForMessageQueue, waitForVerdict)
import           TorXakis.Lib.Session      (Session, sessionMsgs)

type MessagesEP = "session"
               :> Capture "sid" SessionId
               :> "messages"
               :> StreamGet NewlineFraming JSON (StreamGenerator Msg)

streamMessages :: Env -> SessionId -> Handler (StreamGenerator Msg)
streamMessages env sid = do
    s <- getSession env sid
    return $ StreamGenerator
           $ \sendFirst sendRest ->
                race_ (dataSource sendFirst sendRest s)
                      (waitForVerdict s >> waitForMessageQueue s)
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
        -- wait5 :: IO ()
        -- wait5 = threadDelay (10^(6::Int) * 5)

type SSMessagesEP = "session" :> "sse"
               :> Capture "sid" SessionId
               :> "messages"
               :> Raw

-- | Server sent messages.
ssMessagesEP :: Env -> SessionId -> Tagged Handler Application
ssMessagesEP env sid = Tagged $ \req respond -> do
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
    --   eName :: Builder
    --   eName = fromByteString "TorXakis Message"
      msg'  :: Builder
      msg'  = fromLazyByteString $ encode msg
