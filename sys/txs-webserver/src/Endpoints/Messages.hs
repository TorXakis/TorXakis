{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoints.Messages
    ( MessagesEP
    , messages
    , CloseMessagesEP
    , closeMessages
    , OpenMessagesEP
    , openMessages
    )
where

import           Conduit                     (runConduit, (.|))
import           Control.Concurrent.Async    (async, race_)
import           Control.Concurrent.Chan     (Chan, newChan, writeChan)
import           Control.Concurrent.STM.TVar (readTVar, writeTVar)
import           Control.Monad               (when)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically, retry)
import           Data.Aeson                  (encode)
import           Data.Binary.Builder         (Builder, fromLazyByteString)
import           Data.Conduit.Combinators    (mapM_)
import           Data.Conduit.TQueue         (sourceTQueue)
import           Data.Monoid                 ((<>))
import qualified Data.Text.Lazy              as TL
import           Data.Text.Lazy.Encoding     as TLE
import           Lens.Micro                  ((^.))
import           Network.HTTP.Types.Status   (status404)
import           Network.Wai                 (responseLBS)
import           Network.Wai.EventSource     (ServerEvent (CloseEvent, ServerEvent),
                                              eventData, eventId, eventName,
                                              eventSourceAppChan)
import           Prelude                     hiding (mapM_)
import           Servant


import           Common                      (Env,
                                              ServerSession (ServerSession),
                                              SessionId, getServerSession,
                                              getServerSessionIO,
                                              _contListening, _libSession)
import           EnvData                     (Msg)
import           TorXakis.Lib                (waitForMessageQueue,
                                              waitForVerdict,
                                              writeClosingVerdict)
import           TorXakis.Lib.Session        (sessionMsgs)

type MessagesEP = "sessions"
                 :> Capture "sid" SessionId
                 :> "messages"
                 :> Raw

-- | Server sent messages. Note that a GET request to this end-point won't be
-- completed until another call is made to the session's 'messages/close'
-- endpoint to signal that we are no longer interested in reviving messages.
--
messages :: Env -> SessionId -> Tagged Handler Application
messages env sId = Tagged $ \req respond -> do
    mSvrS <- getServerSessionIO env sId
    case mSvrS of
        Nothing -> do
            let msg = "Could not find session with id: "
                      <> TLE.encodeUtf8 (TL.pack (show sId))
            respond $ responseLBS status404 [] msg
        Just svrS -> do
            let s = _libSession svrS
            -- TODO: set _contListening to True!
            ch <- newChan
            _ <- async $ race_ (runConduit $ sourceTQueue (s ^. sessionMsgs) .| mapM_ (sendTo ch))
                               (waitForClose svrS >> waitTillDone s ch)
            eventSourceAppChan ch req respond
    where
      waitForClose svrS = atomically $ do
          b <- readTVar (_contListening svrS)
          when b retry

      waitTillDone s ch =
          waitForVerdict s >>
          waitForMessageQueue s >>
          writeChan ch CloseEvent

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

type OpenMessagesEP = "sessions"
                       :> Capture "sid" SessionId
                       :> "messages"
                       :> "open"
                       :> PostNoContent '[JSON] ()

-- | Open the messages server sent events endpoint.
openMessages :: Env -> SessionId -> Handler ()
openMessages env sId = do
    svrS <- getServerSession env sId
    liftIO $ atomically $ writeTVar (_contListening svrS) True
    return ()

type CloseMessagesEP = "sessions"
                       :> Capture "sid" SessionId
                       :> "messages"
                       :> "close"
                       :> PostNoContent '[JSON] ()

-- | Close the messages server sent events endpoint.
closeMessages :: Env -> SessionId -> Handler ()
closeMessages env sId = do
    ServerSession s contLstnTVar <- getServerSession env sId
    liftIO $ do
        atomically $ writeTVar contLstnTVar False
        writeClosingVerdict s
    return ()
