{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Endpoints.Messages
(
  MessagesEP
, streamMessages
, SSMessagesEP
, ssMessagesEP
) where

import           Conduit                  (ZipSource (..), getZipSource,
                                           repeatC, runConduit, yield, (.|))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (race_, async)
import           Control.Monad            (void)
import           Data.Aeson               (ToJSON)
import           Data.Conduit.Combinators (map, mapM_)
import           Data.Conduit.TQueue      (sourceTQueue)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Lens.Micro               ((^.))
import           Prelude                  hiding (map, mapM_)
import           Servant                  ((:>), Capture, JSON, NewlineFraming
                                          , StreamGenerator
                                          , StreamGenerator (StreamGenerator)
                                          , StreamGet
                                          , Raw, Tagged (Tagged), Application
                                          , Handler)
import           Network.Wai.EventSource (eventSourceAppChan, ServerEvent
                                         , ServerEvent (ServerEvent)
                                         , eventName
                                         , eventId
                                         , eventData
                                         )
import           Data.Binary.Builder (Builder, fromByteString)
import qualified Data.Text.Lazy as TL
import           Data.Text.Encoding as TE
import           Data.Text.Lazy.Encoding as TLE
import           Control.Concurrent.Chan (Chan, newChan, writeChan)
import           Control.Monad.Trans.Class (lift)
import           Data.Monoid ((<>))
import           Network.Wai (responseLBS)
import           Network.HTTP.Types.Status (status404)


import           ChanId                   (ChanId)
import           Common                   (SessionId, getSessionIO, getSession, Env)
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

streamMessages :: Env -> SessionId -> Handler (StreamGenerator Msg)
streamMessages env sid = do
    s <- getSession env sid
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
            _ <- async $ runConduit $
                 sourceTQueue (s ^. sessionMsgs) .| mapM_ (sendTo ch)
            eventSourceAppChan ch req respond

sendTo :: Chan ServerEvent -> Msg -> IO ()
sendTo ch msg =
    writeChan ch (asServerEvent msg)

asServerEvent :: Msg -> ServerEvent
asServerEvent msg = ServerEvent
    { eventName = Just eName
    , eventId = Nothing
    , eventData = [msg']
    }
    where
      eName :: Builder
      eName = fromByteString "TorXakis Message"
      msg'  :: Builder
      -- TODO: encode msg as a JSON bytestring.
      msg'  = fromByteString $ TE.encodeUtf8 $ T.pack $ show msg


-- -- | Get the next N messages in the session.
-- --
-- -- If no messages are available this call will block waiting for new messages.
-- -- Normally one won't use such a function, but it is useful when playing with
-- -- the core 'TorXakis' function in an GHCi session.
-- --
-- getNextNMsgs :: Session -> Int -> IO [Msg]
-- getNextNMsgs s n =
--     runConduit $ sourceTQueue (s ^. sessionMsgs) .| take n .| sinkList
