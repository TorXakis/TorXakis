module Common
(
  SessionId
, TxsHandler
, Env (..)
)
where

import           Control.Concurrent.STM.TVar (TVar)
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Data.IntMap.Strict          as Map
import           Servant.Server

import           TorXakis.Session (Session)

type SessionId = Int

type TxsHandler = ReaderT Env Handler
newtype Env = Env {sessions :: TVar (Map.IntMap Session)}
