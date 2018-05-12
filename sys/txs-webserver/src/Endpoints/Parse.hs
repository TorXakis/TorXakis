-- | End-points for the parsing of TorXakis entities.
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Endpoints.Parse
    ( ParseActionEP
    , parseAction
    )
where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    as Text
import           GHC.Generics (Generic)
import           Servant

import           TxsDDefs     (Action)

import           Common       (Env, SessionId, liftLib)
import qualified TorXakis.Lib as Lib

-- | Textual representation of an action.
newtype ActionText = ActionText Text
    deriving (Show, Generic)

instance ToJSON ActionText
instance FromJSON ActionText

type ParseActionEP = "sessions"
                  :> Capture "sid" SessionId
                  :> "parse-action"
                  :> ReqBody '[JSON] ActionText
                  :> Get '[JSON] Action

parseAction :: Env -> SessionId -> ActionText -> Handler Action
parseAction env sId (ActionText t) = liftLib env sId (`Lib.parseAction` t)
