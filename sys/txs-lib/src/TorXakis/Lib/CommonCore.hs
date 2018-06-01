{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric #-}
-- |
module TorXakis.Lib.CommonCore where

import           Control.Monad.Except  (ExceptT, throwError)
import           Control.Monad.State   (lift)
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Semigroup        ((<>))
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Lens.Micro            ((^.))

import           Name                  (Name)
import           TorXakis.Lens.TxsDefs (ix)
import qualified TxsCore               as Core
import           TxsDDefs              (Action)
import           TxsDefs               (ModelDef)

import           TorXakis.Lib.Common
import           TorXakis.Lib.Session

-- | How a step is described
data StepType = NumberOfSteps Int
              | AnAction Action
    --           | GoTo StateNumber
    --           | Reset -- ^ Go to the initial state.
    --           | Rewind Steps
              deriving (Show, Eq, Generic)

-- TODO: Types like 'StepType' are needed by the clients of 'txs-webserver'. So
-- to avoid introducing a dependency 'txs-lib' we could create a new package
-- called 'txs-lib-data', or something similar.
-- TODO: discuss with Jan: do we need a `Tree` step here?
instance ToJSON StepType
instance FromJSON StepType

lookupModel :: Session -> Name -> ExceptT Text IO ModelDef
lookupModel s mn = do
    tdefs <- lift $ runIOC s Core.txsGetTDefs
    maybe
        (throwError $ "No model named " <> mn)
        return (tdefs ^. ix mn)

