{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- |
module ChanId

where

import           Control.DeepSeq
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Data
import           GHC.Generics    (Generic)

import           Id
import           Name
import           SortId

data ChanId = ChanId
    { name      :: Name
    , unid      :: Id
    , chansorts :: [SortId]
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance ToJSON ChanId
instance FromJSON ChanId

instance Resettable ChanId
instance Identifiable ChanId
