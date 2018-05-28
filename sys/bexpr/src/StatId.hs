{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module StatId

where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           Id
import           Name
import           ProcId

data StatId = StatId
    { name   :: Name
    , unid   :: Id
    , procid :: ProcId
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable StatId
instance Identifiable StatId
