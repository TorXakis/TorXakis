{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module FuncId

where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           Id
import           Name
import           Sort

data FuncId = FuncId
    { name     :: Name            -- smallid
    , unid     :: Id
    , funcargs :: [Sort]
    , funcsort :: Sort
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable FuncId
instance Identifiable FuncId
