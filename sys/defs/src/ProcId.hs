{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module ProcId

where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           ChanId
import           Id
import           Name
import           SortId
import           VarId

data  ExitSort      =  NoExit
                     | Exit [SortId]
                     | Hit
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable ExitSort
instance Identifiable ExitSort

data ProcId         = ProcId    { name      :: Name
                                , unid      :: Id
                                , procchans :: [ChanId]
                                , procvars  :: [VarId]
                                , procexit  :: ExitSort
                                }
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable ProcId
instance Identifiable ProcId


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
