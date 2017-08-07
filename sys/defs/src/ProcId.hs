{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ProcId

where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name
import ChanId
import SortId
import VarId

data  ExitSort      =  NoExit
                     | Exit [SortId]
                     | Hit
     deriving (Eq,Ord,Read,Show, Generic, NFData)

data ProcId         = ProcId    { name       :: Name
                                , unid       :: Int
                                , procchans  :: [ChanId]
                                , procvars   :: [VarId]
                                , procexit   :: ExitSort
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
