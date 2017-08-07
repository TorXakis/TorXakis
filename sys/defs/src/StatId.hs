{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module StatId

where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name
import ProcId

data StatId         = StatId    { name       :: Name
                                , unid       :: Int
                                , procid     :: ProcId
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
