{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module CnectId

where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name

data CnectId        = CnectId   { name       :: Name            -- capid
                                , unid       :: Int
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
