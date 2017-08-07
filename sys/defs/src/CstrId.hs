{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module CstrId

where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name
import SortId

data CstrId         = CstrId    { name       :: Name            -- capid
                                , unid       :: Int
                                , cstrargs   :: [SortId]
                                , cstrsort   :: SortId
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
