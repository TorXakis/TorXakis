{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module FuncId

where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name
import SortId


data FuncId         = FuncId    { name       :: Name            -- smallid
                                , unid       :: Int
                                , funcargs   :: [SortId]
                                , funcsort   :: SortId
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

