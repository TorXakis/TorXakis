{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module PurpId

where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name

data PurpId         = PurpId    { name       :: Name            -- capid
                                , unid       :: Int
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

