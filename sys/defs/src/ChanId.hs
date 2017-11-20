{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- ----------------------------------------------------------------------------------------- --

module ChanId

where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           Name
import           SortId

data ChanId         = ChanId    { name      :: Name
                                , unid      :: Int
                                , chansorts :: [SortId]
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
