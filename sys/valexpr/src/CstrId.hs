{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module CstrId

where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           Id
import           Name
import           SortId

data CstrId         = CstrId    { name     :: Name            -- capid
                                , unid     :: Id
                                , cstrargs :: [SortId]
                                , cstrsort :: SortId
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable CstrId
instance Identifiable CstrId
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
