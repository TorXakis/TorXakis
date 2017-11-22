{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- ----------------------------------------------------------------------------------------- --

module VarId

where

import           Control.DeepSeq
import           Data.Data
import           Data.Monoid
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Name
import           SortId
import           Variable

data VarId          = VarId     { name    :: Name             --smallid
                                , unid    :: Int
                                , varsort :: SortId
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Variable VarId where
  vname v            = VarId.name v <> "$$" <> (T.pack . show) (VarId.unid v)
  vunid              = VarId.unid
  vsort              = VarId.varsort
  cstrVariable       = VarId . T.pack
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
