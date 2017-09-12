{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- ----------------------------------------------------------------------------------------- --

module VarId

where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name
import SortId
import Variable

data VarId          = VarId     { name       :: Name             --smallid
                                , unid       :: Int
                                , varsort    :: SortId
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

instance Variable VarId
  where
    vname v            = VarId.name v ++ "$$" ++ show (VarId.unid v)
    vunid              = VarId.unid
    vsort              = VarId.varsort
    cstrVariable       = VarId 
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
