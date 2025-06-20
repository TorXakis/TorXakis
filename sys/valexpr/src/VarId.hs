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
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Id

-- Local imports.
import           Name
import           SortId
import           SortOf
import           Variable


data VarId = VarId
    { name    :: Name             --smallid
    , unid    :: Id
    , varsort :: SortId
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Variable VarId where
  vname v            = VarId.name v <> "$$" <> (T.pack . show) (VarId.unid v)
  vunid              = _id . VarId.unid
  vsort              = VarId.varsort
  cstrVariable n i   = VarId (T.pack n) (Id i)

instance Resettable VarId
instance Identifiable VarId

instance SortOf VarId  where
  sortOf (VarId _nm _unid srt)                    = srt