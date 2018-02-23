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

-- Local imports.
import           Id
import           Name
import           Sort
import           SortOf
import           Variable


data VarId = VarId
    { name    :: Name             -- smallid
    , unid    :: Id
    , varsort :: Sort
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Variable VarId where
      vname v            = nm
          where Right nm = Name.name
                           $ toText (VarId.name v) <> "$$" <> (T.pack . show) (VarId.unid v)
      vunid              = _id . VarId.unid
      vsort              = VarId.varsort
      cstrVariable n i   = VarId nm (Id i)
          where Right nm = Name.name $ T.pack n

instance Resettable VarId
instance Identifiable VarId

instance SortOf VarId  where
    sortOf (VarId _nm _unid srt) = srt
