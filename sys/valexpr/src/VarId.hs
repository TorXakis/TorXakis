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
    { name    :: Maybe Name             -- smallid
    -- TODO: an empty name is generated by `CnectDef` at the parser. I don't know if this can be done otherwise.
    , unid    :: Id
    , varsort :: Sort
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Variable VarId where
      vname (VarId (Just v) i _) = nm
          where Right nm = mkName
                           $ toText v <> "$$" <> (T.pack . show) i
      vunid              = _id . VarId.unid
      vsort              = VarId.varsort
      cstrVariable n i   = VarId (Just nm) (Id i)
          where Right nm = mkName $ T.pack n

instance Resettable VarId
instance Identifiable VarId

instance SortOf VarId  where
    sortOf (VarId _nm _unid srt) = srt
