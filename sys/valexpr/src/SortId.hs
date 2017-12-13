{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings #-}
module SortId where

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

-- Local imports.
import           Id
import           Name


data SortId = SortId
    { name :: Name            -- capid
    , unid :: Id
    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable SortId
instance Identifiable SortId

-- * standard sorts

sortId_Bool :: SortId
sortId_Bool = SortId "Bool" 101

sortId_Int :: SortId
sortId_Int = SortId "Int" 102

sortId_String :: SortId
sortId_String = SortId "String" 104

sortId_Regex :: SortId
sortId_Regex = SortId "Regex" 105