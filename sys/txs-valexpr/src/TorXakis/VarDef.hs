{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Variable Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TorXakis.VarDef
( VarDef
, MinimalVarDef (..)
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import           GHC.Generics        (Generic)

import TorXakis.Name
import TorXakis.Sort

-- | Class for a variable definition.
class (Eq v, Ord v, Read v, Show v
      , HasName v, HasSort v, Hashable v
      ) => VarDef v

-- | Minimal implementation of Variable Definition.
data MinimalVarDef = MinimalVarDef { -- | name
                                     varName :: Name
                                     -- | Sort
                                   , varSort :: Sort
                                   }
                                   deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Hashable MinimalVarDef where
    hashWithSalt s (MinimalVarDef n srt) = s `hashWithSalt` n
                                             `hashWithSalt` srt

instance HasName MinimalVarDef where
    getName = varName
instance HasSort MinimalVarDef where
    getSort = varSort
instance VarDef MinimalVarDef