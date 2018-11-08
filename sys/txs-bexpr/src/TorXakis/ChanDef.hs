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
module TorXakis.ChanDef
( ChanDef (..)
, ChanSort (..)
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           GHC.Generics        (Generic)

import TorXakis.Name
import TorXakis.Sort

newtype ChanSort = ChanSort [Sort]
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Hashable ChanSort where
    hashWithSalt s (ChanSort xs)    = s `hashWithSalt` xs

-- | Data structure of Channel Definition.
data ChanDef = ChanDef { -- | Name
                         chanName :: Name
                         -- | ChanSort
                       , chanSort :: ChanSort
                       }
                       deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ChanDef where
    getName = chanName
