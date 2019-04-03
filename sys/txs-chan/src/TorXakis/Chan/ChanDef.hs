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
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TorXakis.Chan.ChanDef
( ChanSort
, toSorts
, mkChanSort
, ChanDef (..)
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import           GHC.Generics        (Generic)

import TorXakis.Error
import TorXakis.Name
import TorXakis.Sort
import TorXakis.SortContext

-- | Data structure for Channel Sort: a list of 'TorXakis.Sort's.
newtype ChanSort = ChanSort { toSorts :: [Sort] }
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Smart constructor for ChanSort
mkChanSort :: SortContext c => c -> [Sort] -> Either Error ChanSort
mkChanSort ctx l | not $ null undefinedSorts = Left $ Error ("mkChanSort: Channel has undefined sorts " ++ show undefinedSorts)
                 | otherwise                 = Right $ ChanSort l
    where
        undefinedSorts :: [Sort]
        undefinedSorts = filter (not . flip memberSort ctx) l

instance Hashable ChanSort where
    hashWithSalt s (ChanSort xs)    = s `hashWithSalt` xs

-- | Data structure of Channel Definition.
data ChanDef = ChanDef { -- | Name
                         chanName :: Name
                         -- | ChanSort
                       , chanSort :: ChanSort
                       }
              | ChanExit{ -- | ChanSort
                          chanSort :: ChanSort
                        }
              | ChanHidden{ -- | ChanSort
                            chanSort :: ChanSort
                          }
              | ChanHit
              | ChanMiss
              | ChanQuiescence
                       deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ChanDef where
    getName = chanName
