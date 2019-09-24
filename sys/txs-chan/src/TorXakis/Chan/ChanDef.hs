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
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Chan.ChanDef
( ChanSort
, toSorts
, mkChanSort
, ChanDef (..)
, ChanRef (..)
, toMapByChanRef
, FreeChans (..)
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)

import TorXakis.Error
import TorXakis.Name
import TorXakis.Sort
import TorXakis.SortContext

-- | Data structure for Channel Sort: a list of 'TorXakis.Sort's.
newtype ChanSort = ChanSort { -- | to List of 'TorXakis.Sort's.
                              toSorts :: [Sort] }
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

instance UsedSorts c ChanSort where
    usedSorts _ = Set.fromList . toSorts

-- CHAN DEFs can not be used everywhere
-- kind of channel              user defined        exit        HChan           hit         miss        quiescence
-- Offer                            YES             YES         YES             YES         YES         YES
-- process instantiation            YES             NO          NO              NO          NO          NO
-- synchronization (parallel)       YES             NO          NO              NO          NO          NO
-- HIDE                             YES             NO          NO              NO          NO          NO

-- | Data structure of (User Defined) Channel Definition.
data ChanDef = ChanDef { -- | Name
                         chanName :: Name
                         -- | ChanSort
                       , chanSort :: ChanSort
                       }
               deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | References to channels.
data ChanRef =  -- | Reference to a user defined channel
                ChanRefUser { -- | Name
                              chanRef :: RefByName ChanDef
                            }
              | -- | Reference to the predefined EXIT channel
                ChanRefExit 
                       deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | toMapByChanRef
toMapByChanRef :: [ChanDef] -> Map.Map ChanRef ChanDef
toMapByChanRef = Map.fromList . map toTuple
    where
        toTuple :: ChanDef -> (ChanRef, ChanDef)
        toTuple cd = ( (ChanRefUser . RefByName . chanName) cd, cd)

instance HasName ChanDef where
    getName = chanName

instance UsedNames ChanDef where
    usedNames = Set.singleton . chanName

instance UsedSorts c ChanDef where
    usedSorts ctx = usedSorts ctx . chanSort

-- | Class for Free Channels
class FreeChans a where
    -- | Determine the free channels
    freeChans :: a -> Set.Set (RefByName ChanDef)
    -- | Is expression closed?
    -- A closed expression has no free channels.
    isClosed :: a -> Bool
    isClosed = Set.null . freeChans

instance FreeChans ChanRef where
    freeChans cr@ChanRefUser{} = (Set.singleton . chanRef) cr
    freeChans _                = Set.empty