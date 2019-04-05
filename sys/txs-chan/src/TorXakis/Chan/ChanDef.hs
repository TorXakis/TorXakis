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

-- CHAN DEFs can not be used everywhere -- TODO: do we need to split?
-- kind of channel              user defined        exit        HChan           hit         miss        quiescence
-- Offer                            YES             YES         YES             YES         YES         YES
-- process instantiation            YES             NO          NO              NO          NO          NO
-- synchronization (parallel)       YES             YES         NO              NO          NO          NO
-- HIDE                             YES             NO          NO              NO          NO          NO

-- | Data structure of Channel Definition.
data ChanDef = ChanDefUser { -- | Name
                             chanName :: Name
                             -- | ChanSort
                           , chanSort :: ChanSort
                           }
              | ChanDefExit{ -- | ChanSort
                             chanSort :: ChanSort
                           }
              | ChanDefHidden{ -- | ChanSort
                               chanSort :: ChanSort
                             }
              | ChanDefHit
              | ChanDefMiss
              | ChanDefQuiescence
                       deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | References to channels.
data ChanRef = ChanRefUser { -- | Name
                             chanRef :: RefByName ChanDef
                           }
              | ChanRefExit
              | ChanRefHidden
              | ChanRefHit
              | ChanRefMiss
              | ChanRefQuiescence
                       deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | toMapByChanRef
toMapByChanRef :: [ChanDef] -> Map.Map ChanRef ChanDef
toMapByChanRef = Map.fromList . map toTuple
    where
        toTuple :: ChanDef -> (ChanRef, ChanDef)
        toTuple cd = case cd of
                          ChanDefUser{}       -> ( (ChanRefUser . RefByName . chanName) cd, cd)
                          ChanDefExit{}       -> ( ChanRefExit                            , cd)
                          ChanDefHidden{}     -> ( ChanRefHidden                          , cd)
                          ChanDefHit{}        -> ( ChanRefHit                             , cd)
                          ChanDefMiss{}       -> ( ChanRefMiss                            , cd)
                          ChanDefQuiescence{} -> ( ChanRefQuiescence                      , cd)

instance HasName ChanDef where
    getName = chanName          -- TODO: undefined behaviour for EXIT channel etc.

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