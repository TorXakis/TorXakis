{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Relabel
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Relabel Channels in ProcDef.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.Relabel
( Relabel (..)
, RelabelMap
, toMap
, mkRelabelMap
)

-- ----------------------------------------------------------------------------------------- --
-- import

where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.ChanDef
import           TorXakis.Error

-- | Relabel Map for 'TorXakis.ChanDef's
newtype RelabelMap = RelabelMap {
                                  toMap :: Map.Map ChanDef ChanDef
                                } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | make Relabel Map: must be bijection and each mapping must maintain sort.
mkRelabelMap :: Map.Map ChanDef ChanDef -> Either MinError RelabelMap
mkRelabelMap m | elems /= keys         = Left $ MinError (T.pack ("Not a bijection: elements (" ++ show elems ++ ") differ from keys (" ++ show keys ++ ")"))
               | null mismatches       = Right $ RelabelMap m
               | otherwise             = Left $ MinError (T.pack ("Mismatches in sort of channels : " ++ show mismatches))
    where
       keys :: Set.Set ChanDef
       keys = Set.fromList (Map.keys m)

       elems :: Set.Set ChanDef
       elems = Set.fromList (Map.elems m)

       mismatches :: [(ChanDef,ChanDef)]
       mismatches = filter match (Map.toList m)

       match :: (ChanDef, ChanDef) -> Bool
       match (c1,c2) = chanSort c1 == chanSort c2

class Relabel e
  where
    relabel :: RelabelMap -> e -> e
    relabel m | Map.null (toMap m) = id
              | otherwise          = relabel' m

    relabel' :: RelabelMap -> e -> e

instance Relabel ChanDef
  where
    relabel' m c = Map.findWithDefault c c (toMap m)
