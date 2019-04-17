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
, hide
)

-- ----------------------------------------------------------------------------------------- --
-- import

where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           Data.Tuple
import           GHC.Generics        (Generic)

import           TorXakis.Chan
import           TorXakis.ChanContext
import           TorXakis.Error
import           TorXakis.Name
-- TODO add lookup, Elems & member functions

-- | Relabel Map for 'TorXakis.ChanDef's
newtype RelabelMap = RelabelMap { -- | toMap conversion
                                  toMap :: Map.Map Name Name
                                } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | make Relabel Map: must be injective but is not required to be surjective.
--   Furthermore, the domain and range might be the same.
--   See https://en.wikipedia.org/wiki/Bijection,_injection_and_surjection
mkRelabelMap :: Map.Map Name Name -> Either Error RelabelMap
mkRelabelMap m | nrOfElems /= nrOfKeys    = Left $ Error ("Not injective: number of elements (" ++ show nrOfElems ++ ") differs from number of keys (" ++ show nrOfKeys ++ ")")
               | otherwise                = Right $ RelabelMap m
    where
       nrOfKeys :: Int
       nrOfKeys = Map.size m

       nrOfElems :: Int
       nrOfElems = Set.size (Set.fromList (Map.elems m))

hide :: Set.Set Name -> RelabelMap -> RelabelMap
hide s (RelabelMap m) = let l  = Set.toList s
                            m' = deleteAll l m
                            r  = reverseMap m'
                            evasiveMoves = (Map.fromList . catMaybes) (map (evasiveMove r) l)
                        in
                            RelabelMap (Map.union m' evasiveMoves)
    where
        deleteAll :: Ord k => [k] ->  Map.Map k a ->  Map.Map k a
        deleteAll xs p = foldl (flip Map.delete) p xs
        
        reverseMap :: Ord a => Map.Map k a -> Map.Map a k
        reverseMap = Map.fromList . map swap . Map.toList

        -- | The relabel (if any) necessary to prevent name clashes after relabel within hide
        evasiveMove :: Ord v => Map.Map v v -> v -> Maybe (v,v)
        evasiveMove r n = case Map.lookup n r of
                               Nothing -> Nothing
                               Just v  -> Just (n , root r v)
        
        -- | Find root of element in map.
        -- The map is treated as a transitive graph
        root :: Ord v => Map.Map v v -> v -> v
        root r v = case Map.lookup v r of
                        Nothing -> v
                        Just w  -> root r w
        
class Relabel e where
    relabel :: RelabelMap -> e -> e
    relabel m | Map.null (toMap m) = id
              | otherwise          = relabel' m

    relabel' :: RelabelMap -> e -> e

instance Relabel Name where
    relabel' m c = Map.findWithDefault c c (toMap m)

instance Relabel ChanRef where
    relabel' m cr@ChanRefUser{} = case Map.lookup ((toName . chanRef) cr) (toMap m) of
                                       Nothing -> cr
                                       Just n  -> ChanRefUser (RefByName n)
    relabel' _ x                = x