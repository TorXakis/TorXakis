{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RefMap
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the RefMap.
-- A Map created by using the reference to the items.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
module TorXakis.RefMap
( RefMap (..)
, empty
, member
, TorXakis.RefMap.lookup
, elems
, insert
, union
, repeatedByRefIncremental
, repeatedByRef
)
where
import           Data.Hashable
import qualified Data.HashMap         as HashMap
import           Data.List.Unique     (repeated)

import           TorXakis.Referable

-- | Map of Referable objects.
data RefMap a where
    RefMap :: (Referable a, Ord (Ref a), Hashable (Ref a)) =>
                [a] -> RefMap a

-- | Convert list of referable objects into HashMap
toHashMap :: (Referable a, Ord (Ref a), Hashable (Ref a)) => [a] -> HashMap.Map (Ref a) a
toHashMap = HashMap.fromList . map (\e -> (toRef e,e))

-- | The empty map.
empty :: (Referable a, Ord (Ref a), Hashable (Ref a)) => RefMap a
empty = RefMap ([]::[a])

-- | Is the key a member of the map?
member :: Ref a -> RefMap a -> Bool
member r (RefMap xs) = HashMap.member r (toHashMap xs)

-- | Lookup the value at a key in the map.
lookup :: Ref a -> RefMap a -> Maybe a
lookup r (RefMap xs) = HashMap.lookup r (toHashMap xs)

-- | Return all elements of the map in arbitrary order of their keys.
elems :: RefMap a -> [a]
elems (RefMap xs) = xs

-- | Insert a new value in the map using its reference as key. 
-- If the key is already present in the map, the associated value is replaced with the supplied value.
insert :: a -> RefMap a -> RefMap a
insert v (RefMap m) = RefMap (HashMap.elems (HashMap.insert (toRef v) v (toHashMap m)))

-- | The (left-biased) union of two maps. It prefers the first map when duplicate keys are encountered.
union :: RefMap a -> RefMap a -> RefMap a
union (RefMap a) (RefMap b) = RefMap (HashMap.elems (HashMap.union (toHashMap a) (toHashMap b)))

-- | Return the elements with non-unique references that the second list contains in the combination of the first and second list.
repeatedByRefIncremental :: (Referable a, Ord (Ref a)) => [a] -> [a] -> [a]
repeatedByRefIncremental xs ys = filter ((`elem` nuRefs) . toRef) ys
    where nuRefs = repeated $ map toRef xs ++ map toRef ys

-- | Return the elements with non-unique references: 
-- the elements with a reference that is present more than once in the list.
repeatedByRef :: (Referable a, Ord (Ref a)) => [a] -> [a]
repeatedByRef = repeatedByRefIncremental []