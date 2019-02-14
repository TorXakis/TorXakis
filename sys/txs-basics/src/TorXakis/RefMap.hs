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
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module TorXakis.RefMap
( RefMap
, empty
, member
, TorXakis.RefMap.lookup
, elems
, insert
, union
, toRefMap
, repeatedByRefIncremental
, repeatedByRef
)
where
import           Data.Data            (Data)
import qualified Data.HashMap         as HashMap
import           Data.List.Unique     (repeated)

import           TorXakis.Referable

-- | Map of Referable objects.
data Referable a => -- hlint says make it newtype, I agree yet: https://ghc.haskell.org/trac/ghc/ticket/16319
            RefMap a = RefMap { -- | the HashMap
                                toHashMap :: HashMap.Map (Ref a) a
                              } deriving (Eq, Ord, Show, Read, Data)

-- | The empty map.
empty :: Referable a => RefMap a
empty = RefMap HashMap.empty

-- | Is the key a member of the map?
member :: Referable a => Ref a -> RefMap a -> Bool
member r = HashMap.member r . toHashMap

-- | Lookup the value at a key in the map.
lookup :: Referable a => Ref a -> RefMap a -> Maybe a
lookup r = HashMap.lookup r . toHashMap

-- | Return all elements of the map in arbitrary order of their keys.
elems :: Referable a => RefMap a -> [a]
elems = HashMap.elems . toHashMap

-- | Insert a new value in the map using its reference as key. 
-- If the key is already present in the map, the associated value is replaced with the supplied value.
insert :: Referable a => a -> RefMap a -> RefMap a
insert v m = RefMap (HashMap.insert (toRef v) v (toHashMap m))

-- | The (left-biased) union of two maps. It prefers the first map when duplicate keys are encountered.
union :: Referable a => RefMap a -> RefMap a -> RefMap a
union a b = RefMap (HashMap.union (toHashMap a) (toHashMap b))

-- | Return 'Data.HashMap.Map' where the reference of the element is taken as key
--   and the element itself is taken as value.
toRefMap :: Referable a => [a] -> RefMap a
toRefMap = RefMap . HashMap.fromList . map (\e -> (toRef e, e))

-- | Return the elements with non-unique references that the second list contains in the combination of the first and second list.
repeatedByRefIncremental :: (Referable a, Referable b, Ref a ~ Ref b) => [a] -> [b] -> [b]
repeatedByRefIncremental xs ys = filter ((`elem` nuRefs) . toRef) ys
    where nuRefs = repeated $ map toRef xs ++ map toRef ys

-- | Return the elements with non-unique references: 
-- the elements with a reference that is present more than once in the list.
repeatedByRef :: forall a . Referable a => [a] -> [a]
repeatedByRef = repeatedByRefIncremental ([] :: [a])
