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
----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
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
import           Control.Arrow
import           Control.DeepSeq
import           Data.Data
import           Data.Hashable
import qualified Data.HashMap         as HashMap
import           Data.List.Unique     (repeated)

import           TorXakis.Referable

-- | Map of Referable objects.
data RefMap a where
    RefMap :: (Referable a, Ord (Ref a), Hashable (Ref a)) =>
                HashMap.Map (Ref a) a -> RefMap a


-- | The empty map.
empty :: (Referable a, Ord (Ref a), Hashable (Ref a)) => RefMap a
empty = RefMap HashMap.empty

-- | Is the key a member of the map?
member :: Ref a -> RefMap a -> Bool
member r (RefMap m) = HashMap.member r m

-- | Lookup the value at a key in the map.
lookup :: Ref a -> RefMap a -> Maybe a
lookup r (RefMap m) = HashMap.lookup r m

-- | Return all elements of the map in arbitrary order of their keys.
elems :: RefMap a -> [a]
elems (RefMap m) = HashMap.elems m

-- | Insert a new value in the map using the provided reference as key.
-- If the key is already present in the map, the associated value is replaced with the supplied value.
--
-- The user must ensure for derivable references that the provided key and value adhere to
-- toRef c v == k
-- where c is the context needed to derive the reference from the value.
insert :: Ref a -> a -> RefMap a -> RefMap a
insert k v (RefMap m) = RefMap (HashMap.insert k v m)

-- | The (left-biased) union of two maps. It prefers the first map when duplicate keys are encountered.
union :: RefMap a -> RefMap a -> RefMap a
union (RefMap a) (RefMap b) = RefMap (HashMap.union a b)

-- | Create a map from a list of Referable objects.
toRefMap :: (RefDerivable a c, Ord (Ref a), Hashable (Ref a)) => c -> [a] -> RefMap a
toRefMap ctx = RefMap . HashMap.fromList . map (\e -> (toRef ctx e, e))

-- | Return the elements with non-unique references that the second list contains in the combination of the first and second list.
repeatedByRefIncremental :: (RefDerivable a c, Ord (Ref a)) => c -> [a] -> [a] -> [a]
repeatedByRefIncremental ctx xs ys = filter ((`elem` nuRefs) . (toRef ctx)) ys
    where nuRefs = repeated $ map (toRef ctx) xs ++ map (toRef ctx) ys

-- | Return the elements with non-unique references: 
-- the elements with a reference that is present more than once in the list.
repeatedByRef :: (RefDerivable a c, Ord (Ref a)) => c -> [a] -> [a]
repeatedByRef ctx = repeatedByRefIncremental ctx []

---------------------------------------------------------------
-- instances
---------------------------------------------------------------
instance (Eq a, Eq (Ref a)) => Eq (RefMap a) where
    (RefMap a) == (RefMap b) = a == b

instance (Ord a, Ord (Ref a)) => Ord (RefMap a) where
    compare (RefMap a) (RefMap b) = compare a b

instance (Show a, Show (Ref a)) => Show (RefMap a) where
    show (RefMap m) = show m

instance ( Referable a, Ord (Ref a), Hashable (Ref a)
         , Read a, Read (Ref a)
         ) => Read (RefMap a) where
    readsPrec n input = map (first RefMap) (readsPrec n input)

instance ( Referable a, Ord (Ref a), Hashable (Ref a)
         , Data a, Data (Ref a)
         ) => Data (RefMap a) where
    gunfold k z _ = k (z RefMap)

    toConstr (RefMap _) = con_RefMap

    dataTypeOf _ = ty_T

con_RefMap :: Constr
con_RefMap = mkConstr ty_T "RefMap" [] Prefix

ty_T :: DataType
ty_T = mkDataType "TorXakis.RefMap" [con_RefMap]

instance (NFData a, NFData (Ref a)) => NFData (RefMap a) where
    rnf (RefMap m) = rnf m