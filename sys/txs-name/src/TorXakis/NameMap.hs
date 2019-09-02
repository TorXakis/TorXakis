{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  NameMap
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the NameMap.
-- A Map created by using the reference to the items.
----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE UndecidableInstances  #-}
module TorXakis.NameMap
( NameMap
, empty
, member
, TorXakis.NameMap.lookup
, elems
, insert
, union
, unions
, toNameMap
)
where
import           Control.Arrow
import           Control.DeepSeq
import           Data.Data
import qualified Data.HashMap         as HashMap

import           TorXakis.Name

-- | Map of objects with Names.
data NameMap a where
    NameMap :: HasName a =>
                HashMap.Map Name a -> NameMap a


-- | The empty map.
empty :: HasName a => NameMap a
empty = NameMap HashMap.empty

-- | Is an object with the provided name a member of the map?
member :: Name -> NameMap a -> Bool
member n (NameMap m) = HashMap.member n m

-- | Lookup the object with the provided name in the map.
lookup :: Name -> NameMap a -> Maybe a
lookup n (NameMap m) = HashMap.lookup n m

-- | Return all elements of the map in arbitrary order of their names.
elems :: NameMap a -> [a]
elems (NameMap m) = HashMap.elems m

-- | Insert a new value in the map using the name of the provided value as key.
-- If the key is already present in the map, the associated value is replaced with the supplied value.
insert :: a -> NameMap a -> NameMap a
insert v (NameMap m) = NameMap (HashMap.insert (getName v) v m)

-- | The (left-biased) union of two maps. It prefers the first map when duplicate keys are encountered.
union :: NameMap a -> NameMap a -> NameMap a
union (NameMap a) (NameMap b) = NameMap (HashMap.union a b)

-- | The union of a list of `NameMap`s.
unions :: HasName a => [NameMap a] -> NameMap a
unions ns = NameMap (HashMap.unions (map toHashMap ns))
    where
        toHashMap :: NameMap a -> HashMap.Map Name a
        toHashMap (NameMap a) = a


-- | Create a map from a list of named objects.
toNameMap :: HasName a => [a] -> NameMap a
toNameMap = NameMap . HashMap.fromList . map (\e -> (getName e, e))

---------------------------------------------------------------
-- instances
---------------------------------------------------------------
instance (Eq a, HasName a) => Eq (NameMap a) where
    (NameMap a) == (NameMap b) = a == b

instance (Ord a, HasName a) => Ord (NameMap a) where
    compare (NameMap a) (NameMap b) = compare a b

instance (Show a, HasName a) => Show (NameMap a) where
    show (NameMap m) = show m

instance (Read a, HasName a) => Read (NameMap a) where
    readsPrec n input = map (first NameMap) (readsPrec n input)

instance (Data a, HasName a) => Data (NameMap a) where
    gunfold k z _ = k (z NameMap)

    toConstr (NameMap _) = conRefMap

    dataTypeOf _ = tyT

conRefMap :: Constr
conRefMap = mkConstr tyT "NameMap" [] Prefix

tyT :: DataType
tyT = mkDataType "TorXakis.NameMap" [conRefMap]

instance NFData a => NFData (NameMap a) where
    rnf (NameMap m) = rnf m