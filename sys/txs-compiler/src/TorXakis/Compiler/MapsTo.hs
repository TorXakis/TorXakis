{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.MapsTo
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definition for the 'MapsTo' class, and associated operators.
--
-- A constraint of the form 'MapsTo k v mm' denotes that 'mm' contains a map
-- from keys of type 'k' to values of type 't'.
--------------------------------------------------------------------------------
module TorXakis.Compiler.MapsTo
    ( -- * Predicates on maps
      MapsTo
    , In
    , Contents
      -- * Lookup
    , lookup
    , lookupM
    -- * Extraction of maps, keys and values
    , keys
    , values
    , innerMap
    -- * Combination of maps
    , (:&) ((:&))
    , (<.+>)
    , (<.++>)
    , (.&)
    , (.&.)
    , replaceInnerMap
    )
where

import           Control.Monad.Except    (liftEither)
import           Data.Either.Utils       (maybeToEither)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Proxy              (Proxy (Proxy))
import qualified Data.Text               as T
import           Data.Type.Bool          (type (||))
import           Data.Typeable           (Typeable, typeOf)
import           Data.Kind               (Type)
import           GHC.TypeLits            (ErrorMessage ((:<>:), ShowType, Text),
                                          TypeError)
import           Prelude                 hiding (lookup)

import           TorXakis.Compiler.Data  (CompilerM)
import           TorXakis.Compiler.Error (Entity (Entity), Error (Error),
                                          ErrorLoc (NoErrorLoc),
                                          ErrorType (Undefined), _errorLoc,
                                          _errorMsg, _errorType)

-- | 'mm' maps keys of type 'k' onto values of type 'v'. The type 'mm' can be
-- thought as a composite map, which contains multiple maps. See @:&@ for more
-- details.
class (In (k, v) (Contents mm) ~ 'True) => MapsTo k v mm where
    -- | Lookup a key in the map.
    lookup  :: (Ord k, Show k, Typeable k, Typeable v)
            => k -> mm -> Either Error v
    -- | Monadic version of @lookup@.
    lookupM :: (Ord k, Show k, Typeable k, Typeable v)
            => k -> mm -> CompilerM v
    lookupM k m = liftEither $ lookup k m
    -- | Get the inner map.
    innerMap :: mm -> Map k v
    -- | Add a map. The addition is biased towards the new map. If the two maps
    -- have the same key, then the value of the right map is discarded, and the
    -- value of the left map is kept.
    (<.+>) :: Ord k => Map k v -> mm -> mm
    infixl 4 <.+>
    -- | Replace the inner map with the given one.
    replaceInnerMap :: mm -> Map k v -> mm

-- | Get all the keys of type 'k' that are associated to values of type 'v' in
-- the given map.
keys :: forall k v mm . MapsTo k v mm => mm -> [k]
keys mm = Map.keys im
    where
      im :: Map k v
      im = innerMap mm

-- | Get all the values of type 'v' that are associated to keys of type 'k' in
-- the given map.
values :: forall k v mm . MapsTo k v mm => mm -> [v]
values mm = Map.elems im
    where
      im :: Map k v
      im = innerMap mm

-- | Compute when a type is in a tree.
type family In (x :: Type) (ys :: Tree Type) :: Bool where
    In x ('Leaf y)   = x == y
    In x ('Node l r) = In x l || In x r

-- | Compute when two types are the same.
type family y == x :: Bool where
    x == x = 'True
    x == y = 'False

-- | Type family that represents the contents of a map.
type family Contents mm :: Tree Type

-- | Search tree for types. This type would contain the key-value pairs found
-- in a composite map.
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- | A map from 'k' to 'v' is defined to contain a 'k' 'v' association.
type instance Contents (Map k v) = 'Leaf (k, v)

-- | A list of any type is not a map.
type instance Contents [a] = 'Leaf ()

-- | Canonical instance: a map from 'k' to 'v' maps 'k' to 'v'.
instance MapsTo k v (Map k v) where
    lookup k = maybeToEither err . Map.lookup k
        where err = Error
                  { _errorType = Undefined Entity
                  -- No error location can be given. However the error location
                  -- could be overwritten by the caller of 'lookup'.
                  , _errorLoc  = NoErrorLoc
                  , _errorMsg  =  "Could not find key " <> T.pack (show k)
                               <> " of type " <> (T.pack . show . typeOf $ k)
                               <> " when looking for a value of type "
                               <> (T.pack . show . typeOf $ (Proxy :: Proxy v))
                  }
    innerMap = id
    -- Note that here we're using the monoidal implementation of <> for maps,
    -- which overwrites the keys of 'm1'.
    (<.+>) = (<>)
    replaceInnerMap _ new = new

-- | Combinator for maps.
data a :& b = a :& b
infixr 5 :&

-- | Combine lists of key values pairs into a pair of maps.
(.&.) :: (Ord k0, Ord k1)
      => [(k0, v0)] -> [(k1, v1)] -> Map k0 v0 :& Map k1 v1
kv0 .&. kv1 = Map.fromList kv0 :& Map.fromList kv1
infixr 6 .&.

-- | Combine a list of key values pairs with an existing map.
(.&) :: (Ord k0)
      => [(k0, v0)] -> mm -> Map k0 v0 :& mm
kv0 .& mm = Map.fromList kv0 :& mm
infixr 6 .&

-- | Like @<.+>@ but its left argument is a list instead of a map. Useful when
-- we want to add a list to a composite map.
(<.++>) ::(Ord k, MapsTo k v mm) => [(k, v)] -> mm -> mm
xs <.++> m = Map.fromList xs <.+> m
infixr 6 <.++>

-- | The composite structure contains the contents of 'a' and 'b'.
type instance Contents (a :& b) = 'Node (Contents a) (Contents b)

-- | The pair 'm0 :& m1' maps values of type 'k' onto values of type 'v'
class ( In (k, v) (Contents m0) ~ inM0
      , In (k, v) (Contents m1) ~ inM1
      , (inM0 || inM1) ~ 'True
      ) => PairMapsTo k v m0 m1 inM0 inM1 where
    lookupPair :: (Ord k, Show k, Typeable k, Typeable v)
               => k -> m0 :& m1 -> Either Error v
    innerMapPair :: m0 :& m1 -> Map k v
    addMapPair :: Ord k => Map k v -> m0 :& m1 -> m0 :& m1
    replaceInnerMapPair :: m0 :& m1 -> Map k v -> m0 :& m1

instance ( MapsTo k v m0, In (k, v) (Contents m1) ~ 'False
         ) => PairMapsTo k v m0 m1 'True 'False where
    lookupPair k (m0 :& _)     = lookup k m0
    innerMapPair (m0 :& _)     = innerMap m0
    addMapPair m (m0 :& m1)    = (m <.+> m0) :& m1
    replaceInnerMapPair (m0 :& m1) new = replaceInnerMap m0 new :& m1

instance ( In (k, v) (Contents m0) ~ 'False, MapsTo k v m1
         ) => PairMapsTo k v m0 m1 'False 'True where
    lookupPair k (_ :& m1)     = lookup k m1
    innerMapPair (_ :& m1)     = innerMap m1
    addMapPair m (m0 :& m1)    = m0 :& (m <.+> m1)
    replaceInnerMapPair (m0 :& m1) new = m0 :& replaceInnerMap m1 new

instance PairMapsTo k v m0 m1 inM0 inM1 => MapsTo k v (m0 :& m1) where
    lookup = lookupPair
    innerMap = innerMapPair
    (<.+>) = addMapPair
    replaceInnerMap = replaceInnerMapPair

instance ( TypeError (      'Text "No map found: \""
                      ':<>: 'ShowType (m0 :& m1)
                      ':<>: 'Text "\" does not contain a map from "
                      ':<>: 'ShowType k
                      ':<>: 'Text " to "
                      ':<>: 'ShowType v
                     )
         , In (k, v) (Contents m0) ~ 'False
         , In (k, v) (Contents m1) ~ 'False
         , 'False ~ 'True )
         => PairMapsTo k v m0 m1 'False 'False where
    lookupPair          _ = undefined
    innerMapPair        _ = undefined
    addMapPair          _ = undefined
    replaceInnerMapPair _ =  undefined

instance ( TypeError (     'Text "Map is not unique: \""
                      ':<>: 'ShowType (m0 :& m1)
                      ':<>: 'Text "\" contains more than a map from "
                      ':<>: 'ShowType k
                      ':<>: 'Text " to "
                      ':<>: 'ShowType v
                     )
         , In (k, v) (Contents m0) ~ 'True
         , In (k, v) (Contents m1) ~ 'True
         )
         => PairMapsTo k v m0 m1 'True 'True where
    lookupPair          _ = undefined
    innerMapPair        _ = undefined
    addMapPair          _ = undefined
    replaceInnerMapPair _ =  undefined
