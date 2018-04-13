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
module TorXakis.Compiler.MapsTo where

import           Control.Arrow             (left)
import           Control.Lens              (Lens', to, (%~), (.~), (^.))
import           Control.Monad.Error.Class (liftEither)
import           Data.Either.Utils         (maybeToEither)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Proxy                (Proxy)
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Data.Type.Bool
import           GHC.TypeLits              (ErrorMessage ((:<>:), ShowType, Text),
                                            TypeError)
import           Prelude                   hiding (lookup)

import           TorXakis.Compiler.Data    hiding (lookup, lookupWithLoc,
                                            lookupWithLocM)
import           TorXakis.Compiler.Error

-- | 'm' maps keys of type 'k' onto values of type 'v'.
class (In (k, v) (Contents m) ~ 'True) => MapsTo k v m where
    lookup  :: (Ord k, Show k) => k -> m -> Either Error v
    lookupM :: (Ord k, Show k) => k -> m -> CompilerM v
    lookupM k m = liftEither $ lookup k m
    innerMap :: m -> Map k v

keys :: forall k v m . MapsTo k v m => m -> [k]
keys m = Map.keys im
    where
      im :: Map k v
      im = innerMap m

values :: forall k v m . MapsTo k v m => m -> [v]
values m = Map.elems im
    where
      im :: Map k v
      im = innerMap m

-- | Compute when a type is in a tree.
type family In (x :: *) (ys :: Tree *) :: Bool where
    In x ('Leaf y)   = x == y
    In x ('Node l r) = In x l || In x r

-- | Compute when two types are the same.
type family y == x :: Bool where
    x == x = 'True
    x == y = 'False

-- | Type family that represents the contents of a map.
type family Contents m :: Tree *

data Tree a = Leaf a | Node (Tree a) (Tree a)

type instance Contents (Map k v) = 'Leaf (k, v)
type instance Contents [a] = 'Leaf ()

instance MapsTo k v (Map k v) where
    lookup k m = maybeToEither err . Map.lookup k $ m
        where err = Error
                  { _errorType = UndefinedRef
                  , _errorLoc  = NoErrorLoc
                  , _errorMsg  = "Could not find " <> T.pack (show k)
                  }
    innerMap = id


lookupWithLoc :: (HasErrorLoc l, MapsTo k v mm, Ord k, Show k)
              => (k, l) -> mm -> Either Error v
lookupWithLoc (k, l) mm = lookup k mm <!> l

lookupWithLocM :: (HasErrorLoc l, MapsTo k v mm, Ord k, Show k)
               => (k, l) -> mm -> CompilerM v
lookupWithLocM p mm = liftEither $ lookupWithLoc p mm

-- | Combinator for maps.
data a :& b = a :& b

-- | Combine lists of key values pairs into a pair of maps.
(.&.) :: (Ord k0, Ord k1)
      => [(k0, v0)] -> [(k1, v1)] -> Map k0 v0 :& Map k1 v1
kv0 .&. kv1 = Map.fromList kv0 :& Map.fromList kv1

type instance Contents (a :& b) = 'Node (Contents a) (Contents b)

-- | The pair 'm0 :& m1' maps values of type 'k' onto values of type 'v'
class ( In (k, v) (Contents m0) ~ inM0
      , In (k, v) (Contents m1) ~ inM1
      , (inM0 || inM1) ~ 'True
      ) =>
      PairMapsTo k v m0 m1 inM0 inM1 where
    lookupPair :: (Ord k, Show k) => k -> m0 :& m1 -> Either Error v
    innerMapPair  :: m0 :& m1 -> Map k v

instance ( MapsTo k v m0, In (k, v) (Contents m1) ~ 'False
         ) => PairMapsTo k v m0 m1 'True 'False where
    lookupPair k (m0 :& _) = lookup k m0
    innerMapPair (m0 :& _) = innerMap m0

instance ( In (k, v) (Contents m0) ~ 'False, MapsTo k v m1
         ) => PairMapsTo k v m0 m1 'False 'True where
    lookupPair k (_ :& m1) = lookup k m1
    innerMapPair (_ :& m1) = innerMap m1

instance PairMapsTo k v m0 m1 inM0 inM1 => MapsTo k v (m0 :& m1) where
    lookup = lookupPair
    innerMap = innerMapPair

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
    lookupPair _ = undefined
    innerMapPair _ = undefined


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
    lookupPair _ = undefined
    innerMapPair _ = undefined
