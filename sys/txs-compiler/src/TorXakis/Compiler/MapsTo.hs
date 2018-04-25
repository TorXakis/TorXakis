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
import           Data.Proxy                (Proxy (Proxy))
import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Data.Type.Bool
import           Data.Typeable             (Typeable, typeOf)
import           GHC.TypeLits              (ErrorMessage ((:<>:), ShowType, Text),
                                            TypeError)
import           Prelude                   hiding (lookup)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error

-- | 'm' maps keys of type 'k' onto values of type 'v'.
class (In (k, v) (Contents m) ~ 'True) => MapsTo k v m where
    -- | Lookup a key in the map.
    lookup  :: (Ord k, Show k, Typeable k, Typeable v)
            => k -> m -> Either Error v
    -- | Monadic version of @lookup@.
    lookupM :: (Ord k, Show k, Typeable k, Typeable v)
            => k -> m -> CompilerM v
    lookupM k m = liftEither $ lookup k m
    -- | Get the inner map.
    innerMap :: m -> Map k v
    -- | Add a map. The addition is biased towards the new map. If the two maps
    -- have the same key, then the value of the right map is discarded, and the
    -- value of the left map is kept.
    (<.+>) :: Ord k => Map k v -> m -> m
    -- | Replace the inner map with the given one.
    replaceInnerMap :: m -> Map k v -> m

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
type instance Contents (Either a b) = 'Leaf ()

instance MapsTo k v (Map k v) where
    lookup k m = maybeToEither err . Map.lookup k $ m
        where err = Error
                  { _errorType = UndefinedRef
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

-- lookupWithLoc :: (HasErrorLoc l, MapsTo k v mm, Ord k, Show k)
--               => (k, l) -> mm -> Either Error v
-- lookupWithLoc (k, l) mm = lookup k mm <!> l

-- lookupWithLocM :: (HasErrorLoc l, MapsTo k v mm, Ord k, Show k)
--                => (k, l) -> mm -> CompilerM v
-- lookupWithLocM p mm = liftEither $ lookupWithLoc p mm

-- | Combinator for maps.
data a :& b = a :& b

-- | Combine lists of key values pairs into a pair of maps.
(.&.) :: (Ord k0, Ord k1)
      => [(k0, v0)] -> [(k1, v1)] -> Map k0 v0 :& Map k1 v1
kv0 .&. kv1 = Map.fromList kv0 :& Map.fromList kv1

-- | Combine a list of key values pairs with an existing map.
(.&) :: (Ord k0)
      => [(k0, v0)] -> mm -> Map k0 v0 :& mm
kv0 .& mm = Map.fromList kv0 :& mm

(<.++>) ::(Ord k, MapsTo k v mm) => [(k, v)] -> mm -> mm
xs <.++> m = Map.fromList xs <.+> m

type instance Contents (a :& b) = 'Node (Contents a) (Contents b)

-- | The pair 'm0 :& m1' maps values of type 'k' onto values of type 'v'
class ( In (k, v) (Contents m0) ~ inM0
      , In (k, v) (Contents m1) ~ inM1
      , (inM0 || inM1) ~ 'True
      ) =>
      PairMapsTo k v m0 m1 inM0 inM1 where
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
