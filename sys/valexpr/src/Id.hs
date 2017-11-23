{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Async
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Damian Nadales <damian.nadales@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides a type-wrapper around id's.
-----------------------------------------------------------------------------

module Id where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Data
import           Data.Foldable
import           Data.Text           (Text)
import           GHC.Generics

newtype Id = Id { _id :: Int }
    deriving (Eq, Show, Ord, Enum, Num, Read, NFData, Data)

-- * Resettable

-- | Class of expressions that contain values of type @Id@ that can be reset
-- (set to zero).
class Resettable e where
    -- | Reset all the @Id@'s inside an expression.
    reset :: e -> e

    default reset :: (Generic e, GResettable (Rep e)) => e -> e
    reset = to . gReset . from

instance Resettable Id where
    reset _ = Id 0

instance Resettable Text where
    reset = id

instance Resettable a => Resettable [a] where
    reset = (reset <$>)

class GResettable f where
    gReset :: f e -> f e

-- | Resetting a constructor without arguments will give the same result, since
-- no @Id@ can be found there.
instance GResettable U1 where
    gReset U1 = U1

-- | Resetting the product is equal to resetting each term in the product.
instance (GResettable a, GResettable b) => GResettable (a :*: b) where
    gReset (a :*: b) = gReset a :*: gReset b

-- | Resetting the sum amounts to resetting the each term of it as well.
instance (GResettable a, GResettable b) => GResettable (a :+: b) where
    gReset (L1 x) = L1 (gReset x)
    gReset (R1 x) = R1 (gReset x)

-- | We do need to do anything for resetting the meta-data.
instance (GResettable a) => GResettable (M1 i c a) where
    gReset (M1 x) = M1 (gReset x)

-- | And the only interesting case: resetting the arguments of the type
-- constructors. In this case we have to use our @Reset@ (__not @GReset@__).
instance (Resettable a) => GResettable (K1 i a) where
    gReset (K1 x) = K1 (reset x)

-- * Identifiable

-- | Expression that contains a single @Id@
class Identifiable e where
    getId :: e -> Maybe Id

    default getId :: (Generic e, GIdentifiable (Rep e)) => e -> Maybe Id
    getId = gGetId . from

instance Identifiable Id where
    getId = Just

instance Identifiable Text where
    getId = const Nothing

instance Identifiable a => Identifiable [a] where
    getId = asum . (getId <$>)

class GIdentifiable f where
    gGetId :: f e -> Maybe Id

instance GIdentifiable U1 where
    gGetId U1 = Nothing


-- | Getting the identifier of a product amounts to getting the first @Id@
-- identifiable of the first identifiable value. An identifiable instance
-- should contain at least one such a value, but we do now check for this.
instance (GIdentifiable a, GIdentifiable b) => GIdentifiable (a :*: b) where
    gGetId (a :*: b) = gGetId a <|> gGetId b

-- | Getting the identifier of the sum is the same as getting the identifier of
-- the term.
instance (GIdentifiable a, GIdentifiable b) => GIdentifiable (a :+: b) where
    gGetId (L1 x) = gGetId x
    gGetId (R1 x) = gGetId x

-- | Nothing to do for the metadata.
instance (GIdentifiable a) => GIdentifiable (M1 i c a) where
    gGetId (M1 x) = gGetId x

-- | And getting the id of a constructor is the same as getting the id of its
-- argument.
instance (Identifiable a) => GIdentifiable (K1 i a) where
    gGetId (K1 x) = getId x
