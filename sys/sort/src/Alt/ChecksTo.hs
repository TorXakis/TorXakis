{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
module Alt.ChecksTo where

import           GHC.Generics
import           Data.Text       (Text)
import qualified Data.Map.Strict as Map

import           Alt.Name
import           Alt.Sort
import           Alt.Field
import           Alt.Cstr
import           Alt.LookupTable

-- | Can type 'a' be transformed to type 'b' after a consistency check?
class ChecksTo a b where
    checkTo :: a -> b

    default checkTo :: (Generic a, Generic b, GChecksTo (Rep a) (Rep b)) => a -> b
    checkTo = to . gCheckTo . from

instance ChecksTo a a where
    checkTo = id

newtype Foo = Foo String deriving (Show, Generic)

newtype Bar = Bar String deriving (Show, Generic)

instance ChecksTo Foo Bar

instance ChecksTo FieldD Field where
    checkTo (FieldD n s) = Field n (fromText s)

instance ChecksTo CstrD Cstr

instance (HasName a, ChecksTo a b) => ChecksTo [a] (LookupTable b) where
    checkTo xs = Map.fromList $ zip (name <$> xs) (checkTo <$> xs)

class GChecksTo f g where
    gCheckTo :: f a -> g a

instance GChecksTo U1 U1 where
    gCheckTo U1 = U1

instance GChecksTo a a' => GChecksTo (M1 i c a) (M1 i' c' a') where
    gCheckTo (M1 x) = M1 (gCheckTo x)

instance ChecksTo a a' => GChecksTo (K1 i a) (K1 i' a') where
    gCheckTo (K1 a) = K1 (checkTo a)

instance (GChecksTo a a', GChecksTo b b') => GChecksTo (a :*: b) (a' :*: b')
  where    
    gCheckTo (a :*: b) = gCheckTo a :*: gCheckTo b

instance (GChecksTo a a', GChecksTo b b') => GChecksTo (a :+: b) (a' :+: b')
  where    
    gCheckTo (L1 a) = L1 (gCheckTo a)
    gCheckTo (R1 a) = R1 (gCheckTo a)


