{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall -Werror #-}
module FreeMonoidXSpec where

import           Data.AEq        (AEq, (~==))
import           Data.Foldable
import           Data.List
import           Data.Monoid     hiding (Product (..))
import           Data.Proxy
import           FreeMonoidX
import           GHC.Exts
import           Sum             (SumTerm (..))
import           Test.Hspec
import           Test.QuickCheck

-- * Instances of `IntMultipliable` used for testing purposes

-- | A term of a `FreeMonoidX` where the operation `<>` is the product (`*`)
newtype PProduct a = PProduct {getPProduct :: a}
    deriving (Show, Functor, Num, Ord)

instance AEq a => Eq (PProduct a) where
    (PProduct x) == (PProduct y) = x ~== y

instance Num a => Monoid (PProduct a) where
    mempty = PProduct 1
    (PProduct x) `mappend` (PProduct y) = PProduct $ x * y

instance Fractional a => Fractional (PProduct a) where
    fromRational r = PProduct (fromRational r)
    (PProduct x) / (PProduct y) = PProduct $ x / y

instance Fractional a => IntMultipliable (PProduct a) where
    -- Instances of `IntMultipliable` for `PPproduct` are only defined for
    -- fractional numbers. We cannot define this for `Int` (or `Integer`) since
    -- the multiplicative inverse is not defined for them.
    n <.> (PProduct x) = PProduct (x ^^ toInteger n)

-- * Laws of multiplication and instances of it

multiplyLaw :: (Integral n, Eq a, IntMultipliable a, Monoid a, Show a)
            => n -> a -> Property
multiplyLaw n x
    =    (               n' <.> x ===                 fold (genericReplicate n' x))
    .&&. (n' /= 0 ==> (-n') <.> x === (-1 :: Int) <.> fold (genericReplicate n' x))
    where n' = abs n

propIntMultipliableFor :: ( Eq (f a), IntMultipliable (f a)
                          , Monoid (f a), Show (f a))
                       => (a  -> f a) -> Proxy a -> Int -> a -> Property
propIntMultipliableFor f _ n s = multiplyLaw n (f s)

propIntMultipliableForFMX :: ( Eq (f a), IntMultipliable (f a), Ord (f a)
                             , Monoid (f a), Show (f a))
                          => ([a] -> FreeMonoidX (f a)) -> Proxy a -> Int -> [a]
                          -> Property
propIntMultipliableForFMX f _ n xs = multiplyLaw n (f xs)


-- * From list laws and properties

fromListLaw :: (Eq a, IntMultipliable a, Monoid a, Show a, Ord a)
            => [a] -> Property
fromListLaw xs = fold xs === foldFMX (fromList xs)

propFromListSum :: [Int] -> Property
propFromListSum xs = fromListLaw (SumTerm <$> xs)

propFromListProduct :: [Double] -> Property
propFromListProduct xs = fromListLaw (PProduct <$> xs)

-- * Properties of `nrOfTerms`

nrOfTermsLaw :: Ord a => Proxy a -> [a] -> Property
nrOfTermsLaw _ xs = nrofDistinctTerms (fromList xs) === length (nub xs)

-- * Monoid laws and properties `FreeMonoidX`

monoidLawEmpty0ForBoP :: (Eq a, Show a, Monoid (FreeMonoidX a))
                     => FreeMonoidX a -> Property
monoidLawEmpty0ForBoP p = p <> mempty === p

monoidLawEmpty1ForBoP :: (Eq a, Show a, Monoid (FreeMonoidX a))
                     => FreeMonoidX a -> Property
monoidLawEmpty1ForBoP p = mempty <> p === p

monoidLawMappendForBoP :: (Eq a, Show a, Monoid (FreeMonoidX a))
                       => FreeMonoidX a
                       -> FreeMonoidX a
                       -> FreeMonoidX a
                       -> Property
monoidLawMappendForBoP p0 p1 p2 = (p0 <> p1) <> p2 === p0 <> (p1 <> p2)

propMonoidEmpty0For :: (Eq (f a), Show (f a), Ord (f a)
                       , Monoid (FreeMonoidX (f a)))
                    => (a -> f a) -> Proxy a -> [a] -> Property
propMonoidEmpty0For f _ = monoidLawEmpty0ForBoP . fromList . (f <$>)

propMonoidEmpty1For :: (Eq (f a), Show (f a), Ord (f a)
                       , Monoid (FreeMonoidX (f a)))
                    => (a -> f a) -> Proxy a -> [a] -> Property
propMonoidEmpty1For f _ = monoidLawEmpty1ForBoP . fromList . (f <$>)

propMonoidMappendFor :: (Eq (f a), Show (f a), Ord (f a)
                        , Monoid (FreeMonoidX (f a)))
                     => (a -> f a) -> Proxy a -> [a] -> [a] -> [a] -> Property
propMonoidMappendFor f _ xs ys zs =
    monoidLawMappendForBoP  p0 p1 p2
    where
      p0 = fromList (f <$> xs)
      p1 = fromList (f <$> ys)
      p2 = fromList (f <$> zs)

-- * Properties of append

propAppendFor :: (Eq (f a), Show (f a), Ord (f a)
                 , IntMultipliable (f a)
                 , Monoid (f a)
                 , Monoid (FreeMonoidX (f a)))
              => (a -> f a) -> Proxy a ->a -> [a] -> Property
propAppendFor f _ x xs = foldFMX (append x' p) === x' <> foldFMX p
    where
      x' = f x
      p  = fromList (f <$> xs)


-- * Properties of remove

propRemoveFor :: (Eq (f a), Show (f a), Ord (f a)
                 , IntMultipliable (f a)
                 , Monoid (f a)
                 , Monoid (FreeMonoidX (f a)))
              => (a -> f a) -> Proxy a -> a -> [a] -> Property
propRemoveFor f _ x xs =
    foldFMX (remove x' p) ===  ((-1 :: Int) <.> x') <> foldFMX p
    where
      x' = f x
      p  = fromList (f <$> xs)

-- * Proxies for instantiating properties to the correct (concrete) types

pInt :: Proxy Int
pInt = Proxy

pDouble :: Proxy Double
pDouble = Proxy

-- * Main specification

spec :: Spec
spec = do
    describe "IntMultipliable instances" $ do
        it "`SumTerm` is an instance of `IntMultipliable`" $
            property $ propIntMultipliableFor SumTerm pInt
        it "`PProduct` is an instance of `IntMultipliable`" $
            property $ propIntMultipliableFor PProduct pDouble
        it "`FreeMonoidX` with `SumTerm` terms is an instance of `IntMultipliable`" $
            property $ propIntMultipliableForFMX (fromList . (SumTerm <$>)) pInt
        it "`FreeMonoidX` with `PProduct` terms is an instance of `IntMultipliable`" $
            property $ propIntMultipliableForFMX (fromList . (PProduct <$>)) pDouble
    describe "Monoid instance of `FreeMonoidX` for Sum" $ do
        it "Satisfies the first `mempty` law" $
            property $ propMonoidEmpty0For SumTerm pInt
        it "Satisfies the second `mempty` law" $
            property $ propMonoidEmpty1For SumTerm pInt
        it "Satisfies the `mapped` law" $
            property $ propMonoidMappendFor SumTerm pInt
    describe "Monoid instance of `FreeMonoidX` for Product" $ do
        it "Satisfies the first `mempty` law" $
            property $ propMonoidEmpty0For PProduct pDouble
        it "Satisfies the second `mempty` law" $
            property $ propMonoidEmpty1For PProduct pDouble
        it "Satisfies the `mapped` law" $
            property $ propMonoidMappendFor PProduct pDouble
    describe "Operations" $ do
        it "SumTerm satisfies the `fromList` laws" $
            property propFromListSum
        it "Data.Monoid.PProduct satisfies the `fromList` laws" $
            property propFromListProduct
        it "The number of distinct terms is correctly computed" $
            property $ nrOfTermsLaw pInt
        it "append should append a term to the polynomial (SumTerm)" $
            property $ propAppendFor SumTerm pInt
        it "append should append a term to the polynomial (PProduct)" $
            property $ propAppendFor PProduct pDouble
        it "remove should remove a term to the polynomial (SumTerm)" $
            property $ propRemoveFor SumTerm pInt
        it "remove should remove a term to the polynomial (PProduct)" $
            property $ propRemoveFor PProduct pDouble