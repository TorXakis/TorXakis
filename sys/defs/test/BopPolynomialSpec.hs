{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE FlexibleContexts #-}
module BopPolynomialSpec where

import           BopPolynomial
import           Data.Foldable
import           Data.List
import           Data.Monoid     hiding (Product (..))
import           GHC.Exts
import           Test.Hspec
import           Test.QuickCheck

multiplyLaw :: (Integral n, Eq a, IntMultipliable a, Show a) => n -> a -> Property
multiplyLaw n x
    =                 multiply   n'  x ===   fold (genericReplicate n' x)
    .&&. (n' /= 0 ==> multiply (-n') x === multiply (-1) (fold (genericReplicate n' x)))
    where n' = abs n

propIntMultipliableSum :: Int -> Int -> Property
propIntMultipliableSum n s = multiplyLaw n (Sum s)

propIntMultipliableProduct :: Int -> Double -> Property
propIntMultipliableProduct n s = multiplyLaw n (Product s)

fromListLaw :: (Eq a, IntMultipliable a, Show a, Ord a) => [a] -> Property
fromListLaw xs = fold xs === foldP (fromList xs)

propFromListSum :: [Int] -> Property
propFromListSum xs = fromListLaw (Sum <$> xs)

propFromListProduct :: [Double] -> Property
propFromListProduct xs = fromListLaw (Product <$> xs)

nrOfTermsLaw :: Ord a => [a] -> Property
nrOfTermsLaw xs = nrofTerms (fromList xs) === length (nub xs)

monoidLawEmpty0ForBoP :: (Eq a, Show a, Monoid (BopPolynomial a))
                     => BopPolynomial a -> Property
monoidLawEmpty0ForBoP p = p <> mempty === p

monoidLawEmpty1ForBoP :: (Eq a, Show a, Monoid (BopPolynomial a))
                     => BopPolynomial a -> Property
monoidLawEmpty1ForBoP p = mempty <> p === p

monoidLawMappendForBoP :: (Eq a, Show a, Monoid (BopPolynomial a))
                     => BopPolynomial a
                     -> BopPolynomial a
                     -> BopPolynomial a
                     -> Property
monoidLawMappendForBoP p0 p1 p2 = (p0 <> p1) <> p2 === p0 <> (p1 <> p2)

propMonoidEmpty0For :: (Eq (f a), Show (f a), Ord (f a)
                       , Monoid (BopPolynomial (f a)))
                       => (a -> f a) -> [a] -> Property
propMonoidEmpty0For f = monoidLawEmpty0ForBoP . fromList . (f <$>)

propMonoidEmpty1For :: (Eq (f a), Show (f a), Ord (f a)
                       , Monoid (BopPolynomial (f a)))
                       => (a -> f a) -> [a] -> Property
propMonoidEmpty1For f = monoidLawEmpty1ForBoP . fromList . (f <$>)

propMonoidMappendFor :: (Eq (f a), Show (f a), Ord (f a)
                        , Monoid (BopPolynomial (f a)))
                        => (a -> f a) -> [a] -> [a] -> [a] -> Property
propMonoidMappendFor f xs ys zs =
    monoidLawMappendForBoP  p0 p1 p2
    where
      p0 = fromList (f <$> xs)
      p1 = fromList (f <$> ys)
      p2 = fromList (f <$> xs)

spec :: Spec
spec = do
    describe "IntMultipliable instances" $ do
        it "Data.Monoid.Sum is an instance of `IntMultipliable`" $
            property propIntMultipliableSum
        it "Data.Monoid.Product is an instance of `IntMultipliable` provided were " $
            property propIntMultipliableProduct
    describe "Monoid instance of `BopPolynomial` for Sum" $ do
        it "Satisfies the first `mempty` law" $
            property (propMonoidEmpty0For Sum :: [Int] -> Property)
        it "Satisfies the second `mempty` law" $
            property (propMonoidEmpty1For Sum :: [Int] -> Property)
        it "Satisfies the `mapped` law" $
            property (propMonoidMappendFor Sum
                      :: [Int] -> [Int] -> [Int] -> Property)
    describe "Monoid instance of `BopPolynomial` for Product" $ do
        it "Satisfies the first `mempty` law" $
            property (propMonoidEmpty0For Product :: [Double] -> Property)
        it "Satisfies the second `mempty` law" $
            property (propMonoidEmpty1For Product :: [Double] -> Property)
        it "Satisfies the `mapped` law" $
            property (propMonoidMappendFor Product
                      :: [Double] -> [Double] -> [Double] -> Property)
    describe "Operations" $ do
        it "Data.Monoid.Sum satisfies the `fromList` laws" $
            property propFromListSum
        it "Data.Monoid.Product satisfies the `fromList` laws" $
            property propFromListProduct
        it "The number of distinct terms is correctly computed" $
            property (nrOfTermsLaw :: [Int] -> Property)

