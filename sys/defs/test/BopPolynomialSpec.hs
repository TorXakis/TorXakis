{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module BopPolynomialSpec where

import           BopPolynomial
import           Data.Foldable
import           Data.List
import           Data.Monoid
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

propIntMultipliableMyProduct :: Int -> Double -> Property
propIntMultipliableMyProduct n s = multiplyLaw n (MyProduct s)

fromListLaw :: (Eq a, IntMultipliable a, Show a, Ord a) => [a] -> Property
fromListLaw xs = fold xs === foldP (fromList xs)

propFromListSum :: [Int] -> Property
propFromListSum xs = fromListLaw (Sum <$> xs)

propFromListMyProduct :: [Double] -> Property
propFromListMyProduct xs = fromListLaw (MyProduct <$> xs)

nrOfTermsLaw :: Ord a => [a] -> Property
nrOfTermsLaw xs = nrofTerms (fromList xs) === length (nub xs)

spec :: Spec
spec = do
    describe "IntMultipliable instances" $ do
        it "Data.Monoid.Sum is an instance of `IntMultipliable`" $
            property propIntMultipliableSum
        it "Data.Monoid.MyProduct is an instance of `IntMultipliable` provided were " $
            property propIntMultipliableMyProduct
    describe "Operations" $ do
        it "Data.Monoid.Sum satisfies the `fromList` laws" $
            property propFromListSum
        it "Data.Monoid.MyProduct satisfies the `fromList` laws" $
            property propFromListMyProduct
        it "The number of distinct terms is correctly computed" $
            property (nrOfTermsLaw :: [Int] -> Property)

