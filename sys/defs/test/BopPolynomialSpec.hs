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

multiplyLaw :: (Integral n, Eq a, Multiply a, Show a) => n -> a -> Property
multiplyLaw n x = 0 <= n ==> multiply n x === fold (genericReplicate n x)

propMultiplySum :: Int -> Int -> Property
propMultiplySum n s = multiplyLaw n (Sum s)

propMultiplyProduct :: Int -> Int -> Property
propMultiplyProduct n s = multiplyLaw n (Product s)

fromListLaw :: (Eq a, Multiply a, Show a, Ord a) => [a] -> Property
fromListLaw xs = fold xs === foldP (fromList xs)

propFromListSum :: [Int] -> Property
propFromListSum xs = fromListLaw (Sum <$> xs)

propFromListProduct :: [Int] -> Property
propFromListProduct xs = fromListLaw (Product <$> xs)

spec :: Spec
spec = do
    describe "Multiply instances" $ do
        it "Data.Monoid.Sum is an instance of `Multiply`" $
            property propMultiplySum
        it "Data.Monoid.Product is an instance of `Multiply`" $
            property propMultiplyProduct
    describe "From list laws" $ do
        it "Data.Monoid.Sum satisfy the laws" $
            property propFromListSum
        it "Data.Monoid.Product satisfy the laws" $
            property propFromListProduct

