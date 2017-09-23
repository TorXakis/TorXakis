{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BopPolynomialSpec where

import           BopPolynomial
import           Data.AEq        (AEq, (~==))
import           Data.Foldable
import           Data.List
import           Data.Monoid     hiding (Product (..))
import           Data.Proxy
import           GHC.Exts
import           Test.Hspec
import           Test.QuickCheck

-- * Instances of `IntMultipliable` used for testing purposes

-- | A term of `BopPolynomial` where the operation `<>` is the sum (`+`)
newtype PSum a = PSum { getSum :: a }
    deriving (Eq, Show, Functor, Num, Ord)

instance Integral a => IntMultipliable (PSum a) where
    multiply n (PSum x) = PSum (fromInteger $ toInteger x * toInteger n)

instance Num a => Monoid (PSum a) where
    mempty = PSum 0
    (PSum x) `mappend` (PSum y) = PSum $ x + y

-- | A term of a `BopPolynomial` where the operation `<>` is the product (`*`)
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
    multiply n (PProduct x) = PProduct (x ^^ toInteger n)

-- * Laws of multiplication and instances of it

multiplyLaw :: (Integral n, Eq a, IntMultipliable a, Monoid a, Show a)
            => n -> a -> Property
multiplyLaw n x
    =                 multiply   n'  x ===   fold (genericReplicate n' x)
    .&&. (n' /= 0 ==> multiply (-n') x === multiply (-1) (fold (genericReplicate n' x)))
    where n' = abs n

propIntMultipliableFor :: ( Eq (f a), IntMultipliable (f a)
                          , Monoid (f a), Show (f a))
                       => (a  -> f a) -> Proxy a -> Int -> a -> Property
propIntMultipliableFor f _ n s = multiplyLaw n (f s)

-- * From list laws and properties

fromListLaw :: (Eq a, IntMultipliable a, Monoid a, Show a, Ord a)
            => [a] -> Property
fromListLaw xs = fold xs === foldP (fromList xs)

propFromListSum :: [Int] -> Property
propFromListSum xs = fromListLaw (PSum <$> xs)

propFromListProduct :: [Double] -> Property
propFromListProduct xs = fromListLaw (PProduct <$> xs)

-- * Properties of `nrOfTerms`

nrOfTermsLaw :: Ord a => Proxy a -> [a] -> Property
nrOfTermsLaw _ xs = nrofDistinctTerms (fromList xs) === length (nub xs)

-- * Monoid laws and properties `BopPolynomial`

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
                    => (a -> f a) -> Proxy a -> [a] -> Property
propMonoidEmpty0For f _ = monoidLawEmpty0ForBoP . fromList . (f <$>)

propMonoidEmpty1For :: (Eq (f a), Show (f a), Ord (f a)
                       , Monoid (BopPolynomial (f a)))
                    => (a -> f a) -> Proxy a -> [a] -> Property
propMonoidEmpty1For f _ = monoidLawEmpty1ForBoP . fromList . (f <$>)

propMonoidMappendFor :: (Eq (f a), Show (f a), Ord (f a)
                        , Monoid (BopPolynomial (f a)))
                     => (a -> f a) -> Proxy a -> [a] -> [a] -> [a] -> Property
propMonoidMappendFor f _ xs ys zs =
    monoidLawMappendForBoP  p0 p1 p2
    where
      p0 = fromList (f <$> xs)
      p1 = fromList (f <$> ys)
      p2 = fromList (f <$> xs)

-- * Properties of append

propAppendFor :: (Eq (f a), Show (f a), Ord (f a)
                 , IntMultipliable (f a)
                 , Monoid (f a)
                 , Monoid (BopPolynomial (f a)))
              => (a -> f a) -> Proxy a ->a -> [a] -> Property
propAppendFor f _ x xs = foldP (append x' p) === x' <> foldP p
    where
      x' = f x
      p  = fromList (f <$> xs)


-- * Properties of remove

propRemoveFor :: (Eq (f a), Show (f a), Ord (f a)
                 , IntMultipliable (f a)
                 , Monoid (f a)
                 , Monoid (BopPolynomial (f a)))
              => (a -> f a) -> Proxy a -> a -> [a] -> Property
propRemoveFor f _ x xs = foldP (remove x' p) === multiply (-1) x' <> foldP p
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
        it "`PSum` is an instance of `IntMultipliable`" $
            property $ propIntMultipliableFor PSum pInt
        it "`PProduct` is an instance of `IntMultipliable` provided were " $
            property $ propIntMultipliableFor PProduct pDouble
    describe "Monoid instance of `BopPolynomial` for Sum" $ do
        it "Satisfies the first `mempty` law" $
            property $ propMonoidEmpty0For PSum pInt
        it "Satisfies the second `mempty` law" $
            property $ propMonoidEmpty1For PSum pInt
        it "Satisfies the `mapped` law" $
            property $ propMonoidMappendFor PSum pInt
    describe "Monoid instance of `BopPolynomial` for Product" $ do
        it "Satisfies the first `mempty` law" $
            property $ propMonoidEmpty0For PProduct pDouble
        it "Satisfies the second `mempty` law" $
            property $ propMonoidEmpty1For PProduct pDouble
        it "Satisfies the `mapped` law" $
            property $ propMonoidMappendFor PProduct pDouble
    describe "Operations" $ do
        it "PSum satisfies the `fromList` laws" $
            property propFromListSum
        it "Data.Monoid.PProduct satisfies the `fromList` laws" $
            property propFromListProduct
        it "The number of distinct terms is correctly computed" $
            property $ nrOfTermsLaw pInt
        it "append should append a term to the polynomial (PSum)" $
            property $ propAppendFor PSum pInt
        it "append should append a term to the polynomial (PProduct)" $
            property $ propAppendFor PProduct pDouble
        it "remove should remove a term to the polynomial (PSum)" $
            property $ propRemoveFor PSum pInt
        it "remove should remove a term to the polynomial (PProduct)" $
            property $ propRemoveFor PProduct pDouble
