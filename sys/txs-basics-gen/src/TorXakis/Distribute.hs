{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribute
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a generator for distributing identical objects into distinct bins.
-- For background information, see e.g. [brilliant.org](https://brilliant.org/wiki/identical-objects-into-distinct-bins/)
-----------------------------------------------------------------------------
module TorXakis.Distribute
( 
-- * Distribute identical objects over distinct bins
  distribute
)
where
import Test.QuickCheck

-- | Distribute identical objects over distinct bins.
-- The implementation is based on the 
-- [star and banner method](https://brilliant.org/wiki/identical-objects-into-distinct-bins/#stars-and-bars-approach).
-- 
-- 'Int' was chosen since in accordance with the 
-- [size of QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.12.6.1/docs/Test-QuickCheck-Gen.html#v:getSize).
--
-- prop> \(NonNegative n)-> \(NonNegative m) -> length (distribute n m) == m
--
-- prop> \(NonNegative n)-> \(Positive m) -> sum (distribute n m) == n
--
distribute :: Int -- ^ number of identical objects (>= 0)
           -> Int -- ^ number of distinct bins (>= 0)
           -> Gen [Int]
distribute m _ | m < 0 = error ("distribute called with negative number of identical objects " ++ show m)
distribute _ n | n < 0 = error ("distribute called with negative number of bins " ++ show n)
distribute _ 0 = return []
distribute 0 m = return $ replicate m 0      -- Nothing to distribute           -- TODO: does this really spead up?
distribute n m =
    let stars = replicate n     True
        bars  = replicate (m-1) False
     in do
        shuffled <- shuffle (stars ++ bars)
        return $ starSum shuffled 0
    where starSum :: [Bool] -> Int -> [Int]
          starSum []         x = [x]
          starSum (True :ys) x = starSum ys (x+1)   -- Star
          starSum (False:ys) x = x : starSum ys 0   -- Bar