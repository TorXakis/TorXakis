{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  KnuthShuffle
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module KnuthShuffle (
knuthShuffle
) where

import System.Random

-- From https://rosettacode.org/wiki/Knuth_shuffle#Haskell
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l = let (a,b) = splitAt i l in a++c:drop 1 b

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i,j) xs | i==j = xs
                   | otherwise = replaceAt j (xs!!i) $ replaceAt i (xs!!j) xs

knuthShuffle :: [a] -> IO [a]
knuthShuffle xs =
    fmap (foldr swapElems xs. zip [1..]) (mkRands (length xs))
  where
    mkRands = mapM (randomRIO.(,) 0) . enumFromTo 1 . pred
-- knuthShuffle

