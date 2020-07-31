{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Pairs
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module Pairs (
mapPairs,
mapPairsM,
mapWithFunc,
mapWithFuncM,
searchPairs,
searchPairsM,
searchWithFunc,
searchWithFuncM
) where

import qualified Control.Monad as Monad

mapPairs :: (a -> b -> c) -> [a] -> [b] -> [c]
mapPairs f xs ys = [ f x y | x <- xs, y <- ys ]

mapPairsM :: Monad.Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
mapPairsM f xs ys = mapWithFuncM f (const ys) xs

mapWithFunc :: (a -> b -> c) -> (a -> [b]) -> [a] -> [c]
mapWithFunc f g xs = concat [ [ f x y | y <- g x ] | x <- xs ]

mapWithFuncM :: Monad.Monad m => (a -> b -> m c) -> (a -> [b]) -> [a] -> m [c]
mapWithFuncM _ _ [] = return []
mapWithFuncM f g (x:xs) = iter (x:xs) (g x)
  where
    iter [] _ = return []
    iter [_] [] = return []
    iter (_:u:us) [] = iter (u:us) (g u)
    iter (u:us) (v:vs) = do
        newElem <- f u v
        otherElems <- iter (u:us) vs
        return (newElem : otherElems)
-- mapWithFuncM

searchPairs :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe c
searchPairs f xs ys = searchWithFunc f (const ys) xs

searchPairsM :: Monad.Monad m => (a -> b -> m (Maybe c)) -> [a] -> [b] -> m (Maybe c)
searchPairsM f xs ys = searchWithFuncM f (const ys) xs

searchWithFunc :: (a -> b -> Maybe c) -> (a -> [b]) -> [a] -> Maybe c
searchWithFunc _ _ [] = Nothing
searchWithFunc f g (x:xs) = search (x:xs) (g x)
  where
    search [] _ = Nothing
    search [_] [] = Nothing
    search (_:u:us) [] = search (u:us) (g u)
    search (u:us) (v:vs) =
        let s = f u v in
          case s of
            Just _ -> s
            Nothing -> search (u:us) vs
-- mapWithFuncM

searchWithFuncM :: Monad.Monad m => (a -> b -> m (Maybe c)) -> (a -> [b]) -> [a] -> m (Maybe c)
searchWithFuncM _ _ [] = return Nothing
searchWithFuncM f g (x:xs) = search (x:xs) (g x)
  where
    search [] _ = return Nothing
    search [_] [] = return Nothing
    search (_:u:us) [] = search (u:us) (g u)
    search (u:us) (v:vs) = do
        s <- f u v
        case s of
          Just _ -> return s
          Nothing -> search (u:us) vs
-- mapWithFuncM



