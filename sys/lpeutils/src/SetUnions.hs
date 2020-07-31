{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  SetUnions
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module SetUnions (
setUnions,
setIntersections
) where

import qualified Data.Set as Set

-- Because Set.unions does not work on sets of sets for some reason?
setUnions :: (Foldable f, Ord a) => f (Set.Set a) -> Set.Set a
setUnions = foldl Set.union Set.empty

setIntersections :: Ord a => [Set.Set a] -> Set.Set a
setIntersections [] = Set.empty
setIntersections (x:xs) = foldl Set.intersection x xs

