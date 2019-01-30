{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.GenCollection
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- GenCollection - collection of generators
-- Generators can be selected based on sort and complity/size.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.GenCollection
(-- * GenCollection
  GenCollection
, empty
, add
, get
)
where

import qualified Data.HashMap        as HashMap
import           Data.Maybe
import qualified Data.MultiMap       as MultiMap
import qualified Data.Text           as T
import           Test.QuickCheck

import           TorXakis.Error
import           TorXakis.Sort (Sort, SortContext, elemSort)

-- | GenCollection - stores generators
newtype GenCollection c a = GenCollection ( HashMap.Map Sort ( MultiMap.MultiMap Int (c -> Gen a) ) )

-- | empty GenCollection
empty :: GenCollection c a
empty = GenCollection HashMap.empty

-- | Add Generator to GenCollection
-- TODO: do we need the check 
add :: SortContext c => c -> Sort -> Int -> (c -> Gen a) -> GenCollection c a -> Either MinError (GenCollection c a)
add ctx s n g (GenCollection c) | elemSort ctx s = let mm = HashMap.findWithDefault MultiMap.empty s c
                                                       newMM = MultiMap.insert n g mm
                                                     in
                                                       Right $ GenCollection (HashMap.insert s newMM c)
                                | otherwise      = Left $ MinError (T.pack ("Add: Sort " ++ show s ++ " not defined in context."))

-- | Get generators of given sort and size/complexity less than or equal to given value
get :: GenCollection c a -> Sort -> Int -> [c -> Gen a]
get (GenCollection c) s n = case HashMap.lookup s c of
                                Nothing -> []
                                Just mm -> mapMaybe (\(k,v) -> if k <= n then Just v else Nothing) (MultiMap.toList mm)
