{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module TorXakis.Sort.SortIdGen 
( SortIdGen (..)
)
where

import           Test.QuickCheck

import           TorXakis.Sort
import           TorXakis.Sort.IdGen
import           TorXakis.Sort.NameGen

newtype SortIdGen = SortIdGen SortId
  deriving (Eq, Ord, Show)

instance Arbitrary SortIdGen where
    arbitrary = do
                    NameGen name <- arbitrary
                    IdGen   id'  <- arbitrary
                    return $ SortIdGen (SortId name id')