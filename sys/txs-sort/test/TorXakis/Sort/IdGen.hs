{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module TorXakis.Sort.IdGen
( IdGen (..)
)
where

import           Test.QuickCheck
import           TorXakis.Sort

newtype IdGen = IdGen Id
  deriving (Eq, Ord, Show)

instance Arbitrary IdGen where
    arbitrary = fmap (IdGen . Id) (arbitrary :: Gen Int)