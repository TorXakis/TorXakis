{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module TorXakis.Sort.NameGen 
( NameGen (..)
)
where

import qualified Data.Text as T
import           Test.QuickCheck

import           TorXakis.Sort

newtype NameGen = NameGen Name
  deriving (Eq, Ord, Show)

instance Arbitrary NameGen where
    arbitrary = fmap (NameGen . T.pack . getASCIIString) (arbitrary :: Gen ASCIIString)