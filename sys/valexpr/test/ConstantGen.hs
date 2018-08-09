{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module ConstantGen 
( ConstantGen (..)
, constantBoolGen
, constantIntGen
, constantStringGen
)
where

import qualified Data.Text as T
import           Test.QuickCheck

import           Constant

newtype ConstantGen = ConstantGen Constant
  deriving (Eq, Ord, Show)

instance Arbitrary ConstantGen where
    arbitrary = oneof [ constantBoolGen
                      , constantIntGen
                      , constantStringGen
                      ]
    
constantBoolGen :: Gen ConstantGen
constantBoolGen = fmap (ConstantGen . Cbool) arbitrary

constantIntGen :: Gen ConstantGen
constantIntGen = fmap (ConstantGen . Cint) arbitrary

constantStringGen :: Gen ConstantGen
constantStringGen = fmap (ConstantGen . Cstring . T.pack . getASCIIString) (arbitrary :: Gen ASCIIString)