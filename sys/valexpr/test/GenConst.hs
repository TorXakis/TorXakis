{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module GenConst 
( GenConst (..)
, genConstBool
, genConstInt
, genConstString
)
where

import qualified Data.Text as T
import           Test.QuickCheck

import           Const

newtype GenConst = GenConst Const
  deriving (Eq, Ord, Show)

instance Arbitrary GenConst where
    arbitrary = oneof [ genConstBool
                      , genConstInt
                      , genConstString
                      ]
    
genConstBool :: Gen GenConst
genConstBool = fmap (GenConst . Cbool) arbitrary

genConstInt :: Gen GenConst
genConstInt = fmap (GenConst . Cint) arbitrary

genConstString :: Gen GenConst
genConstString = fmap (GenConst . Cstring . T.pack . getASCIIString) (arbitrary :: Gen ASCIIString)