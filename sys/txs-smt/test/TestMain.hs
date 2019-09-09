{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import Test.Hspec
import Test.Hspec.Contrib.HUnit

import TestConstraint
import TestPushPop

main :: IO ()
main = hspec $ do
  describe "constraint" $
    fromHUnitTest testConstraintList
  describe "push pop" $
    fromHUnitTest testPushPopList
