{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module ConstantSpec where

import qualified Data.Text as T
import           Test.Hspec
import           Test.QuickCheck

import           Constant
import           ConstantGen
import           SortId
import           SortOf

prop_BoolConst :: Bool -> Bool
prop_BoolConst b = b == toBool (Cbool b)

prop_BoolSort :: Bool -> Bool
prop_BoolSort b = sortIdBool == sortOf (Cbool b)

prop_IntConst :: Integer -> Bool
prop_IntConst i = i == Constant.toInteger (Cint i)

prop_IntSort :: Integer -> Bool
prop_IntSort i = sortIdInt == sortOf (Cint i)

prop_StringConst :: UnicodeString -> Bool
prop_StringConst (UnicodeString s) = let t = T.pack s in
                                        t == toText (Cstring t)

prop_StringSort :: UnicodeString -> Bool
prop_StringSort (UnicodeString s) = sortIdString == sortOf (Cstring (T.pack s))

prop_ConstEq :: ConstantGen -> Bool
prop_ConstEq (ConstantGen val) =
    not (val /= val)

prop_ConstOrd :: ConstantGen -> Bool
prop_ConstOrd (ConstantGen val) =
    val >= val

prop_ConstShow :: ConstantGen -> Bool
prop_ConstShow (ConstantGen val) =
    show [val] == show [val]

spec :: Spec
spec = do
  describe "Boolean Const" $ do
    it "Stores a Boolean" $ property prop_BoolConst
    it "Has sort Boolean"$ property prop_BoolSort
  describe "Int Const" $ do
    it "Stores a Int" $ property prop_IntConst
    it "Has sort Int"$ property prop_IntSort
  describe "String Const" $ do
    it "Stores a String" $ property prop_StringConst
    it "Has sort String"$ property prop_StringSort
  describe "A Const"$ do
    it "derives Eq" $ property prop_ConstEq
    it "derives Ord" $ property prop_ConstOrd
    it "derives Show" $ property prop_ConstShow