{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module ConstSpec where

import           Control.Lens
import           Data.Maybe
import qualified Data.Text as T
import           Test.Hspec
import           Test.QuickCheck

import           Const
import           GenConst
import           SortId
import           SortOf

prop_BoolConst :: Bool -> Bool
prop_BoolConst b = b == (Cbool b) ^. toBool

prop_BoolSort :: Bool -> Bool
prop_BoolSort b = sortIdBool == sortOf (Cbool b)

prop_IntConst :: Integer -> Bool
prop_IntConst i = i == (Cint i) ^. Const.toInteger

prop_IntSort :: Integer -> Bool
prop_IntSort i = sortIdInt == sortOf (Cint i)

prop_StringConst :: UnicodeString -> Bool
prop_StringConst (UnicodeString s) = (T.pack s) == (Cstring (T.pack s)) ^. toText

prop_StringSort :: UnicodeString -> Bool
prop_StringSort (UnicodeString s) = sortIdString == sortOf (Cstring (T.pack s))

prop_ConstEq :: GenConst -> Bool
prop_ConstEq (GenConst val) =
    not (val /= val)

prop_ConstOrd :: GenConst -> Bool
prop_ConstOrd (GenConst val) =
    val >= val

prop_ConstShow :: GenConst -> Bool
prop_ConstShow (GenConst val) =
    show [val] == show [val]

spec :: Spec
spec = do
  describe "Boolean Const" $ do
 --   it "Stores a Boolean" $ property prop_BoolConst
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