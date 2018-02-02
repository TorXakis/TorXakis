{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings      #-}
module TestConstructorDefs
(
testConstructorList
)
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports

-- generic Haskell imports
import qualified Data.Map  as Map
import           Data.Monoid
import qualified Data.Text as T

-- generic TorXakis imports
import Ref
import SortInternal
-- ----------------------------------------------------------------------------
testConstructorList :: Test
testConstructorList =
    TestList [ TestLabel "Single ConstructorDef" testCDefSingle
             , TestLabel "Multiple ConstructorDefs" testCDefMultiple
             , TestLabel "ConstructorDefs with non-unique references" testNonUniqueRef
             , TestLabel "ConstructorDef with empty name" testEmptyName
             , TestLabel "ConstructorDefs with same name" testNonUniqueName
             , TestLabel "ConstructorDefs with same field name" testNonUniqueFieldName
             ]

---------------------------------------------------------------------------
-- Success cases
---------------------------------------------------------------------------
testCDefSingle :: Test
testCDefSingle = TestCase $ do
    let cList = [cstrTuple 1]
    assertEqual "constructorDefs should succeed for single constructorDef"
        (Right $ ConstructorDefs $ Map.fromList cList)
        $ constructorDefs cList

testCDefMultiple :: Test
testCDefMultiple = TestCase $ do
    let cList = map cstrTuple [1,2,3]
    assertEqual "constructorDefs should succeed for single constructorDef"
        (Right $ ConstructorDefs $ Map.fromList cList)
        $ constructorDefs cList

---------------------------------------------------------------------------
-- Pre-condition violation tests
---------------------------------------------------------------------------
testNonUniqueRef :: Test
testNonUniqueRef = TestCase $ do
    let cstrList = [cstrTuple 1, cstrTuple 1]
    assertEqual "constructorDefs should fail for non-unique references"
        (Left $ RefsNotUnique cstrList)
        $ constructorDefs cstrList

testEmptyName :: Test
testEmptyName = TestCase $ do
    let cstrList = [(Ref 1, ConstructorDef T.empty $ FieldDefs [] 0)]
    assertEqual "constructorDefs should fail for empty constructor name"
        (Left $ EmptyName [Ref 1])
        $ constructorDefs cstrList

testNonUniqueName :: Test
testNonUniqueName = TestCase $ do
    let cDef = ConstructorDef (T.pack "SameName") $ FieldDefs [] 0
        cstrList = [(Ref 1, cDef),(Ref 2, cDef)]
    assertEqual "constructorDefs should fail for non-unique names"
        (Left $ NamesNotUnique cstrList)
        $ constructorDefs cstrList 


testNonUniqueFieldName :: Test
testNonUniqueFieldName = TestCase $ do
    let (cRef2,cDef2) = cstrTuple 2
        fields2 = fields cDef2
        cDef2' = cDef2 { fields =
                            fields2 {
                                fDefsToList =
                                    [( Ref 1
                                    , FieldDef {fieldName = "field1", sort = SortInt}
                                    )]
                                    }
                       }
        cstrList = [cstrTuple 1,(cRef2, cDef2')]
    assertEqual "constructorDefs should fail for non-unique field names"
        (Left $ SameFieldMultipleCstr ["field1"])
        $ constructorDefs cstrList

-- Helpers
cstrTuple :: Int -> (Ref ConstructorDef, ConstructorDef)
cstrTuple i = (cRef, cDef)
    where
        cRef = Ref i
        cDef = ConstructorDef { constructorName = "c" <> T.pack (show i), fields = fDefs }
        Right fDefs = fieldDefs [(Ref 1, fieldInt)]
        fieldInt = FieldDef { fieldName = "field" <> T.pack (show i), sort = SortInt }
