{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings      #-}
module Test.ConstructorDefs
(
testConstructorList
)
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports
import Test.CommonHelpers

-- generic Haskell imports
import qualified Data.HashMap.Strict as Map

-- generic TorXakis imports
import Name
import Ref
import Sort.ConstructorDefs
-- ----------------------------------------------------------------------------
testConstructorList :: Test
testConstructorList =
    TestList [ TestLabel "Single ConstructorDef" testCDefSingle
             , TestLabel "Multiple ConstructorDef's" testCDefMultiple
             , TestLabel "No ConstructorDef's" testEmptyConstructorList
             , TestLabel "ConstructorDef's with same name" testNonUniqueName
             , TestLabel "ConstructorDef's with same field name" testNonUniqueFieldName
             ]

---------------------------------------------------------------------------
-- Success cases
---------------------------------------------------------------------------
testCDefSingle :: Test
testCDefSingle = TestCase $ do
    let cList = [cstr1]
        cstr1 = cstr 1
        expCDefs = ConstructorDefs $ Map.fromList [(RefByName $ constructorName cstr1, cstr1)]
    assertEqual "constructorDefs should succeed for single ConstructorDef"
        (Right expCDefs) (constructorDefs cList)

testCDefMultiple :: Test
testCDefMultiple = TestCase $ do
    let cList = [cstr1, cstr2, cstr3]
        cstr1 = cstr 1
        cstr2 = cstr 2
        cstr3 = cstr 3
        expCDefs = ConstructorDefs $ Map.fromList [(RefByName $ constructorName cstr1, cstr1)
                                                  ,(RefByName $ constructorName cstr2, cstr2)
                                                  ,(RefByName $ constructorName cstr3, cstr3)
                                                  ]
    assertEqual "constructorDefs should succeed for multiple ConstructorDef's"
        (Right expCDefs) (constructorDefs cList)

---------------------------------------------------------------------------
-- Pre-condition violation tests
---------------------------------------------------------------------------
testEmptyConstructorList :: Test
testEmptyConstructorList = TestCase $
    assertEqual "constructorDefs should fail for empty list"
        (Left EmptyConstructorDefs) (constructorDefs ([] :: [ConstructorDef Name]))

-- These conditions are evaluated before 'FieldDefs'' conditions. That's
-- why test entities have empty 'FieldDefs'.
testNonUniqueName :: Test
testNonUniqueName = TestCase $ do
    let Right cName = name "SameName"
        cDef = (ConstructorDef cName $ mkFieldDefs []) :: ConstructorDef Name
        cstrList = [cDef, cDef]
    assertEqual "constructorDefs should fail for non-unique names"
        (Left $ ConstructorNamesNotUnique cstrList)
        (constructorDefs cstrList)

testNonUniqueFieldName :: Test
testNonUniqueFieldName = TestCase $ do
    let cstrList = [cstr1, cDef2']
        cstr1 = cstr 1
        cDef2' = ConstructorDef (mkName "c2") fDefs
        fDefs = mkFieldDefs [field1WhatEver]
        field1WhatEver = fieldNoMeta "field1" "WhatEver"
        field1Int = fieldNoMeta "field1" "Int"
    case constructorDefs cstrList of
        Left (SameFieldMultipleCstr fs) ->
                assertUnorderedEqual
                    "constructorDefs should fail for non-unique field names"
                    fs [field1WhatEver, field1Int]
        r   ->  assertFailure $ "constructorDefs should fail but got '"
                    ++ show r ++ "'"

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
cstr :: Int -> ConstructorDef Name
cstr i = cDef
    where
        cDef = ConstructorDef (mkName $ "c" ++ show i) fDefs
        fDefs = mkFieldDefs [fieldInt]
        fieldInt = mkIntField i
