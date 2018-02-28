{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Test.FieldDefs
(
testFieldList
)
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports
import Test.CommonHelpers

-- generic Haskell imports

-- generic TorXakis imports
import Sort.Internal
-- ----------------------------------------------------------------------------
testFieldList :: Test
testFieldList =
    TestList [ TestLabel "Single FieldDef" testFDefSingle
             , TestLabel "Multiple FieldDef's" testFDefMultiple
             , TestLabel "No FieldDef's" testEmptyFieldList
             , TestLabel "FieldDef's with same name" testNonUniqueName
             ]
             
---------------------------------------------------------------------------
-- Success cases
---------------------------------------------------------------------------
testFDefSingle :: Test
testFDefSingle = TestCase $ do
    let fList = [mkIntField 1]
    assertEqual "fieldDefs should succeed for single FieldDef"
        (Right $ mkFieldDefs fList) (fieldDefs fList)

testFDefMultiple :: Test
testFDefMultiple = TestCase $ do
    let fList = map mkIntField [1,2,3]
    assertEqual "fieldDefs should succeed for multiple FieldDef's"
        (Right $ mkFieldDefs fList) (fieldDefs fList)

testEmptyFieldList :: Test
testEmptyFieldList = TestCase $
    assertEqual "fieldDefs should succeed for empty list"
        (Right $ mkFieldDefs []) (fieldDefs [])

---------------------------------------------------------------------------
-- Pre-condition violation tests
---------------------------------------------------------------------------
testNonUniqueName :: Test
testNonUniqueName = TestCase $ do
    let fDefInt = fieldNoMeta "SameName" "Int"
        fDefWhatEver = fieldNoMeta "SameName" "WhatEver"
        fldList = [fDefInt, fDefWhatEver]
    case fieldDefs fldList of
        Left (FieldNamesNotUnique fs) ->
                assertUnorderedEqual
                    "fieldDefs should fail for non-unique names"
                    fs fldList
        r   ->  assertFailure
                    $ "fieldDefs should fail for non-unique names but got '"
                    ++ show r ++ "'"
