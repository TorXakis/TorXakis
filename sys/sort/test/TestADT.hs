{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings      #-}
module TestADT
(
testADTList
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
testADTList :: Test
testADTList  = TestList [ TestLabel "References" testRef
                        , TestLabel "Adding ADTs with already defined ref" testAddADTAlreadyDefinedRef
                        , TestLabel "Adding ADTs with non-unique ref" testAddADTNonUniqueRef
                        , TestLabel "Adding ADTs with already defined name" testAddADTAlreadyDefinedName
                        , TestLabel "Adding ADTs with non-unique name" testAddADTNonUniqueName
                        -- , TestLabel "Constructable ADTs" testADTAnalysisConstructable
                        -- , TestLabel "Non-Constructable ADTs" testADTAnalysisNonConstructable
                        , TestLabel "Constructable ADTs 2" testConstructableADTs
                        , TestLabel "Non-Constructable ADTs 2" testNonConstructableADTs
                        ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testRef :: Test
testRef = TestCase $ do
    let expected = 12
        rInt :: Ref Int
        rInt = Ref expected
    assertEqual "Same reference?" expected $ toInt rInt

testAddADTAlreadyDefinedRef :: Test
testAddADTAlreadyDefinedRef = TestCase $ do
    let newADTList = [(Ref 1, ADTDef T.empty $ ConstructorDefs Map.empty)]
        existingADTs = ADTDefs $ Map.fromList newADTList
    assertEqual "addADTDefs should fail for already defined references"
        (Left $ refsNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList existingADTs

testAddADTNonUniqueRef :: Test
testAddADTNonUniqueRef = TestCase $ do
    let adtTuple = (Ref 1, ADTDef T.empty $ ConstructorDefs Map.empty)
        newADTList = [adtTuple, adtTuple]
        existingADTs = emptyADTDefs
    assertEqual "addADTDefs should fail for non-unique references"
        (Left $ refsNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList existingADTs

testAddADTNonUniqueName :: Test
testAddADTNonUniqueName = TestCase $ do
    let adt = ADTDef (T.pack "SameName") $ ConstructorDefs Map.empty
        newADTList = [(Ref 1, adt),(Ref 2, adt)]
        existingADTs = emptyADTDefs
    assertEqual "addADTDefs should fail for already defined names"
        (Left $ namesNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList existingADTs

testAddADTAlreadyDefinedName :: Test
testAddADTAlreadyDefinedName = TestCase $ do
    let adt = ADTDef (T.pack "SameName") $ ConstructorDefs Map.empty
        newADTList = [(Ref 1, adt)]
        existingADTs = ADTDefs $ Map.fromList [(Ref 2, adt)]
    assertEqual "addADTDefs should fail for non-unique names"
        (Left $ namesNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList existingADTs

testConstructableADTs :: Test
testConstructableADTs = TestCase $ do
    let adtList = [(adtARef, adtA),(adtBRef, adtB),(adtCRef, adtC)]
    assertEqual "All data types should be constructable" (Map.fromList adtList,[])
        $ verifyConstructableADTs (Map.empty, adtList)

testNonConstructableADTs :: Test
testNonConstructableADTs = TestCase $ do
    let -- B { a :: A }
        adtBRef' = Ref 4
        adtB' = ADTDef { adtName = "B", constructors = cDefsB' }
        Right cDefsB' = constructorDefs [(Ref 1, cstrB1)]
        adtList = [(adtARef, adtA),(adtBRef', adtB'),(adtCRef, adtC)]
        cADTs   = Map.fromList [(adtCRef, adtC)]
        ncADTs   = [(adtBRef', adtB'),(adtARef, adtA)]
    assertEqual "Only C should be constructable" (cADTs,ncADTs)
        $ verifyConstructableADTs (Map.empty,adtList)

-- testADTAnalysisConstructable :: Test
-- testADTAnalysisConstructable = TestCase $ do
--     let adtList = [(adtARef, adtA),(adtBRef, adtB),(adtCRef, adtC)]
--         cADTs   = Map.fromList adtList
--     assertEqual "All data types should be constructable" (cADTs,[])
--         $ analyzeADTs (adtDefsToMap emptyADTDefs,[]) adtList

-- testADTAnalysisNonConstructable :: Test
-- testADTAnalysisNonConstructable = TestCase $ do
--     let -- B { a :: A }
--         adtBRef' = Ref 4
--         adtB' = ADTDef { adtName = "B", constructors = cDefsB' }
--         Right cDefsB' = constructorDefs [(Ref 1, cstrB1)]
--         adtList' = [(adtARef, adtA),(adtBRef', adtB'),(adtCRef, adtC)]
--         cADTs'   = Map.fromList [(adtCRef, adtC)]
--         ncADTs   = [(adtBRef', adtB'),(adtARef, adtA)]
--     assertEqual "Only C should be constructable" (cADTs',ncADTs)
--         $ analyzeADTs (adtDefsToMap emptyADTDefs,[]) adtList'

-- Test Data

-- A { b :: B }
adtARef :: Ref ADTDef
adtARef = Ref 1

adtA :: ADTDef
adtA = ADTDef { adtName = "A", constructors = cDefsA }
       where Right cDefsA = constructorDefs [(Ref 1, cstrA)]
             cstrA = ConstructorDef { constructorName = "cstrA", fields = fDefsA }
             Right fDefsA = fieldDefs [(Ref 1, fieldB)]
             fieldB = FieldDef { fieldName = "fieldB", sort = SortADT adtBRef }
-- B { a :: A } | B { c :: C }
adtBRef :: Ref ADTDef
adtBRef = Ref 2

adtB :: ADTDef
adtB = ADTDef { adtName = "B", constructors = cDefsB }
       where Right cDefsB = constructorDefs [(Ref 1, cstrB1), (Ref 2, cstrB2)]
-- B { a :: A }
cstrB1 :: ConstructorDef
cstrB1 = ConstructorDef { constructorName = "cstrB1", fields = fDefsB1 }
         where Right fDefsB1 = fieldDefs [(Ref 1, fieldA)]
               fieldA = FieldDef { fieldName = "fieldA", sort = SortADT adtARef }
-- B { c :: C }
cstrB2 :: ConstructorDef
cstrB2 = ConstructorDef { constructorName = "cstrB2", fields = fDefsB2 }
         where Right fDefsB2 = fieldDefs [(Ref 1, fieldC)]
               fieldC = FieldDef { fieldName = "fieldC", sort = SortADT adtCRef }
-- C { i :: Int }
adtCRef :: Ref ADTDef
adtCRef = Ref 3

adtC :: ADTDef
adtC = ADTDef { adtName = "C", constructors = cDefsC }
       where Right cDefsC = constructorDefs [(Ref 1, cstrC)]
             cstrC = ConstructorDef { constructorName = "cstrC", fields = fDefsC }
             Right fDefsC = fieldDefs [(Ref 1, fieldInt)]
             fieldInt = FieldDef { fieldName = "fieldInt", sort = SortInt }
