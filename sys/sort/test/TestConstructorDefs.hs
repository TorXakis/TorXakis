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
            -- , TestLabel "Adding multiple ADT" testAddADTMultiple
            -- , TestLabel "Adding ADTs with already defined ref" testAddADTAlreadyDefinedRef
            -- , TestLabel "Adding ADTs with non-unique ref" testAddADTNonUniqueRef
            -- , TestLabel "Adding ADTs with unknown ref" testAddADTUnknownRef
            -- , TestLabel "Adding ADT with empty name" testAddADTEmptyName
            -- , TestLabel "Adding ADTs with already defined name" testAddADTAlreadyDefinedName
            -- , TestLabel "Adding ADTs with non-unique name" testAddADTNonUniqueName
            -- , TestLabel "Constructable ADTs" testConstructableADTs
            -- , TestLabel "Non-Constructable ADTs" testNonConstructableADTs
            -- , TestLabel "ADT without constructor" testADTWithoutConstructor
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
{-
---------------------------------------------------------------------------
-- ADT Reference conditions
---------------------------------------------------------------------------
testAddADTNonUniqueRef :: Test
testAddADTNonUniqueRef = TestCase $ do
    let adtTuple = (Ref 1, ADTDef T.empty $ ConstructorDefs Map.empty)
        newADTList = [adtTuple, adtTuple]
    assertEqual "addADTDefs should fail for non-unique references"
        (Left $ refsNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList emptyADTDefs

testAddADTAlreadyDefinedRef :: Test
testAddADTAlreadyDefinedRef = TestCase $ do
    let newADTList = [(Ref 1, ADTDef T.empty $ ConstructorDefs Map.empty)]
        existingADTs = ADTDefs $ Map.fromList newADTList
    assertEqual "addADTDefs should fail for already defined references"
        (Left $ refsNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList existingADTs

testAddADTUnknownRef :: Test
testAddADTUnknownRef = TestCase $ do
    let newADTList = [(adtARef, adtA)]
    assertEqual "addADTDefs should fail for unknown references"
        (Left $ refNotFoundError [([adtBRef],(adtARef,adtA))])
        $ addADTDefs newADTList emptyADTDefs

---------------------------------------------------------------------------
-- ADT Name conditions
---------------------------------------------------------------------------
testAddADTEmptyName :: Test
testAddADTEmptyName = TestCase $ do
    let newADTList = [(Ref 1, ADTDef T.empty $ ConstructorDefs Map.empty)]
    assertEqual "addADTDefs should fail for empty ADT name"
        (Left $ emptyADTNamePrefix <> T.pack (show [Ref 1]))
        $ addADTDefs newADTList emptyADTDefs

testAddADTNonUniqueName :: Test
testAddADTNonUniqueName = TestCase $ do
    let adt = ADTDef (T.pack "SameName") $ ConstructorDefs Map.empty
        newADTList = [(Ref 1, adt),(Ref 2, adt)]
    assertEqual "addADTDefs should fail for already defined names"
        (Left $ namesNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList emptyADTDefs

testAddADTAlreadyDefinedName :: Test
testAddADTAlreadyDefinedName = TestCase $ do
    let adt = ADTDef (T.pack "SameName") $ ConstructorDefs Map.empty
        newADTList = [(Ref 1, adt)]
        existingADTs = ADTDefs $ Map.fromList [(Ref 2, adt)]
    assertEqual "addADTDefs should fail for non-unique names"
        (Left $ namesNotUniquePrefix <> T.pack (show newADTList))
        $ addADTDefs newADTList existingADTs

---------------------------------------------------------------------------
-- Constructability
---------------------------------------------------------------------------
testConstructableADTs :: Test
testConstructableADTs = TestCase $ do
    let adtList = [(adtARef, adtA),(adtBRef, adtB),(adtCRef, adtC)]
    assertEqual "All data types should be constructable" (Map.fromList adtList,[])
        $ verifyConstructableADTs (Map.empty, adtList)

testNonConstructableADTs :: Test
testNonConstructableADTs = TestCase $ do
    let -- B { a :: A }
        adtB' = ADTDef { adtName = "B", constructors = cDefsB' }
        Right cDefsB' = constructorDefs [(Ref 1, cstrB1)]
        adtList = [(adtARef, adtA),(adtBRef, adtB'),(adtCRef, adtC)]
        expCADTs   = Map.fromList [(adtCRef, adtC)]
        expNCADTs  = Map.fromList [(adtBRef, adtB'),(adtARef, adtA)]
        (actCADTs,actNCADTs) = verifyConstructableADTs (Map.empty,adtList)
    assertEqual "Only C should be constructable" expCADTs actCADTs
    assertEqual "B' and A should be non-constructable" expNCADTs
        $ Map.fromList actNCADTs

testADTWithoutConstructor :: Test
testADTWithoutConstructor = TestCase $ do
    let adtList = [(Ref 1, ADTDef { adtName = "N", constructors = ConstructorDefs Map.empty })]
    assertEqual "ADT without constructor should be non-constructable"
        (Map.empty, adtList)
        $ verifyConstructableADTs (Map.empty, adtList)

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
-}

cstrTuple :: Int -> (Ref ConstructorDef, ConstructorDef)
cstrTuple i = (cRef, cDef)
    where
        cRef = Ref i
        cDef = ConstructorDef { constructorName = "c" <> T.pack (show i), fields = fDefs }
        Right fDefs = fieldDefs [(Ref 1, fieldInt)]
        fieldInt = FieldDef { fieldName = "field" <> T.pack (show i), sort = SortInt }
