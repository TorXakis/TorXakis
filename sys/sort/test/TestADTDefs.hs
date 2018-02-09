{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings      #-}
module TestADTDefs
(
testADTList
)
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports

-- generic Haskell imports
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as T

-- generic TorXakis imports
import Name
import Ref
import SortInternal
-- ----------------------------------------------------------------------------
testADTList :: Test
testADTList  = TestList [ TestLabel "References" testRef
                        , TestLabel "Adding single ADT" testAddADTSingle
                        , TestLabel "Adding multiple ADT" testAddADTMultiple
                        , TestLabel "Adding ADTs with unknown ref" testAddADTUnknownRef
                        , TestLabel "Adding ADTs with already defined name" testAddADTAlreadyDefinedName
                        , TestLabel "Adding ADTs with non-unique name" testAddADTNonUniqueName
                        , TestLabel "Constructable ADTs" testConstructableADTs
                        , TestLabel "Non-Constructable ADTs" testNonConstructableADTs
                        , TestLabel "ADT without constructor" testADTWithoutConstructor
                        ]

---------------------------------------------------------------------------
-- Success cases
---------------------------------------------------------------------------
testRef :: Test
testRef = TestCase $ do
    let expected = T.pack "12"
        rInt :: Ref Int
        rInt = Ref expected
    assertEqual "Same reference?" expected $ Ref.toText rInt

testAddADTSingle :: Test
testAddADTSingle = TestCase $ do
    let newADTList = [adtC]
    assertEqual "addADTDefs should succeed for single ADT"
        (Right $ mkADTDefs newADTList)
        $ addADTDefs newADTList emptyADTDefs

testAddADTMultiple :: Test
testAddADTMultiple = TestCase $ do
    let newADTList = [adtC, adtB']
        adtB' = ADTDef { adtName = "B", constructors = cDefsB' }
        cDefsB' = mkConstructorDefs [cstrB2]
    assertEqual "addADTDefs should succeed for multiple ADTs"
        (Right $ mkADTDefs newADTList)
        $ addADTDefs newADTList emptyADTDefs

---------------------------------------------------------------------------
-- ADT Reference conditions
---------------------------------------------------------------------------
testAddADTUnknownRef :: Test
testAddADTUnknownRef = TestCase $ do
    let newADTList = [adtA]
    assertEqual "addADTDefs should fail for unknown references"
        (Left $ RefsNotFound [([adtBRef],adtA)])
        $ addADTDefs newADTList emptyADTDefs

---------------------------------------------------------------------------
-- ADT Name conditions
---------------------------------------------------------------------------
testAddADTNonUniqueName :: Test
testAddADTNonUniqueName = TestCase $ do
    let adt = ADTDef "SameName" $ ConstructorDefs Map.empty
        newADTList = [adt, adt]
    assertEqual "addADTDefs should fail for non-unique names"
        (Left $ NamesNotUnique newADTList)
        $ addADTDefs newADTList emptyADTDefs

testAddADTAlreadyDefinedName :: Test
testAddADTAlreadyDefinedName = TestCase $ do
    let adt = ADTDef "SameName" $ ConstructorDefs Map.empty
        newADTList = [adt]
        existingADTs = mkADTDefs [adt]
    assertEqual "addADTDefs should fail for already defined names"
        (Left $ NamesNotUnique newADTList)
        $ addADTDefs newADTList existingADTs

---------------------------------------------------------------------------
-- Constructability
---------------------------------------------------------------------------
testConstructableADTs :: Test
testConstructableADTs = TestCase $ do
    let adtList = [adtA, adtB, adtC]
    assertEqual "All data types should be constructable" (adtList,[])
        $ verifyConstructableADTs ([], adtList)

testNonConstructableADTs :: Test
testNonConstructableADTs = TestCase $ do
    let -- B { a :: A }
        adtB' = ADTDef { adtName = "B", constructors = cDefsB' }
        cDefsB' = mkConstructorDefs [cstrB1]
        adtList = [adtA,adtB',adtC]
        expCADTs   = Set.fromList [adtC]
        expNCADTs  = Set.fromList [adtB', adtA]
        (actCADTs,actNCADTs) = verifyConstructableADTs ([], adtList)
    assertEqual "Only C should be constructable" expCADTs $ Set.fromList actCADTs
    assertEqual
        "B' and A should be non-constructable" expNCADTs $ Set.fromList actNCADTs

testADTWithoutConstructor :: Test
testADTWithoutConstructor = TestCase $ do
    let adtList = [ADTDef { adtName = "N", constructors = ConstructorDefs Map.empty }]
    assertEqual "ADT without constructor should be non-constructable"
        ([], adtList)
        $ verifyConstructableADTs ([], adtList)

---------------------------------------------------------------------------
-- Test Data
---------------------------------------------------------------------------
-- A { b :: B }
adtARef :: Ref ADTDef
adtARef = Ref $ T.pack "A"

adtA :: ADTDef
adtA = ADTDef { adtName = "A", constructors = cDefsA }
       where cDefsA = mkConstructorDefs [cstrA]
             cstrA = ConstructorDef { constructorName = "cstrA", fields = fDefsA }
             fDefsA = mkFieldDefs [fieldB]
             fieldB = FieldDef { fieldName = "fieldB", sort = SortADT adtBRef }
-- B { a :: A } | B { c :: C }
adtBRef :: Ref ADTDef
adtBRef = Ref $ T.pack "B"

adtB :: ADTDef
adtB = ADTDef { adtName = "B", constructors = cDefsB }
       where cDefsB = mkConstructorDefs [cstrB1, cstrB2]
-- B { a :: A }
cstrB1 :: ConstructorDef
cstrB1 = ConstructorDef { constructorName = "cstrB1", fields = fDefsB1 }
         where fDefsB1 = mkFieldDefs [fieldA]
               fieldA = FieldDef { fieldName = "fieldA", sort = SortADT adtARef }
-- B { c :: C }
cstrB2 :: ConstructorDef
cstrB2 = ConstructorDef { constructorName = "cstrB2", fields = fDefsB2 }
         where fDefsB2 = mkFieldDefs [fieldC]
               fieldC = FieldDef { fieldName = "fieldC", sort = SortADT adtCRef }
-- C { i :: Int }
adtCRef :: Ref ADTDef
adtCRef = Ref $ T.pack "C"

adtC :: ADTDef
adtC = ADTDef { adtName = "C", constructors = cDefsC }
       where cDefsC = mkConstructorDefs [cstrC]
             cstrC = ConstructorDef { constructorName = "cstrC", fields = fDefsC }
             fDefsC = mkFieldDefs [fieldInt]
             fieldInt = FieldDef { fieldName = "fieldInt", sort = SortInt }

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
mkADTDefs :: [ADTDef] -> ADTDefs
mkADTDefs = ADTDefs . Map.fromList . map (\a -> (Ref $ Name.toText $ adtName a, a))

mkConstructorDefs :: [ConstructorDef] -> ConstructorDefs
mkConstructorDefs = ConstructorDefs . Map.fromList . map (\c -> (Ref $ Name.toText $ constructorName c, c))

mkFieldDefs :: [FieldDef] -> FieldDefs
mkFieldDefs fs = FieldDefs fs $ length fs
