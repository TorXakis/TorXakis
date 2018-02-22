{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module TestADTDefs
(
testADTList
)
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports

-- generic Haskell imports
import qualified Data.HashMap.Strict as Map
-- import qualified Data.Set as Set
import           Data.String (IsString (..))
import qualified Data.Text as T

-- generic TorXakis imports
import Name
import Ref
import Sort.ADTDefs
import Sort.ConstructorDefs
import Sort.ConvertsTo
import Sort.FieldDefs
-- ----------------------------------------------------------------------------
testADTList :: Test
testADTList  = TestList [ TestLabel "Adding single ADT" testAddADTSingle
                        , TestLabel "Adding ADT that depends on other ADT" testAddADTDependent
                        , TestLabel "Adding multiple ADTs" testAddADTMultiple
                        , TestLabel "Adding ADTs with unknown ref" testAddADTUnknownRef
                        -- , TestLabel "Adding ADTs with already defined name" testAddADTAlreadyDefinedName
                        -- , TestLabel "Adding ADTs with non-unique name" testAddADTNonUniqueName
                        -- , TestLabel "Constructable ADTs" testConstructableADTs
                        -- , TestLabel "Non-Constructable ADTs" testNonConstructableADTs
                        -- , TestLabel "ADT without constructor" testADTWithoutConstructor
                        ]

---------------------------------------------------------------------------
-- Success cases
---------------------------------------------------------------------------
testAddADTSingle :: Test
testAddADTSingle = TestCase $ do
    let adtList    = [adtCName]
        expADTDefs = ADTDefs $ Map.fromList [(RefByName $ adtName adtCSort, adtCSort)]
    assertEqual "addADTDefs should succeed for single ADT"
        (Right expADTDefs)
        $ addADTDefs adtList emptyADTDefs

testAddADTDependent :: Test
testAddADTDependent = TestCase $ do
    let newADTList = [adtCName, adtB' "C"]
        adtB' t = ADTDef "B" $ cDefsB' t
        cDefsB' t = ConstructorDefs $ Map.fromList [(RefByName $ constructorName $ cstrB2 t, cstrB2 t)]
        expADTDefs = ADTDefs $ Map.fromList [(RefByName $ adtName  adtCSort,  adtCSort)
                                            ,(RefByName $ adtName adtB'Sort, adtB'Sort)]
        adtB'Sort = adtB' $ SortADT $ RefByName "C"
    assertEqual "addADTDefs should succeed for dependent ADTs"
        (Right expADTDefs)
        $ addADTDefs newADTList emptyADTDefs

testAddADTMultiple :: Test
testAddADTMultiple = TestCase $ do
    let newADTList = [adtAName, adtBName, adtCName]
        expADTDefs = ADTDefs $ Map.fromList [(RefByName $ adtName adtCSort, adtCSort)
                                            ,(RefByName $ adtName adtBSort, adtBSort)
                                            ,(RefByName $ adtName adtASort, adtASort)]
    assertEqual "addADTDefs should succeed for multiple ADTs"
        (Right expADTDefs)
        $ addADTDefs newADTList emptyADTDefs

---------------------------------------------------------------------------
-- ADT Reference conditions
---------------------------------------------------------------------------
testAddADTUnknownRef :: Test
testAddADTUnknownRef = TestCase $ do
    let newADTList = [adtAName]
    assertEqual "addADTDefs should fail for unknown references"
        (Left $ RefsNotFound [(["B"], adtAName)])
        $ addADTDefs newADTList emptyADTDefs

---------------------------------------------------------------------------
-- ADT Name conditions
---------------------------------------------------------------------------
{-testAddADTNonUniqueName :: Test
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
    let adtList = [adtAName, adtBName, adtCName]
        (actConstructableSorts,actNCADTs) = verifyConstructableADTs ([], adtList)
    assertEqual "All data types should be constructable" (Set.fromList $ map (SortADT . Ref) ["A","B","C"], [])
        (Set.fromList actConstructableSorts,actNCADTs)

testNonConstructableADTs :: Test
testNonConstructableADTs = TestCase $ do
    let -- B { a :: A }
        adtB' = ADTDef "B" cDefsB' }
        cDefsB' = ConstructorDefs $ Map.fromList [(RefByName $ constructorName cstrB1 $ nameOf $ SortADT $ RefByName "B"]
        adtList = [adtAName, adtB', adtCName]
        expCADTSorts = Set.fromList [SortADT $ RefByName "C"]
        expNCADTs    = Set.fromList [adtB', adtAName]
        (actCADTSorts,actNCADTs) = verifyConstructableADTs ([], adtList)
    assertEqual "Only C should be constructable" expCADTSorts $ Set.fromList actCADTSorts
    assertEqual
        "B' and A should be non-constructable" expNCADTs $ Set.fromList actNCADTs

testADTWithoutConstructor :: Test
testADTWithoutConstructor = TestCase $ do
    let adtList = [ADTDef "N" ConstructorDefs Map.empty }]
        (actCADTSorts,actNCADTs) = verifyConstructableADTs ([], adtList)
    assertEqual "ADT without constructor should be non-constructable"
        ([], Set.fromList adtList) (actCADTSorts, Set.fromList actNCADTs)
-}
---------------------------------------------------------------------------
-- Test Data
---------------------------------------------------------------------------
-- A { b :: B }
adtAName :: ADTDef Name
adtAName = adtA "B"

adtASort :: ADTDef Sort
adtASort = adtA $ SortADT $ RefByName "B"

adtA :: ConvertsTo v v => v -> ADTDef v
adtA s = ADTDef "A" cDefsA
         where cDefsA = ConstructorDefs $ Map.fromList [(RefByName $ constructorName cstrA, cstrA)]
               cstrA = ConstructorDef "cstrA" fDefsA
               fDefsA = mkFieldDefs [fieldB]
               fieldB = FieldDef "fieldB" s
-- B { a :: A } | B { c :: C }
adtBName :: ADTDef Name
adtBName = adtB "A" "Int"

adtBSort :: ADTDef Sort
adtBSort = adtB (SortADT $ RefByName "A") SortInt

adtB :: ConvertsTo v v => v -> v -> ADTDef v
adtB s1 s2 = ADTDef "B" cDefsB
         where
            cDefsB = ConstructorDefs $ Map.fromList [(RefByName $ constructorName cstrB1s1, cstrB1s1),
                                                     (RefByName $ constructorName cstrB2s2, cstrB2s2)]
            cstrB1s1 = cstrB1 s1
            cstrB2s2 = cstrB2 s2

-- B { a :: A }
cstrB1 :: v -> ConstructorDef v
cstrB1 s = ConstructorDef "cstrB1" fDefsB1
           where fDefsB1 = mkFieldDefs [fieldA]
                 fieldA = FieldDef "fieldA" s
-- B { c :: C }
cstrB2 :: v -> ConstructorDef v
cstrB2 s = ConstructorDef "cstrB2" fDefsB2
           where fDefsB2 = mkFieldDefs [fieldC]
                 fieldC = FieldDef "fieldC" s
-- C { i :: Int }
adtCName :: ADTDef Name
adtCName = adtC "Int"

adtCSort :: ADTDef Sort
adtCSort = adtC SortInt

adtC :: ConvertsTo v v => v -> ADTDef v
adtC s = ADTDef "C" cDefsC
       where cDefsC = ConstructorDefs $ Map.fromList [(RefByName $ constructorName cstrC, cstrC)]
             cstrC = ConstructorDef "cstrC" fDefsC
             fDefsC = mkFieldDefs [fieldInt]
             fieldInt = FieldDef "fieldInt" s

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
-- mkADTDefs :: [ADTDef Name] -> ADTDefs
-- mkADTDefs = ADTDefs . convertTo -- Map.fromList . map (\a -> (Ref $ adtName a, a))

-- mkConstructorDefs :: ConvertsTo v v => [ConstructorDef v] -> ConstructorDefs v
-- mkConstructorDefs = ConstructorDefs . convertTo -- Map.fromList . map (\c -> (Ref $ Name.toText $ constructorName c, c))

mkFieldDefs :: [FieldDef v] -> FieldDefs v
mkFieldDefs fs = FieldDefs fs $ length fs

instance IsString Name where
    fromString s = n
        where Right n = name $ T.pack s

instance ConvertsTo Sort Sort where
    convertTo = id
