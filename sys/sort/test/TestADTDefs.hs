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
import           Data.String (IsString (..))
import qualified Data.Text as T

-- generic TorXakis imports
import Name
import Ref
import Sort.ADTDefs
import Sort.ConstructorDefs
import Sort.FieldDefs
-- ----------------------------------------------------------------------------
testADTList :: Test
testADTList  = TestList [ TestLabel "References" testRef
                        , TestLabel "Adding single ADT" testAddADTSingle
                        , TestLabel "Adding ADT that depends on other ADT" testAddADTDependent
                        , TestLabel "Adding multiple ADTs" testAddADTMultiple
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
-- QUESTION: what are we testing here? Isn't this true by construction?
testRef = TestCase $ do
    let expected = T.pack "12"
        rInt :: Ref Int
        rInt = Ref expected
    assertEqual "Same reference?" expected $ Ref.toText rInt

testAddADTSingle :: Test
-- NOTE: this seems to be testing that "data C = cstrC { fieldInt :: Int }" can be constructed.
testAddADTSingle = TestCase $ do
    let newADTList = [adtCName]
        expADTList = [adtCSort]
    assertEqual "addADTDefs should succeed for single ADT"
        (Right $ mkADTDefs expADTList)
        $ addADTDefs newADTList emptyADTDefs

testAddADTDependent :: Test
testAddADTDependent = TestCase $ do
    let newADTList = [adtCName, adtB' $ nameOf $ SortADT $ Ref "C"]
        -- Right nmB = name "B"
        adtB' t = ADTDef { adtName = "B", constructors = cDefsB' t }
        cDefsB' t = mkConstructorDefs [cstrB2 t]
        expADTList = [adtCSort, adtB' $ SortADT $ Ref "C"]
    assertEqual "addADTDefs should succeed for dependent ADTs"
        (Right $ mkADTDefs expADTList)
        $ addADTDefs newADTList emptyADTDefs

testAddADTMultiple :: Test
testAddADTMultiple = TestCase $ do
    let newADTList = [adtAName, adtBName, adtCName]
        expADTList = [adtASort, adtBSort, adtCSort]
    assertEqual "addADTDefs should succeed for multiple ADTs"
        (Right $ mkADTDefs expADTList)
        $ addADTDefs newADTList emptyADTDefs
        
---------------------------------------------------------------------------
-- ADT Reference conditions
---------------------------------------------------------------------------
testAddADTUnknownRef :: Test
testAddADTUnknownRef = TestCase $ do
    let newADTList = [adtAName]
    assertEqual "addADTDefs should fail for unknown references"
        (Left $ RefsNotFound [([Ref "B"], adtAName)])
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
    let adtList = [adtAName, adtBName, adtCName]
        (actConstructableSorts,actNCADTs) = verifyConstructableADTs ([], adtList)
    assertEqual "All data types should be constructable" (Set.fromList $ map (SortADT . Ref) ["A","B","C"], [])
        (Set.fromList actConstructableSorts,actNCADTs)

testNonConstructableADTs :: Test
testNonConstructableADTs = TestCase $ do
    let -- B { a :: A }
        adtB' = ADTDef { adtName = "B", constructors = cDefsB' }
        cDefsB' = mkConstructorDefs [cstrB1 $ nameOf $ SortADT $ Ref "B"]
        adtList = [adtAName, adtB', adtCName]
        expCADTSorts = Set.fromList [SortADT $ Ref "C"]
        expNCADTs    = Set.fromList [adtB', adtAName]
        (actCADTSorts,actNCADTs) = verifyConstructableADTs ([], adtList)
    assertEqual "Only C should be constructable" expCADTSorts $ Set.fromList actCADTSorts
    assertEqual
        "B' and A should be non-constructable" expNCADTs $ Set.fromList actNCADTs

testADTWithoutConstructor :: Test
testADTWithoutConstructor = TestCase $ do
    let adtList = [ADTDef { adtName = "N", constructors = ConstructorDefs Map.empty }]
        (actCADTSorts,actNCADTs) = verifyConstructableADTs ([], adtList)
    assertEqual "ADT without constructor should be non-constructable"
        ([], Set.fromList adtList) (actCADTSorts, Set.fromList actNCADTs)

---------------------------------------------------------------------------
-- Test Data
---------------------------------------------------------------------------
-- A { b :: B }
adtAName :: ADTDef Name
adtAName = adtA $ nameOf $ SortADT $ Ref "B"

adtASort :: ADTDef Sort
adtASort = adtA $ SortADT $ Ref "B"

adtA :: v -> ADTDef v
adtA s = ADTDef { adtName = "A", constructors = cDefsA }
         where cDefsA = mkConstructorDefs [cstrA]
               cstrA = ConstructorDef { constructorName = "cstrA", fields = fDefsA }
               fDefsA = mkFieldDefs [fieldB]
               fieldB = FieldDef { fieldName = "fieldB", sort = s }
-- B { a :: A } | B { c :: C }
adtBName :: ADTDef Name
adtBName = adtB (nameOf $ SortADT $ Ref "A") (nameOf SortInt)

adtBSort :: ADTDef Sort
adtBSort = adtB (SortADT $ Ref "A") SortInt

adtB :: v -> v -> ADTDef v
adtB s1 s2 = ADTDef { adtName = "B", constructors = cDefsB }
         where cDefsB = mkConstructorDefs [cstrB1 s1, cstrB2 s2]
-- B { a :: A }
cstrB1 :: v -> ConstructorDef v
cstrB1 s = ConstructorDef { constructorName = "cstrB1", fields = fDefsB1 }
           where fDefsB1 = mkFieldDefs [fieldA]
                 fieldA = FieldDef { fieldName = "fieldA", sort = s }
-- B { c :: C }
cstrB2 :: v -> ConstructorDef v
cstrB2 s = ConstructorDef { constructorName = "cstrB2", fields = fDefsB2 }
           where fDefsB2 = mkFieldDefs [fieldC]
                 fieldC = FieldDef { fieldName = "fieldC", sort = s }
-- C { i :: Int }
adtCName :: ADTDef Name
adtCName = adtC $ nameOf SortInt

adtCSort :: ADTDef Sort
adtCSort = adtC SortInt

adtC :: v -> ADTDef v
adtC s = ADTDef { adtName = "C", constructors = cDefsC }
       where cDefsC = mkConstructorDefs [cstrC]
             cstrC = ConstructorDef { constructorName = "cstrC", fields = fDefsC }
             fDefsC = mkFieldDefs [fieldInt]
             fieldInt = FieldDef { fieldName = "fieldInt", sort = s}

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
mkADTDefs :: [ADTDef Sort] -> ADTDefs
mkADTDefs = ADTDefs . Map.fromList . map (\a -> (Ref $ Name.toText $ adtName a, a))

mkConstructorDefs :: [ConstructorDef v] -> ConstructorDefs v
mkConstructorDefs = ConstructorDefs . Map.fromList . map (\c -> (Ref $ Name.toText $ constructorName c, c))

mkFieldDefs :: [FieldDef v] -> FieldDefs v
mkFieldDefs fs = FieldDefs fs $ length fs

instance IsString Name  where
    fromString s = n
        where Right n = name $ T.pack s
