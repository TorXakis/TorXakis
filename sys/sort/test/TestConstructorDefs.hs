{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings      #-}
module TestConstructorDefs
(
-- testConstructorList
)
where
{-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports

-- generic Haskell imports
import           Data.Either
import qualified Data.Map  as Map
import qualified Data.Text as T

-- generic TorXakis imports
import Name
import Ref
import Sort.ConstructorDefs
import Sort.FieldDefs
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
    let cList = [cstr 1]
    assertEqual "constructorDefs should succeed for single ConstructorDef"
        (Right $ mkConstructorDefs cList)
        $ constructorDefs cList

testCDefMultiple :: Test
testCDefMultiple = TestCase $ do
    let cList = map cstr [1,2,3]
    assertEqual "constructorDefs should succeed for multiple ConstructorDef's"
        (Right $ mkConstructorDefs cList)
        $ constructorDefs cList

---------------------------------------------------------------------------
-- Pre-condition violation tests
---------------------------------------------------------------------------
testEmptyConstructorList :: Test
testEmptyConstructorList = TestCase $
    assertEqual "constructorDefs should fail for empty list"
        (Left EmptyConstructorDefs) $ constructorDefs ([] :: [ConstructorDef Name])

testNonUniqueName :: Test
testNonUniqueName = TestCase $ do
    let Right cName = name "SameName"
        cDef = (ConstructorDef cName $ FieldDefs [] 0) :: ConstructorDef Name
        cstrList = [cDef, cDef]
    assertEqual "constructorDefs should fail for non-unique names"
        (Left $ ConstructorNamesNotUnique cstrList)
        $ constructorDefs cstrList 

testNonUniqueFieldName :: Test
testNonUniqueFieldName = TestCase $ do
    let cDef2 = cstr 2
        fields2 = fields cDef2
        cDef2' = cDef2 { fields =
                            fields2 {
                                fDefsToList =
                                    [FieldDef { fieldName = "field1"
                                              , sort = mkName "2"}]
                                    }
                       }
        cstrList = [cstr 1, cDef2']
    assertEqual "constructorDefs should fail for non-unique field names"
        (Left $ SameFieldMultipleCstr ["field1"])
        $ constructorDefs cstrList

---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------
cstr :: Int -> ConstructorDef Name
cstr i = cDef
    where
        cDef = ConstructorDef { constructorName = mkName ("c" ++ show i), fields = fDefs }
        Right fDefs = fieldDefs [fieldInt]
        fieldInt = FieldDef { fieldName = mkName ("field" ++ show i), sort = mkName $ show i }

mkName :: String -> Name
mkName = fromRight "" . name . T.pack

mkConstructorDefs :: [ConstructorDef v] -> ConstructorDefs v
mkConstructorDefs = ConstructorDefs . Map.fromList . map (\c -> (Ref $ Name.toText $ constructorName c, c))
-}