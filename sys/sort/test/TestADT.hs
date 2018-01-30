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
import qualified Data.Map as Map

-- generic TorXakis imports
import Ref
import SortInternal
-- ----------------------------------------------------------------------------
testADTList :: Test
testADTList  = TestList [ TestLabel "Ref Test" testRef
                        , TestLabel "Test Constructable ADTs" testADTAnalysisConstructable
                        , TestLabel "Test Non-Constructable ADTs" testADTAnalysisNonConstructable
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

testADTAnalysisConstructable :: Test
testADTAnalysisConstructable = TestCase $ do
    let adtList = [(adtARef, adtA),(adtBRef, adtB),(adtCRef, adtC)]
        cADTs   = Map.fromList adtList
    assertEqual "All data types should be constructable" (cADTs,[])
        $ analyzeADTs (adtDefsToMap emptyADTDefs,[]) adtList

testADTAnalysisNonConstructable :: Test
testADTAnalysisNonConstructable = TestCase $ do
    let -- B { a :: A }
        adtBRef' = Ref 4
        adtB' = ADTDef { adtName = "B"
                       , constructors = cDefsB'
                       }
        Right cDefsB' = constructorDefs [(Ref 1, cstrB1)]
        adtList' = [(adtARef, adtA),(adtBRef', adtB'),(adtCRef, adtC)]
        cADTs'   = Map.fromList [(adtCRef, adtC)]
        ncADTs   = [(adtARef, adtA),(adtBRef', adtB')]
    assertEqual "Only C should be constructable" (cADTs',reverse ncADTs)
        $ analyzeADTs (adtDefsToMap emptyADTDefs,[]) adtList'

-- Test Data

-- A { b :: B }
adtARef = Ref 1
adtA = ADTDef { adtName = "A"
              , constructors = cDefsA
              }
Right cDefsA = constructorDefs [(Ref 1, cstrA)]
cstrA = ConstructorDef { constructorName = "cstrA"
                       , fields = fDefsA
                       }
Right fDefsA = fieldDefs [(Ref 1, fieldB)]
fieldB = FieldDef { fieldName = "fieldB"
                  , sort = SortADT adtBRef}
-- B { a :: A } | B { c :: C }
adtBRef = Ref 2
adtB = ADTDef { adtName = "B"
, constructors = cDefsB
}
Right cDefsB = constructorDefs [(Ref 1, cstrB1), (Ref 2, cstrB2)]
-- B { a :: A }
cstrB1 = ConstructorDef { constructorName = "cstrB1"
                , fields = fDefsB1
                }
Right fDefsB1 = fieldDefs [(Ref 1, fieldA)]
fieldA = FieldDef { fieldName = "fieldA"
            , sort = SortADT adtARef}
-- B { c :: C }
cstrB2 = ConstructorDef { constructorName = "cstrB2"
                , fields = fDefsB2
                }
Right fDefsB2 = fieldDefs [(Ref 1, fieldC)]
fieldC = FieldDef { fieldName = "fieldC"
            , sort = SortADT adtCRef}
-- C { i :: Int }
adtCRef = Ref 3
adtC = ADTDef { adtName = "C"
              , constructors = cDefsC
              }
Right cDefsC = constructorDefs [(Ref 1, cstrC)]
cstrC = ConstructorDef { constructorName = "cstrC"
                       , fields = fDefsC
                       }
Right fDefsC = fieldDefs [(Ref 1, fieldInt)]
fieldInt = FieldDef { fieldName = "fieldInt"
                  , sort = SortInt}