{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TestSortContextSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'TestSortContext'.
-----------------------------------------------------------------------------
module TorXakis.TestSortContextSpec
(spec
)
where
import           Debug.Trace
import qualified Data.HashMap        as Map
import qualified Data.Text           as T
import           Test.Hspec

import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext
import           TorXakis.TestSortContext

unsafeName :: String -> Name
unsafeName n = case mkName (T.pack n) of
                    Right x -> x
                    Left e -> error ("unexpected error with name " ++ n ++ "\n" ++ show e)

unsafeConstructorDef :: Name -> [FieldDef] -> ConstructorDef
unsafeConstructorDef n fs = case mkConstructorDef n fs of
                                Right x -> x
                                Left e -> error ("unexpected error with constructor " ++ show n ++ "\n" ++ show e)

unsafeADTDef :: Name -> [ConstructorDef] -> ADTDef
unsafeADTDef n cs = case mkADTDef n cs of
                            Right x -> x
                            Left e -> error ("unexpected error with adtdef " ++ show n ++ "\n" ++ show e)

-- | An ADTDef is not constructable when it needs itself to be constructed
prop_ADTDefs_nonConstructable :: Bool
prop_ADTDefs_nonConstructable =
    let aName = unsafeName "adtName"
        adtdef = unsafeADTDef aName 
                              [ unsafeConstructorDef (unsafeName "cstrName")
                                                     [ FieldDef (unsafeName "fieldName") (SortADT aName) ]
                              ]
      in
        case addADTs [adtdef] (empty::ContextTestSort) of
            Right _ -> False
            Left _  -> True

-- | An ADTDef can not contain unknown references in a context
prop_ADTDefs_unknownReference :: Bool
prop_ADTDefs_unknownReference =
    let unknownName = unsafeName "unknown"
        adtdef = unsafeADTDef (unsafeName "adtName") 
                              [ unsafeConstructorDef (unsafeName "cstrName")
                                                     [ FieldDef (unsafeName "fieldName") (SortADT unknownName) ]
                              ]
      in
        case addADTs [adtdef] (empty::ContextTestSort) of
            Right _ -> False
            Left _  -> True

-- | A context cannot contain ADTDefs with the same name
prop_ADTDefs_unique :: Bool
prop_ADTDefs_unique =
    let adtdef = unsafeADTDef (unsafeName "adtName") 
                              [ unsafeConstructorDef (unsafeName "cstrName") [] ]
      in
        case addADTs [adtdef, adtdef] (empty::ContextTestSort) of
            Right _ -> False
            Left _  -> True

-- | ADTDefs can be dependent on each other
prop_ADTDefs_Dependent :: Bool
prop_ADTDefs_Dependent =
    let aName = unsafeName "A"
        bName = unsafeName "B"
        cName = unsafeName "C"
        depName = unsafeName "dependent"
        cstrName = unsafeName "cstr"
        
        depConstructorDef = unsafeConstructorDef depName
                                                 [ FieldDef aName (SortADT aName)
                                                 , FieldDef bName (SortADT bName)
                                                 , FieldDef cName (SortADT cName)
                                                 ]
        aDef = unsafeADTDef aName
                            [ depConstructorDef
                            , unsafeConstructorDef cstrName []
                            ]
        bDef = unsafeADTDef bName
                            [ depConstructorDef
                            , unsafeConstructorDef cstrName
                                                   [ FieldDef (unsafeName "diffA") (SortADT aName) ]
                            ]
        cDef = unsafeADTDef cName
                            [ depConstructorDef
                            , unsafeConstructorDef cstrName
                                                   [ FieldDef (unsafeName "diffA") (SortADT aName)
                                                   , FieldDef (unsafeName "diffB") (SortADT bName)
                                                   ]
                            ]
        complexityA :: Int
        complexityA   = 0
        complexityB   = 1 + complexityA
        complexityC   = 2 + complexityA + complexityB
        complexityDep = 3 + complexityA + complexityB + complexityC
      in
        case addADTs [cDef, bDef, aDef] (empty::ContextTestSort) of
            Left  _      -> False
            Right newCtx ->     elemsADT newCtx == [ aDef, bDef, cDef ]
                            &&  all ( `elem` Map.toList (mapSortSize newCtx) ) [(SortADT aName, complexityA)
                                                                               ,(SortADT bName, complexityB)
                                                                               ,(SortADT cName, complexityC)] -- also primitive Sorts are contained
                            &&  mapAdtMapConstructorSize newCtx == Map.fromList [ (aName, Map.fromList [(depName, complexityDep), (cstrName, complexityA)])
                                                                                , (bName, Map.fromList [(depName, complexityDep), (cstrName, complexityB)])
                                                                                , (cName, Map.fromList [(depName, complexityDep), (cstrName, complexityC)])
                                                                                ]

-- | a sort context can be incrementally extended - which should be the same as creating it in one step.
prop_increment :: Bool
prop_increment =
    let
        aName = unsafeName "A"
        aDef = unsafeADTDef aName
                            [ unsafeConstructorDef (unsafeName "CstrA") [] ]
        bName = unsafeName "B"
        bDef = unsafeADTDef bName
                            [ unsafeConstructorDef (unsafeName "CstrB") [ FieldDef (unsafeName "FieldB") (SortADT aName) ] ]
        dName = unsafeName "D"
        dDef = unsafeADTDef dName
                            [ unsafeConstructorDef (unsafeName "CstrD") [] ]
        cName = unsafeName "C"
        cDef = unsafeADTDef cName
                            [ unsafeConstructorDef (unsafeName "CstrCbyB") [ FieldDef (unsafeName "FieldB") (SortADT bName) ]
                            , unsafeConstructorDef (unsafeName "CstrCbyD") [ FieldDef (unsafeName "FieldD") (SortADT dName) ]
                            ]
        c0 = empty :: ContextTestSort
        incr1 = [aDef, bDef]
        incr2 = [cDef, dDef]
      in
        case addADTs incr1 c0 of
            Left e1  -> error ("Invalid incr1 - " ++ show e1)
            Right c1 -> case addADTs incr2 c1 of
                                Left e2  -> error ("Invalid incr2 - " ++ show e2)
                                Right c2 -> case addADTs (incr2 ++ incr1) c0 of
                                                Left e    -> trace ("error = " ++ show e) False
                                                Right c12 -> c12 == c2 || trace ("incr1 = " ++ show incr1 ++ "\nincr2 = " ++ show incr2) False

spec :: Spec
spec =
  describe "A test sort context" $ do
    it "cannot be extended with non constructable ADTDefs" prop_ADTDefs_nonConstructable
    it "cannot be extended with ADTDefs with unknown references" prop_ADTDefs_unknownReference
    it "cannot be extended such that names are no longer unique" prop_ADTDefs_unique
    it "can contain dependent ADTDefs" prop_ADTDefs_Dependent
    it "can be incrementally extended" prop_increment