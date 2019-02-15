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
    let aName = unsafeName "aName"
        aRef = mkADTRef aName
        aDef = unsafeADTDef aName 
                              [ unsafeConstructorDef (unsafeName "cstrName")
                                                     [ FieldDef (unsafeName "fieldName") (SortADT aRef) ]
                              ]
      in
        case addADTs [aDef] (empty::ContextTestSort) of
            Right _ -> False
            Left _  -> True

-- | An ADTDef can not contain unknown references in a context
prop_ADTDefs_unknownReference :: Bool
prop_ADTDefs_unknownReference =
    let unknownName = unsafeName "unknown"
        unknownRef = mkADTRef unknownName
        adtdef = unsafeADTDef (unsafeName "aName") 
                              [ unsafeConstructorDef (unsafeName "cstrName")
                                                     [ FieldDef (unsafeName "fieldName") (SortADT unknownRef) ]
                              ]
      in
        case addADTs [adtdef] (empty::ContextTestSort) of
            Right _ -> False
            Left _  -> True

-- | A context cannot contain ADTDefs with the same name
prop_ADTDefs_unique :: Bool
prop_ADTDefs_unique =
    let adtdef = unsafeADTDef (unsafeName "aName") 
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
        
        aRef = mkADTRef aName
        bRef = mkADTRef bName
        cRef = mkADTRef cName
        
        depName = unsafeName "dependent"
        cstrName = unsafeName "cstr"
        
        depConstructorDef = unsafeConstructorDef depName
                                                 [ FieldDef aName (SortADT aRef)
                                                 , FieldDef bName (SortADT bRef)
                                                 , FieldDef cName (SortADT cRef)
                                                 ]
        aConstructorDef = unsafeConstructorDef cstrName []
        bConstructorDef = unsafeConstructorDef cstrName
                                               [ FieldDef (unsafeName "diffA") (SortADT aRef) ]
        cConstructorDef = unsafeConstructorDef cstrName
                                               [ FieldDef (unsafeName "diffA") (SortADT aRef)
                                               , FieldDef (unsafeName "diffB") (SortADT bRef)
                                               ]
        aDef = unsafeADTDef aName
                            [ depConstructorDef
                            , aConstructorDef
                            ]
        bDef = unsafeADTDef bName
                            [ depConstructorDef
                            , bConstructorDef
                            ]
        cDef = unsafeADTDef cName
                            [ depConstructorDef
                            , cConstructorDef
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
                            &&  all ( `elem` Map.toList (mapSortSize newCtx) ) [(SortADT (toRef aDef), complexityA)
                                                                               ,(SortADT (toRef bDef), complexityB)
                                                                               ,(SortADT (toRef cDef), complexityC)] -- also primitive Sorts are contained
                            &&  mapAdtMapConstructorSize newCtx == Map.fromList [ (toRef aDef, Map.fromList [(toRef depConstructorDef, complexityDep), (toRef aConstructorDef, complexityA)])
                                                                                , (toRef bDef, Map.fromList [(toRef depConstructorDef, complexityDep), (toRef bConstructorDef, complexityB)])
                                                                                , (toRef cDef, Map.fromList [(toRef depConstructorDef, complexityDep), (toRef cConstructorDef, complexityC)])
                                                                                ]

prop_initial :: Bool
prop_initial =
       all ( (0==) . snd ) (Map.toList (mapSortSize (empty :: ContextTestSort)))
    && Map.null (mapAdtMapConstructorSize (empty :: ContextTestSort))

-- | a sort context can be incrementally extended - which should be the same as creating it in one step.
prop_increment :: Bool
prop_increment =
    let
        aName = unsafeName "A"
        aDef = unsafeADTDef aName
                            [ unsafeConstructorDef (unsafeName "CstrA") [] ]
        bName = unsafeName "B"
        bDef = unsafeADTDef bName
                            [ unsafeConstructorDef (unsafeName "CstrB") [ FieldDef (unsafeName "FieldB") (SortADT (toRef aDef)) ] ]
        dName = unsafeName "D"
        dDef = unsafeADTDef dName
                            [ unsafeConstructorDef (unsafeName "CstrD") [] ]
        cName = unsafeName "C"
        cDef = unsafeADTDef cName
                            [ unsafeConstructorDef (unsafeName "CstrCbyB") [ FieldDef (unsafeName "FieldB") (SortADT (toRef bDef)) ]
                            , unsafeConstructorDef (unsafeName "CstrCbyD") [ FieldDef (unsafeName "FieldD") (SortADT (toRef dDef)) ]
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
    it "contains initially only basic sorts" prop_initial
    it "can be incrementally extended" prop_increment