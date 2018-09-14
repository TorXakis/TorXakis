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
import qualified Data.HashMap        as Map
import qualified Data.Text           as T
import           Test.Hspec

import           TorXakis.Name
import           TorXakis.Sort
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
                                                     [ FieldDef (unsafeName "fieldName") (SortADT (RefByName aName)) ]
                              ]
        ctx = empty :: MinimalTestSortContext
      in
        case addAdtDefs ctx [adtdef] of
            Right _ -> False
            Left _  -> True

-- | An ADTDef can not contain unknown references in a context
prop_ADTDefs_unknownReference :: Bool
prop_ADTDefs_unknownReference =
    let unknownName = unsafeName "unknown"
        adtdef = unsafeADTDef (unsafeName "adtName") 
                              [ unsafeConstructorDef (unsafeName "cstrName")
                                                     [ FieldDef (unsafeName "fieldName") (SortADT (RefByName unknownName)) ]
                              ]
        ctx = empty :: MinimalTestSortContext
      in
        case addAdtDefs ctx [adtdef] of
            Right _ -> False
            Left _  -> True

-- | A context cannot contain ADTDefs with the same name
prop_ADTDefs_unique :: Bool
prop_ADTDefs_unique =
    let adtdef = unsafeADTDef (unsafeName "adtName") 
                              [ unsafeConstructorDef (unsafeName "cstrName") [] ]
        ctx = empty :: MinimalTestSortContext
      in
        case addAdtDefs ctx [adtdef, adtdef] of
            Right _ -> False
            Left _  -> True

-- | ADTDefs can be dependent on each other
prop_ADTDefs_Dependent :: Bool
prop_ADTDefs_Dependent =
    let aName = unsafeName "A"
        bName = unsafeName "B"
        cName = unsafeName "C"
        aRef :: RefByName ADTDef
        aRef = RefByName aName
        bRef :: RefByName ADTDef
        bRef = RefByName bName
        cRef :: RefByName ADTDef
        cRef = RefByName cName
        
        depName = unsafeName "dependent"
        depRef :: RefByName ConstructorDef
        depRef = RefByName depName
        cstrName = unsafeName "cstr"
        cstrRef :: RefByName ConstructorDef
        cstrRef = RefByName cstrName
        
        depConstructorDef = unsafeConstructorDef depName
                                                 [ FieldDef aName (SortADT aRef)
                                                 , FieldDef bName (SortADT bRef)
                                                 , FieldDef cName (SortADT cRef)
                                                 ]
        aDef = unsafeADTDef aName
                            [ depConstructorDef
                            , unsafeConstructorDef cstrName []
                            ]
        bDef = unsafeADTDef bName
                            [ depConstructorDef
                            , unsafeConstructorDef cstrName
                                                   [ FieldDef (unsafeName "diffA") (SortADT aRef) ]
                            ]
        cDef = unsafeADTDef cName
                            [ depConstructorDef
                            , unsafeConstructorDef cstrName
                                                   [ FieldDef (unsafeName "diffA") (SortADT aRef)
                                                   , FieldDef (unsafeName "diffB") (SortADT bRef)
                                                   ]
                            ]
        ctx = empty :: MinimalTestSortContext
      in
        case addAdtDefs ctx [cDef, bDef, aDef] of
            Left  _      -> False
            Right newCtx ->     adtDefs newCtx == Map.fromList [(aRef, aDef),(bRef, bDef),(cRef, cDef)]
                            &&  all ( `elem` Map.toList (getMapSortSize newCtx) ) [(SortADT aRef, 1),(SortADT bRef, 2),(SortADT cRef, 3)] -- also primitive Sorts are contained
                            &&  getMapAdtMapConstructorSize newCtx == Map.fromList [ (aRef, Map.fromList [(depRef,3), (cstrRef,0)])
                                                                                   , (bRef, Map.fromList [(depRef,3), (cstrRef,1)])
                                                                                   , (cRef, Map.fromList [(depRef,3), (cstrRef,2)])
                                                                                   ]

spec :: Spec
spec =
  describe "A test context" $ do
    it "cannot be extended with non constructable ADTDefs" prop_ADTDefs_nonConstructable
    it "cannot be extended with ADTDefs with unknown references" prop_ADTDefs_unknownReference
    it "cannot be extended such that names are no longer unique" prop_ADTDefs_unique
    it "can contain dependent ADTDefs" prop_ADTDefs_Dependent