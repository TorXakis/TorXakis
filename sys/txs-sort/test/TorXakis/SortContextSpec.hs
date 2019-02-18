{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortContextSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'SortContext'.
-----------------------------------------------------------------------------
module TorXakis.SortContextSpec
(spec
)
where
import qualified Data.Text           as T
import           Test.Hspec

import           TorXakis.ContextSort
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext


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
        aRef = mkADTRef aName
        cyclicAdtdef = unsafeADTDef aName 
                                    [ unsafeConstructorDef (unsafeName "cstrName")
                                                           [ FieldDef (unsafeName "fieldName") (SortADT aRef) ]
                                    ]
      in
        case addADTs [cyclicAdtdef] (empty :: ContextSort) of
            Right _ -> False
            Left _  -> True

-- | An ADTDef can not contain unknown references in a context
prop_ADTDefs_unknownReference :: Bool
prop_ADTDefs_unknownReference =
    let unknownName = unsafeName "unknown"
        unknownRef = mkADTRef unknownName
        undefinedRefAdtdef = unsafeADTDef (unsafeName "adtName") 
                                          [ unsafeConstructorDef (unsafeName "cstrName")
                                                                 [ FieldDef (unsafeName "fieldName") (SortADT unknownRef) ]
                                          ]
      in
        case addADTs [undefinedRefAdtdef] (empty :: ContextSort) of
            Right _ -> False
            Left _  -> True

-- | A context cannot contain ADTDefs with the same name
prop_ADTDefs_unique :: Bool
prop_ADTDefs_unique =
    let adtdef = unsafeADTDef (unsafeName "adtName") 
                              [ unsafeConstructorDef (unsafeName "cstrName") [] ]
      in
        case addADTs [adtdef, adtdef] (empty :: ContextSort) of
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
        depConstructorDef = unsafeConstructorDef (unsafeName "dependent")
                                                 [ FieldDef aName (SortADT aRef)
                                                 , FieldDef bName (SortADT bRef)
                                                 , FieldDef cName (SortADT cRef)
                                                 ]
        cstrName = unsafeName "cstr"
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
      in
        case addADTs [aDef, bDef, cDef] (empty :: ContextSort) of
            Left  _      -> False
            Right newCtx -> elemsADT newCtx == [aDef, bDef, cDef]


spec :: Spec
spec =
  describe "A sort context" $ do
    it "cannot be extended with non constructable ADTDefs" prop_ADTDefs_nonConstructable
    it "cannot be extended with ADTDefs with unknown references" prop_ADTDefs_unknownReference
    it "cannot be extended such that names are no longer unique" prop_ADTDefs_unique
    it "can contain dependent ADTDefs" prop_ADTDefs_Dependent