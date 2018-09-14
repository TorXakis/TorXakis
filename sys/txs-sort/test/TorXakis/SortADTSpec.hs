{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortADTSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'SortADT'.
-----------------------------------------------------------------------------
module TorXakis.SortADTSpec
(spec
)
where
import qualified Data.Text as T
import           Test.Hspec

import           TorXakis.Name
import           TorXakis.Sort

arbitraryFieldDef :: FieldDef
arbitraryFieldDef =
    let arbitraryFieldName :: Name
        arbitraryFieldName = case mkName (T.pack "arbitraryFieldName") of
                                Right x -> x
                                Left _ -> error "unexpected error"

        arbitrarySort :: Sort
        arbitrarySort = SortInt
      in
        FieldDef arbitraryFieldName arbitrarySort


-- | NonUniqueFields for Constructor should result in error
prop_Constructor_NonUniqueFields :: Bool
prop_Constructor_NonUniqueFields =
    let
        field :: FieldDef
        field = arbitraryFieldDef
        
        arbitraryConstructorName :: Name
        arbitraryConstructorName = case mkName (T.pack "arbitraryConstructorName") of
                                        Right x -> x
                                        Left _ -> error "unexpected error"
      in
        case mkConstructorDef arbitraryConstructorName [field, field] of
            Left _  -> True
            Right _ -> False

-- | An empty constructor list for an ADTDef should result in an error
prop_ADTDef_Empty :: Bool
prop_ADTDef_Empty =
    let arbitraryName :: Name
        arbitraryName = case mkName (T.pack "arbitraryName") of
                            Right x -> x
                            Left _ -> error "unexpected error"
      in
        case mkADTDef arbitraryName [] of
            Left _  -> True
            Right _ -> False

-- | NonUniqueFields over all Constructors for an ADTDef should result in error
prop_ADTDef_NonUniqueFields :: Bool
prop_ADTDef_NonUniqueFields =
    let field :: FieldDef
        field = arbitraryFieldDef

        arbitraryConstructorName1 :: Name
        arbitraryConstructorName1 = case mkName (T.pack "arbitraryConstructorName1") of
                                        Right x -> x
                                        Left _ -> error "unexpected error"

        constructor1 :: ConstructorDef
        constructor1 = case mkConstructorDef arbitraryConstructorName1 [field] of
                                        Right x -> x
                                        Left _ -> error "unexpected error"

        arbitraryConstructorName2 :: Name
        arbitraryConstructorName2 = case mkName (T.pack "arbitraryConstructorName2") of
                                        Right x -> x
                                        Left _ -> error "unexpected error"

        constructor2 :: ConstructorDef
        constructor2 = case mkConstructorDef arbitraryConstructorName2 [field] of
                                        Right x -> x
                                        Left _ -> error "unexpected error"

        arbitraryName :: Name
        arbitraryName = case mkName (T.pack "arbitraryName") of
                            Right x -> x
                            Left _ -> error "unexpected error"
      in
        case mkADTDef arbitraryName [constructor1, constructor2] of
            Left _  -> True
            Right _ -> False

spec :: Spec
spec = do
  describe "A constructor" $
    it "must have fields with unique names" prop_Constructor_NonUniqueFields
  describe "An ADT" $ do
    it "must be constructable, i.e., have at least one constructor " prop_ADTDef_Empty
    it "must over all constructors have fields with unique names" prop_ADTDef_NonUniqueFields