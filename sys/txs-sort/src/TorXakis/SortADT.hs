{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SortADT
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for 'Sort' and Abstract Data Types.
--
-- We have to put 'Sort' and Abstract Data Types into one file
-- because of the circular dependency caused by the 'Sort.SortADT' constructor.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.SortADT
( -- * Sort
  Sort (..)
  -- ** Has Sort
, HasSort (..)
  -- * Abstract Data Types
  -- ** Field Definition
, FieldDef (..)
  -- ** Constructor Definition
, ConstructorDef
, constructorName
, memberField
, lookupField
, positionField
, elemsField
, mkConstructorDef
  -- ** Abstract Data Type Definition
, ADTDef
, adtName
, memberConstructor
, lookupConstructor
, elemsConstructor
, mkADTDef
  -- ** Round Tripping Functionality
, functionNameIsConstructor
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import           Data.List           (elemIndex, find)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.FunctionName
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.PrettyPrint.TorXakis
-----------------------------------------------------------------------------
-- Sort
-----------------------------------------------------------------------------
-- | The data type that represents 'Sort'.
data Sort = SortBool
          | SortInt
          | SortChar
          | SortString
          | SortRegex
          | SortADT (RefByName ADTDef)
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)
-- If we want to make Sort package more flexible, we can use SortPrim "Int" & SortADT "WhatEver".

instance Hashable Sort where
    s `hashWithSalt` SortBool    = s `hashWithSalt` T.pack "Bool"
    s `hashWithSalt` SortInt     = s `hashWithSalt` T.pack "Int"
    s `hashWithSalt` SortChar    = s `hashWithSalt` T.pack "Char"
    s `hashWithSalt` SortString  = s `hashWithSalt` T.pack "String"
    s `hashWithSalt` SortRegex   = s `hashWithSalt` T.pack "Regex"
    s `hashWithSalt` (SortADT r) = s `hashWithSalt` r  -- ADT name differs from predefined names

-- | Enables 'Sort's of entities to be accessed in a common way.
class HasSort c a where
    getSort :: c -> a -> Sort

-- | Data structure for a field definition.
data FieldDef = FieldDef
    { -- | Name of the field
      fieldName :: Name
      -- | Sort of the field
    , sort      :: Sort
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName FieldDef where
    getName = fieldName

instance HasSort c FieldDef where
    getSort _ = sort

-- | Constructor Definition.
data ConstructorDef = ConstructorDef
                        { -- | Name of the constructor
                          constructorName :: Name
                          -- | Field definitions of the constructor
                          --   Note that the position in the list is relevant as it represents implicitly
                          --   the position of the fields in a constructor.
                          --   In other words, fields are referred to by their position among others to speed up (de)serialization.
                        , fields :: [FieldDef]
                        }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ConstructorDef where
    getName = constructorName

-- | Smart constructor for 'TorXakis.SortADT.ConstructorDef'.
--   A 'TorXakis.SortADT.ConstructorDef' is returned when the following constraint is satisfied:
--
--   * All field names are unique
--
--   Otherwise an error is returned. The error reflects the violations of the aforementioned constraint.
mkConstructorDef :: Name -> [FieldDef] -> Either Error ConstructorDef
mkConstructorDef n fs
    | null nuFields     = Right $ ConstructorDef n fs
    | otherwise             = Left $ Error ("Non unique field names: " ++ show nuFields)
    where
        nuFields :: [FieldDef]
        nuFields = repeatedByName fs

-- | Refers the provided name to a FieldDef in the given ConstructorDef?
memberField :: Name -> ConstructorDef -> Bool
memberField n c = n `elem` map fieldName (fields c)

-- | position Field
positionField :: Name -> ConstructorDef -> Maybe Int
positionField n c = n `elemIndex` map fieldName (fields c)

-- | lookup Field
lookupField :: Name -> ConstructorDef -> Maybe FieldDef
lookupField n = find (\f -> n == fieldName f) . fields

-- | All FieldDefs of given ConstructorDef in order.
elemsField :: ConstructorDef -> [FieldDef]
elemsField = fields

-- | Data structure for Abstract Data Type (ADT) definition.
data ADTDef = ADTDef
    { -- | Name of the ADT
      adtName      :: Name
      -- | Constructor definitions of the ADT
    , constructors :: NameMap ConstructorDef
    }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

instance HasName ADTDef where
    getName = adtName

-- | Function Name of is-made-by-constructor.
-- This function is needed to enable round tripping: TorXakis maps this operator on an implicit function.
functionNameIsConstructor :: ConstructorDef -> FunctionName
functionNameIsConstructor c = case mkFunctionName (T.append (T.pack "is") (TorXakis.Name.toText (constructorName c))) of
                                Left e -> error ("functionNameIsConstructor failed on constructor " ++ show c ++ " with " ++ show e)
                                Right n -> n

-- | Smart constructor for 'TorXakis.SortADT.ADTDef'.
--   An 'TorXakis.SortADT.ADTDef' is returned when the following constraints are satisfied:
--
--   * List of 'TorXakis.SortADT.ConstructorDef's is non-empty.
--
--   * Names of 'TorXakis.SortADT.ConstructorDef's are unique
--
--   * Names of 'TorXakis.SortADT.FieldDef's are unique across all 'TorXakis.SortADT.ConstructorDef's
--
--   * Round tripping is possible. 
--     In other words, implicit functions (e.g. for field access and is-made-by-constructor) have distinct function signatures.
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkADTDef :: Name -> [ConstructorDef] -> Either Error ADTDef
mkADTDef _ [] = Left $ Error "Empty Constructor List"
mkADTDef m cs
    | not $ null nuCstrDefs                 = Left $ Error ("Non-unique constructor definitions: " ++ show nuCstrDefs)
    | not $ null nuFields                   = Left $ Error ("Non-unique field definitions: " ++ show nuFields)
    | not $ null conflictFieldIsConstructor = Left $ Error ("Conflicts between Field and implicit isConstructor function: " ++ show conflictFieldIsConstructor)
    | otherwise                             = Right $ ADTDef m (toNameMap cs)
    where
        nuCstrDefs :: [ConstructorDef]
        nuCstrDefs   = repeatedByName cs

        allFields :: [FieldDef]
        allFields = concatMap fields cs

        nuFields :: [FieldDef]
        nuFields = repeatedByName allFields

        -- for each constructor TorXakis adds isCstr :: X -> Bool      function which should not conflict with
        --              the accessor function field  :: X -> SortField
        -- hence for round tripping we need to check that fields of type Bool don't have a name equal to any functionNameIsConstructor
        conflictFieldIsConstructor :: [FieldDef]
        conflictFieldIsConstructor = filter sameTextRepresentation allBoolFields
            where
                allBoolFields :: [FieldDef]
                allBoolFields = filter boolField allFields

                boolField :: FieldDef -> Bool
                boolField f = sort f == SortBool

                sameTextRepresentation :: FieldDef -> Bool
                sameTextRepresentation fd = TorXakis.Name.toText (fieldName fd) `elem` isConstructorNames

                isConstructorNames :: [Text]
                isConstructorNames = map (TorXakis.FunctionName.toText . functionNameIsConstructor) cs


-- | Refers the provided ConstructorDef name to a ConstrucotrDef in the given ADTDef?
memberConstructor :: Name -> ADTDef -> Bool
memberConstructor r a = member r (constructors a)

-- | lookup ConstructorDef
lookupConstructor :: Name -> ADTDef -> Maybe ConstructorDef
lookupConstructor r a = TorXakis.NameMap.lookup r (constructors a)

-- | All ConstructorDefs of given ADTDef
elemsConstructor :: ADTDef -> [ConstructorDef]
elemsConstructor = elems . constructors

-- Pretty Print
instance PrettyPrint a Sort where
    prettyPrint _ _ SortBool     = TxsString (T.pack "Bool")
    prettyPrint _ _ SortInt      = TxsString (T.pack "Int")
    prettyPrint _ _ SortChar     = TxsString (T.pack "Char")
    prettyPrint _ _ SortString   = TxsString (T.pack "String")
    prettyPrint _ _ SortRegex    = TxsString (T.pack "Regex")
    prettyPrint _ _ (SortADT a)  = (TxsString . TorXakis.Name.toText . toName) a

instance PrettyPrint c FieldDef where
    prettyPrint o c fd = TxsString ( T.concat [ TorXakis.Name.toText (fieldName fd)
                                              , T.pack " :: "
                                              , TorXakis.PrettyPrint.TorXakis.toText ( prettyPrint o c (sort fd) )
                                              ] )

instance PrettyPrint c ConstructorDef where
    prettyPrint o c cv = TxsString ( T.append (TorXakis.Name.toText (constructorName cv))
                                              (case (short o, fields cv) of
                                                  (True, [])   -> T.empty
                                                  (True, x:xs) -> T.concat [ wsField
                                                                           , T.pack "{ "
                                                                           , TorXakis.Name.toText (fieldName x)
                                                                           , shorten (sort x) xs
                                                                           , wsField
                                                                           , T.pack "}"
                                                                           ]
                                                  _            -> T.concat [ wsField
                                                                           , T.pack "{ "
                                                                           , T.intercalate (T.append wsField (T.pack "; ")) (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o c) (fields cv))
                                                                           , wsField
                                                                           , T.pack "}"
                                                                           ]
                                              )
                                   )
        where wsField :: T.Text
              wsField = if multiline o then T.pack "\n        "
                                       else T.singleton ' '

              shorten :: Sort -> [FieldDef] -> T.Text
              shorten s []                     = addSort s
              shorten s (x:xs) | sort x == s   = T.concat [ T.pack ", "
                                                          , TorXakis.Name.toText (fieldName x)
                                                          , shorten s xs
                                                          ]
              shorten s (x:xs)                 = T.concat [ addSort s
                                                          , wsField
                                                          , T.pack "; "
                                                          , TorXakis.Name.toText (fieldName x)
                                                          , shorten (sort x) xs
                                                          ]

              addSort :: Sort -> T.Text
              addSort s = T.append (T.pack " :: ") ( TorXakis.PrettyPrint.TorXakis.toText ( prettyPrint o c s ) )

instance PrettyPrint c ADTDef where
    prettyPrint o c av = TxsString ( T.concat [ T.pack "TYPEDEF "
                                              , TorXakis.Name.toText (adtName av)
                                              , T.pack " ::="
                                              , wsConstructor
                                              , offsetFirst
                                              , T.intercalate (T.append wsConstructor (T.pack "| ")) (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o c) (elemsConstructor av))
                                              , separator o
                                              , T.pack "ENDDEF"
                                              ] )
        where wsConstructor = if multiline o then T.pack "\n    "
                                             else T.singleton ' '
              offsetFirst   = if multiline o then T.pack "  "      -- same length as constructors separator ("| ") to get nice layout with multilines
                                             else T.empty
