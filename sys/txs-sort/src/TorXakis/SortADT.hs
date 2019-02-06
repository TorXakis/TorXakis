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
, fields
, mkConstructorDef
  -- ** Abstract Data Type Definition
, ADTDef
, adtName
, constructors
, mkADTDef
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import qualified Data.HashMap        as Map
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error                     (Error(Error))
import           TorXakis.Name                      (Name, toText, repeatedByName, HasName, getName, RefByName ( toName ), toMapByName)
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
    hashWithSalt s SortBool    = s `hashWithSalt` T.pack "Bool"
    hashWithSalt s SortInt     = s `hashWithSalt` T.pack "Int"
    hashWithSalt s SortChar    = s `hashWithSalt` T.pack "Char"
    hashWithSalt s SortString  = s `hashWithSalt` T.pack "String"
    hashWithSalt s SortRegex   = s `hashWithSalt` T.pack "Regex"
    hashWithSalt s (SortADT r) = s `hashWithSalt` (TorXakis.Name.toText . toName) r  -- ADT name differs from predefined names

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

instance HasSort a FieldDef where
    getSort _ = sort
    
-- | Constructor Definition.
data ConstructorDef = ConstructorDef
                        { -- | Name of the constructor
                          constructorName :: Name
                          -- | Field definitions of the constructor
                          --   Note that the position in the list is relevant as it represents implicitly
                          --   the position of the fields in a constructor.
                        , fields :: [FieldDef]
                        }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ConstructorDef where
    getName = constructorName

-- | Smart constructor for ConstructorDef
mkConstructorDef :: Name -> [FieldDef] -> Either Error ConstructorDef
mkConstructorDef n fs
    | null nuFieldNames     = Right $ ConstructorDef n fs
    | otherwise             = Left $ Error ("Non unique field names: " ++ show nuFieldNames)
    where
        nuFieldNames :: [FieldDef]
        nuFieldNames = repeatedByName fs

-- | Data structure for Abstract Data Type (ADT) definition.
-- TODO: hide decision to use HashMap - provide elems, lookup and member functions for constructors
data ADTDef = ADTDef
    { -- | Name of the ADT
      adtName      :: Name
      -- | Constructor definitions of the ADT
    , constructors :: Map.Map (RefByName ConstructorDef) ConstructorDef
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ADTDef where
    getName = adtName

-- | An ADTDef is returned when the following constraints are satisfied:
--
--   * List of 'ConstructorDef's is non-empty.
--
--   * Names of 'ConstructorDef's are unique
--
--   * Names of 'FieldDef's are unique across all 'ConstructorDef's
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkADTDef :: Name -> [ConstructorDef] -> Either Error ADTDef
mkADTDef _ [] = Left $ Error "Empty Constructor List"
mkADTDef m cs
    | not $ null nuCstrDefs   = Left $ Error ("Non-unique constructor definitions" ++ show nuCstrDefs)
    | not $ null nuFieldNames = Left $ Error ("Non-unique field definitions" ++ show nuFieldNames)
    | otherwise               = Right $ ADTDef m (toMapByName cs)
    where
        nuCstrDefs :: [ConstructorDef]
        nuCstrDefs   = repeatedByName cs
        
        nuFieldNames :: [FieldDef]
        nuFieldNames = repeatedByName (concatMap fields cs)

-- Pretty Print 
instance PrettyPrint a Sort where
    prettyPrint _ _ SortBool     = TxsString (T.pack "Bool")
    prettyPrint _ _ SortInt      = TxsString (T.pack "Int")
    prettyPrint _ _ SortChar     = TxsString (T.pack "Char")
    prettyPrint _ _ SortString   = TxsString (T.pack "String")
    prettyPrint _ _ SortRegex    = TxsString (T.pack "Regex")
    prettyPrint _ _ (SortADT a)  = (TxsString . TorXakis.Name.toText . toName) a

instance PrettyPrint a FieldDef where
    prettyPrint o c fd = TxsString ( T.concat [ TorXakis.Name.toText (fieldName fd)
                                              , T.pack " :: "
                                              , TorXakis.PrettyPrint.TorXakis.toText ( prettyPrint o c (sort fd) )
                                              ] )

instance PrettyPrint a ConstructorDef where
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

instance PrettyPrint a ADTDef where
    prettyPrint o c av = TxsString ( T.concat [ T.pack "TYPEDEF "
                                              , TorXakis.Name.toText (adtName av)
                                              , T.pack " ::="
                                              , wsConstructor
                                              , offsetFirst
                                              , T.intercalate (T.append wsConstructor (T.pack "| ")) (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o c) (Map.elems (constructors av)))
                                              , separator o
                                              , T.pack "ENDDEF"
                                              ] )
        where wsConstructor = if multiline o then T.pack "\n    "
                                             else T.singleton ' '
              offsetFirst   = if multiline o then T.pack "  "      -- same length as constructors separator ("| ") to get nice layout with multilines
                                             else T.empty
