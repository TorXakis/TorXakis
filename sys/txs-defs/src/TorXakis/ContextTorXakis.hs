{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextTorXakis
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- The TorXakis Context containing all definitions.
-- The TorXakis Context adds additional constraints,
-- including
--
-- * Prevent name clashes with implicit functions from Sorts
--
-- * Prevent name clashes with keywords
--
-- * Prevent function signature clashes with predefined functions and operators
--
-- * Enable round tripping
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextTorXakis
( -- * Context TorXakis
  ContextTorXakis
, TorXakis.ContextTorXakis.empty
)
where
import           Data.Data            (Data)
import           GHC.Generics         (Generic)

import           TorXakis.Error       ( Error ( Error ) )
import           TorXakis.Language
import           TorXakis.Sort        ( Sort ( SortBool )
                                      , ADTDef
                                      , elemsConstructor
                                      , ConstructorDef
                                      , elemsField
                                      , FieldDef
                                      , sort
                                      )
import           TorXakis.SortContext

import           TorXakis.ContextSort   -- for now

-- | The TorXakis Context.
newtype ContextTorXakis = ContextTorXakis { txsContext :: ContextSort
                                          } deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor of empty ContextTorXakis
empty :: ContextTorXakis
empty = ContextTorXakis TorXakis.ContextSort.empty

instance SortContext ContextTorXakis where
    memberSort r = memberSort r . txsContext

    memberADT r = memberADT r . txsContext

    lookupADT r = lookupADT r . txsContext

    elemsADT = elemsADT . txsContext

    -- Add additional checks to make round tripping possible.
    --
    -- * Names are no keywords -- TODO
    --
    -- * Implicit functions have distinct function signatures:
    --   for each constructor TorXakis adds 'isCstr' :: X -> Bool      function which should not conflict with
    --              the accessor function   'field'  :: X -> SortField
    -- hence for round tripping we need to check that fields of type Bool don't have a name equal to any isCstr.
    addADTs as (ContextTorXakis ctx) =
        case addADTs as ctx of
             Left e     -> Left e
             Right sctx -> if not $ null conflictADTDefs
                              then Left $ Error ("Conflicts between Field and implicit isConstructor function in the following ADTDefs: " ++ show conflictADTDefs)
                              else Right $ ContextTorXakis sctx
      where
        conflictADTDefs :: [ADTDef]
        conflictADTDefs = filter hasADTConflict as

        hasADTConflict :: ADTDef -> Bool
        hasADTConflict a = not $ null conflictFieldDefs
            where
                conflictFieldDefs :: [FieldDef]
                conflictFieldDefs = filter hasBoolFieldConflict allBoolFields

                -- | A Bool Field has a conflict when it has the same as the implicit is-made-by-constructor function
                hasBoolFieldConflict :: FieldDef -> Bool
                hasBoolFieldConflict fd = txsFuncNameField fd `elem` isConstructorNames

                isConstructorNames :: [TxsString]
                isConstructorNames = map txsFuncNameIsConstructor cs

                cs :: [ConstructorDef]
                cs = elemsConstructor a

                allFields :: [FieldDef]
                allFields = concatMap elemsField cs

                allBoolFields :: [FieldDef]
                allBoolFields = filter boolField allFields

                boolField :: FieldDef -> Bool
                boolField f = sort f == SortBool
