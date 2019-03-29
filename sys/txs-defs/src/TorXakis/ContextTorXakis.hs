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
-- The TorXakis Context adds additional realization constraints,
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
import           Data.Data              (Data)
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.Error         ( Error ( Error ) )
import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.Sort          ( Sort ( SortBool )
                                        , ADTDef
                                        , adtName
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
    addADTs as (ContextTorXakis ctx)
            | not $ null conflictADTDefs = Left $ Error ("ADTDefs with conflicts between Boolean Field and implicit isConstructor function: " ++ show conflictADTDefs)
            | not $ null keywordADTDefs  = Left $ Error ("ADTDefs that use the given TorXakis keywords: " ++ show keywordADTDefs)
            | otherwise                  = case addADTs as ctx of
                                                Left e     -> Left e
                                                Right sctx -> Right $ ContextTorXakis sctx
      where
        conflictADTDefs :: [(Name, Set.Set TxsString)]
        conflictADTDefs = mapMaybe maybeADTConflict as

        maybeADTConflict :: ADTDef -> Maybe (Name, Set.Set TxsString)
        maybeADTConflict a = if Set.null conflictFieldNames
                                then Nothing
                                else Just (adtName a, conflictFieldNames)
            where
                conflictFieldNames :: Set.Set TxsString
                conflictFieldNames = Set.filter hasBoolFieldNameConflict allBoolFieldNames

                -- | A Name of Bool Field has a conflict when it has the same textual representation as the implicit is-made-by-constructor function
                hasBoolFieldNameConflict :: TxsString -> Bool
                hasBoolFieldNameConflict n = n `elem` isConstructorNames

                isConstructorNames :: [TxsString]
                isConstructorNames = map txsFuncNameIsConstructor cs

                cs :: [ConstructorDef]
                cs = elemsConstructor a

                allFields :: [FieldDef]
                allFields = concatMap elemsField cs

                allBoolFields :: [FieldDef]
                allBoolFields = filter boolField allFields
                
                allBoolFieldNames :: Set.Set TxsString
                allBoolFieldNames = Set.fromList (map txsFuncNameField allBoolFields)

                boolField :: FieldDef -> Bool
                boolField f = sort f == SortBool

        keywordADTDefs :: [(Name, Set.Set Name)]
        keywordADTDefs = mapMaybe maybeADTKeyword as
        
        maybeADTKeyword :: ADTDef -> Maybe (Name, Set.Set Name)
        maybeADTKeyword a = let actualNames = usedNames a
                                reservedUsed = Set.filter (isTxsReserved . TorXakis.Name.toText) actualNames
                              in
                                if Set.null reservedUsed
                                    then Nothing
                                    else Just (adtName a, reservedUsed)
                                