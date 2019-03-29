{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort.SortContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Sort: all defined sorts and necessary other definitions
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.SortContext
(-- * Sort Context
  SortContext (..)
, conceptualErrorAddADTs
  -- dependencies, yet part of interface
, Error
, Sort
, ADTDef
)
where
import qualified Data.List                      as List
import           Data.Maybe                     (mapMaybe)
import qualified Data.Set                       as Set

import           TorXakis.Error                 ( Error ( Error ) )
import           TorXakis.Name
import           TorXakis.Sort                  (Sort(..)
                                                , ADTDef(adtName)
                                                , elemsConstructor
                                                , ConstructorDef
                                                , elemsField
                                                , sort
                                                , usedSorts
                                                )

-- | A Sort Context instance 
-- contains all definitions to work with sorts and references thereof.
class SortContext c where
    -- | Is the provided sort a member of the context?
    memberSort :: Sort -> c -> Bool
    -- | All Sort elements in the context
    elemsSort :: c -> [Sort]
    elemsSort ctx =   SortBool
                    : SortInt
                    : SortChar
                    : SortString
                    : SortRegex
                    : map (SortADT . RefByName . adtName) (elemsADT ctx)

    -- | Points the provided name to an ADTDef in the context?
    memberADT :: Name -> c -> Bool
    -- | lookup ADTDef using the provided name
    lookupADT :: Name -> c -> Maybe ADTDef
    -- | All ADTDef elements in the context
    elemsADT :: c -> [ADTDef]
    -- | Add ADT definitions to sort context.
    -- When the ADT definitions are accepted, a Sort Context is returned
    -- Otherwise an error is return.
    addADTs :: [ADTDef] -> c -> Either Error c

-- | Validation function that reports whether a conceptual error will occur
--   when the list of 'ADTDef's would be added to the given context.
--   The conceptual error reflects the violations of any of the following constraints:
--
--   * The 'Name's of ADTDef are unique
--
--   * All references are known
--
--   * All ADTs are constructable
conceptualErrorAddADTs :: SortContext c => [ADTDef] -> c -> Maybe Error
conceptualErrorAddADTs as ctx
                | not $ null nonUniqueReferences  = Just $ Error ("Non unique references : " ++ show nonUniqueReferences)
                | not $ null unknownSorts         = Just $ Error ("Unknown sorts : " ++ show unknownSorts)
                | not $ null nonConstructableADTs = Just $ Error ("Non constructable ADTs : " ++ show nonConstructableADTs)
                | otherwise                       = Nothing
    where
        definedADTs :: [ADTDef]
        definedADTs = elemsADT ctx

        nonUniqueReferences :: [ADTDef]
        nonUniqueReferences = repeatedByNameIncremental definedADTs as

        hasUndefinedSorts :: Set.Set Sort -> ADTDef -> Maybe (ADTDef, Set.Set Sort)
        hasUndefinedSorts definedSorts adtdef =
            let undefinedSorts = usedSorts ctx adtdef `Set.difference` definedSorts in
                if null undefinedSorts
                    then Nothing
                    else Just (adtdef,undefinedSorts)

        unknownSorts :: [(ADTDef, Set.Set Sort)]
        unknownSorts = let definedSorts = Set.fromList (elemsSort ctx ++ map (SortADT . RefByName . adtName) as) in
                           mapMaybe (hasUndefinedSorts definedSorts) as
        
        nonConstructableADTs :: [ADTDef]
        nonConstructableADTs =  verifyConstructibleADTs (map adtName definedADTs) as
            where
                -- | Verifies if given list of 'ADTDef's are constructable.
                --
                --   Input:
                --
                --   * A list of known constructable 'ADTDef's
                --
                --   * A list of 'ADTDef's to be verified
                --
                --   Output:
                --
                --   * A list of non-constructable 'ADTDef's
                --
                verifyConstructibleADTs :: [Name]
                                        -> [ADTDef]
                                        -> [ADTDef]
                verifyConstructibleADTs constructableReferences uADTDfs =
                    let (cs, ncs)  = List.partition
                                    (any (allFieldsConstructable constructableReferences) . elemsConstructor)
                                    uADTDfs
                    in if null cs
                    then uADTDfs
                    else verifyConstructibleADTs (map adtName cs ++ constructableReferences) ncs

                allFieldsConstructable :: [Name] -> ConstructorDef -> Bool
                allFieldsConstructable constructableReferences cDef =
                    all ( isSortConstructable constructableReferences . sort )
                        $ elemsField cDef

                isSortConstructable :: [Name] -> Sort -> Bool
                isSortConstructable ns (SortADT t) = toName t `elem` ns
                isSortConstructable _  _           = True