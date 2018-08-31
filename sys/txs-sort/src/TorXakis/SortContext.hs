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
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.SortContext
( -- * Sort Context
  SortContext (..)
, MinimalSortContext
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import qualified Data.List           as List
import           Data.Maybe          (mapMaybe)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error      ( Error(Error) )
import           TorXakis.Name       ( Name, getName, repeatedByNameIncremental, RefByName, toMapByName, toName )
import           TorXakis.SortADT    ( ADTDef, viewADTDef, constructors
                                     , ConstructorDef, viewConstructorDef, fields
                                     , FieldDef(sort)
                                     , Sort(SortADT)
                                     )

-- | A HasSorts instance contains all definitions to work with sort and reference thereof
class SortContext a where
    -- | An empty sort context (initial state)
    empty :: a

    -- | Add adt definitions to sort context.
    --   A sort context is returned when the following constraints are satisfied:
    --
    --   * The 'Name's of ADTDef are unique
    --
    --   * All references are known
    --
    --   * All ADTs are constructable
    --
    --   Otherwise an error is return. The error reflects the violations of any of the formentioned constraints.
    addAdtDefs :: a -> [ADTDef] -> Either Error a

    -- | Validation function that reports whether an error will occurs when the list of 'ADTDef's are added to the given context.
    --   The error reflects the violations of any of the following constraints:
    --
    --   * The 'Name's of ADTDef are unique
    --
    --   * All references are known
    --
    --   * All ADTs are constructable
    violationsAddAdtDefs :: a -> [ADTDef] -> Maybe Error
    violationsAddAdtDefs context as
        | not $ null nonUniqueNames       = Just $ Error (T.pack ("Non unique names : " ++ show nonUniqueNames))
        | not $ null unknownRefs          = Just $ Error (T.pack ("Unknown references : " ++ show unknownRefs))
        | not $ null nonConstructableADTs = Just $ Error (T.pack ("Non constructable ADTs : " ++ show nonConstructableADTs))
        | otherwise       = Nothing
        where
            definedADTs :: [ADTDef]
            definedADTs = Map.elems (adtDefs context)
            
            nonUniqueNames :: [ADTDef]
            nonUniqueNames = repeatedByNameIncremental definedADTs as
            
            definedNames :: [Name]
            definedNames = map getName (definedADTs ++ as)
            
            hasUnknownRefs :: ADTDef -> Maybe (ADTDef, [Sort])
            hasUnknownRefs adtdef = 
                let xs = filter (not . isDefined) (concatMap ( map sort . getFields ) (getConstructors adtdef) ) in
                    if null xs 
                        then Nothing
                        else Just (adtdef,xs)
            
            getFields :: ConstructorDef -> [FieldDef]
            getFields = fields . viewConstructorDef    -- TODO: discuss Jan: should be globally defined?
            
            getConstructors :: ADTDef -> [ConstructorDef]
            getConstructors = Map.elems . constructors . viewADTDef

            isDefined :: Sort -> Bool
            isDefined (SortADT t) = toName t `elem` definedNames
            isDefined _           = True

            unknownRefs :: [(ADTDef, [Sort])]
            unknownRefs = mapMaybe hasUnknownRefs as
            
            nonConstructableADTs :: [ADTDef]
            nonConstructableADTs =  verifyConstructibleADTs (map getName definedADTs) as
              where
                -- | Verifies if given list of 'ADTDef's are constructable.
                --
                --   Input:
                --
                --   * A list of known constructable 'ADTDef's
                --
                --   * A list of 'ADTDef's to be verified
                --
                --   Output: A tuple consisting of:
                --
                --   * A list of non-constructable 'ADTDef's
                --
                verifyConstructibleADTs ::[Name]
                                        -> [ADTDef]
                                        -> [ADTDef]
                verifyConstructibleADTs constructableSortNames uADTDfs =
                    let (cs, ncs)  = List.partition
                                    (any (allFieldsConstructable constructableSortNames) . getConstructors)
                                    uADTDfs
                    in if null cs
                    then uADTDfs
                    else verifyConstructibleADTs (map getName cs ++ constructableSortNames) ncs

                allFieldsConstructable :: [Name] -> ConstructorDef -> Bool
                allFieldsConstructable constructableSortNames cDef =
                    all ( isSortConstructable constructableSortNames . sort )
                        $ getFields cDef

                isSortConstructable :: [Name] -> Sort -> Bool
                isSortConstructable ns (SortADT t) = toName t `elem` ns
                isSortConstructable _  _           = True

    -- | Accessor for ADTDefs
    adtDefs :: a -> Map.Map (RefByName ADTDef) ADTDef

-- | A minimal instance of 'SortContext'.
newtype MinimalSortContext = MinimalSortContext { adtDefsToMap :: Map.Map (RefByName ADTDef) ADTDef 
                                                } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext MinimalSortContext where
    empty = MinimalSortContext Map.empty
    addAdtDefs context as = case violationsAddAdtDefs context as of
                                Just e  -> Left e
                                Nothing -> Right $ context { adtDefsToMap = Map.union (adtDefsToMap context) (toMapByName as) }
    adtDefs = adtDefsToMap