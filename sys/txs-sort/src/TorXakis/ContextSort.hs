{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextSort
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of Sort Context: ContextSort
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextSort
( -- * Sort Context
  ContextSort(ContextSort)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import qualified Data.List           as List
import           Data.Maybe          (mapMaybe)
import           GHC.Generics        (Generic)

import           TorXakis.Error      ( Error ( Error ) )
import           TorXakis.Name       ( Name
                                     , getName
                                     , RefByName ( toName )
                                     , toMapByName
                                     , repeatedByNameIncremental
                                     )
import           TorXakis.SortADT    ( Sort ( SortADT )
                                     , ADTDef ( constructors )
                                     , ConstructorDef ( fields )
                                     , sort
                                     )
import           TorXakis.SortContext

-- | An instance of 'SortContext'.
newtype ContextSort = ContextSort { adtDefs :: Map.Map (RefByName ADTDef) ADTDef 
                                  } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortReadContext ContextSort where
    elemSort ctx (SortADT a) = Map.member a (adtDefs ctx)
    elemSort _   _           = True

    lookupADTDef ctx adtRef = Map.lookup adtRef (adtDefs ctx)

    elemsADTDef ctx         = Map.elems (adtDefs ctx)

instance SortContext ContextSort where
    empty = ContextSort Map.empty

    addAdtDefs ctx as = case violationsAddAdtDefs of
                                Just e  -> Left e
                                Nothing -> Right $ ctx { adtDefs = Map.union (adtDefs ctx) (toMapByName as) }
        where
            -- | Validation function that reports whether an error will occurs when the list of 'ADTDef's are added to the given context.
            --   The error reflects the violations of any of the following constraints:
            --
            --   * The 'Name's of ADTDef are unique
            --
            --   * All references are known
            --
            --   * All ADTs are constructable
            violationsAddAdtDefs :: Maybe Error
            violationsAddAdtDefs
                | not $ null nonUniqueNames       = Just $ Error ("Non unique names : " ++ show nonUniqueNames)
                | not $ null unknownRefs          = Just $ Error ("Unknown references : " ++ show unknownRefs)
                | not $ null nonConstructableADTs = Just $ Error ("Non constructable ADTs : " ++ show nonConstructableADTs)
                | otherwise                       = Nothing
                where
                    definedADTs :: [ADTDef]
                    definedADTs = Map.elems (adtDefs ctx)
                    
                    nonUniqueNames :: [ADTDef]
                    nonUniqueNames = repeatedByNameIncremental definedADTs as
                    
                    definedNames :: [Name]
                    definedNames = map getName (definedADTs ++ as)
                    
                    hasUnknownRefs :: ADTDef -> Maybe (ADTDef, [Sort])
                    hasUnknownRefs adtdef = 
                        let xs = filter (not . isDefined) (concatMap ( map sort . fields ) (getConstructors adtdef) ) in
                            if null xs 
                                then Nothing
                                else Just (adtdef,xs)

                    getConstructors :: ADTDef -> [ConstructorDef]
                    getConstructors = Map.elems . constructors

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
                                $ fields cDef

                        isSortConstructable :: [Name] -> Sort -> Bool
                        isSortConstructable ns (SortADT t) = toName t `elem` ns
                        isSortConstructable _  _           = True
