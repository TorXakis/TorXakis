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
, TorXakis.ContextSort.empty
)
where
import           Data.Data           (Data)
import qualified Data.List           as List
import           Data.Maybe          (mapMaybe)
import           GHC.Generics        (Generic)

import           TorXakis.Error      ( Error ( Error ) )
import           TorXakis.RefMap
import           TorXakis.SortADT    ( Sort ( SortADT )
                                     , ADTDef
                                     , elemsConstructor
                                     , ConstructorDef ( fields )
                                     , sort
                                     )
import           TorXakis.SortContext

-- | An instance of 'SortContext'.
newtype ContextSort = ContextSort { adtDefs :: RefMap ADTDef 
                                  } deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor of empty SortContext
empty :: ContextSort
empty = ContextSort TorXakis.RefMap.empty

instance SortContext ContextSort where
    memberSort (SortADT a) = member a . adtDefs
    memberSort _           = const True

    memberADT adtRef = member adtRef . adtDefs

    lookupADT adtRef = TorXakis.RefMap.lookup adtRef . adtDefs

    elemsADT = elems . adtDefs

    addADTs as ctx = case violationsAddAdtDefs of
                                Just e  -> Left e
                                Nothing -> Right $ ctx { adtDefs = union (adtDefs ctx) (fromList as) }
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
                | not $ null nonUniqueReferences  = Just $ Error ("Non unique references : " ++ show nonUniqueReferences)
                | not $ null unknownReferences    = Just $ Error ("Unknown references : " ++ show unknownReferences)
                | not $ null nonConstructableADTs = Just $ Error ("Non constructable ADTs : " ++ show nonConstructableADTs)
                | otherwise                       = Nothing
                where
                    definedADTs :: [ADTDef]
                    definedADTs = elems (adtDefs ctx)
                    
                    nonUniqueReferences :: [ADTDef]
                    nonUniqueReferences = repeatedByRefIncremental definedADTs as
                    
                    definedReferences :: [Ref ADTDef]
                    definedReferences = map toRef (definedADTs ++ as)
                    
                    hasUnknownReferences :: ADTDef -> Maybe (ADTDef, [Sort])
                    hasUnknownReferences adtdef = 
                        let xs = filter (not . isDefined) (concatMap ( map sort . fields ) (elemsConstructor adtdef) ) in
                            if null xs 
                                then Nothing
                                else Just (adtdef,xs)

                    isDefined :: Sort -> Bool
                    isDefined (SortADT t) = t `elem` definedReferences
                    isDefined _           = True

                    unknownReferences :: [(ADTDef, [Sort])]
                    unknownReferences = mapMaybe hasUnknownReferences as
                    
                    nonConstructableADTs :: [ADTDef]
                    nonConstructableADTs =  verifyConstructibleADTs (map toRef definedADTs) as
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
                        verifyConstructibleADTs :: [Ref ADTDef]
                                                -> [ADTDef]
                                                -> [ADTDef]
                        verifyConstructibleADTs constructableReferences uADTDfs =
                            let (cs, ncs)  = List.partition
                                            (any (allFieldsConstructable constructableReferences) . elemsConstructor)
                                            uADTDfs
                            in if null cs
                            then uADTDfs
                            else verifyConstructibleADTs (map toRef cs ++ constructableReferences) ncs

                        allFieldsConstructable :: [Ref ADTDef] -> ConstructorDef -> Bool
                        allFieldsConstructable constructableReferences cDef =
                            all ( isSortConstructable constructableReferences . sort )
                                $ fields cDef

                        isSortConstructable :: [Ref ADTDef] -> Sort -> Bool
                        isSortConstructable ns (SortADT t) = t `elem` ns
                        isSortConstructable _  _           = True
