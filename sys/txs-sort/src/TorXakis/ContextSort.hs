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
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ContextSort
( -- * Sort Context
  ContextSort(ContextSort)
, TorXakis.ContextSort.empty
)
where
import           Data.Data            (Data)
import qualified Data.List            as List
import           Data.Maybe           (mapMaybe)
import qualified Data.Set             as Set
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

import           TorXakis.Error       ( Error ( Error ) )
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.Sort        ( Sort ( SortADT )
                                      , ADTDef
                                      , adtName
                                      , elemsConstructor
                                      , ConstructorDef
                                      , elemsField
                                      , sort
                                      , usedSorts
                                      )
import           TorXakis.SortContext

-- | An instance of 'SortContext'.
newtype ContextSort = ContextSort { adtDefs :: NameMap ADTDef 
                                  } deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor of empty SortContext
empty :: ContextSort
empty = ContextSort TorXakis.NameMap.empty

instance SortContext ContextSort where
    memberSort (SortADT a) = member (toName a) . adtDefs
    memberSort _           = const True

    memberADT adtRef = member adtRef . adtDefs

    lookupADT adtRef = TorXakis.NameMap.lookup adtRef . adtDefs

    elemsADT = elems . adtDefs

    addADTs as ctx = case violationsAddAdtDefs of
                                Just e  -> Left e
                                Nothing -> Right $ ctx { adtDefs = adtDefs ctx `union` toNameMap as }
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
                | not $ null unknownSorts         = Just $ Error ("Unknown sorts : " ++ show unknownSorts)
                | not $ null nonConstructableADTs = Just $ Error ("Non constructable ADTs : " ++ show nonConstructableADTs)
                | otherwise                       = Nothing
                where
                    definedADTs :: [ADTDef]
                    definedADTs = elems (adtDefs ctx)

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

-- | Pretty Printer for 'TorXakis.ContextSort'.
instance PrettyPrintContext ContextSort where
    prettyPrintContext o s = TxsString (T.intercalate (T.pack "\n") (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o s) (elemsADT s)))