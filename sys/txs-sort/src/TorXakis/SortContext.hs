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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
module TorXakis.SortContext
( -- * Sort Context
  SortContext (..)
, elemSort
, violationsAddAdtDefs
, prettyPrintSortContext
, MinimalSortContext(MinimalSortContext)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import qualified Data.List           as List
import           Data.Maybe          (mapMaybe)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error      ( MinError(MinError) )
import           TorXakis.Name       ( Name, getName, repeatedByNameIncremental, RefByName( toName ), toMapByName )
import           TorXakis.SortADT    ( ADTDef, viewADTDef, constructors
                                     , ConstructorDef, viewConstructorDef, fields
                                     , FieldDef(sort)
                                     , Sort(SortADT)
                                     )
import           TorXakis.PrettyPrint.TorXakis

-- | A SortContext instance contains all definitions to work with sorts and references thereof
class SortContext a where
-- TODO: Kind of Error should be a parameter: allow for SortContext that yield more informative Errors than MinError

    -- | An empty sort context (initial state)
    empty :: a

    -- | Accessor for ADTDefs
    adtDefs :: a -> Map.Map (RefByName ADTDef) ADTDef

    -- | Add adt definitions to sort context.
    --   A sort context is returned when the following constraints are satisfied:
    --
    --   * The 'Name's of ADTDef are unique
    --
    --   * All references are known
    --
    --   * All ADTs are constructable
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addAdtDefs :: a -> [ADTDef] -> Either MinError a

-- | Is the provided sort a element of the context?
elemSort :: SortContext a => a     -- ^ the context
                          -> Sort  -- ^ the sort
                          -> Bool
elemSort ctx (SortADT a) = Map.member a (adtDefs ctx)
elemSort _   _           = True

-- | Validation function that reports whether an error will occurs when the list of 'ADTDef's are added to the given context.
--   The error reflects the violations of any of the following constraints:
--
--   * The 'Name's of ADTDef are unique
--
--   * All references are known
--
--   * All ADTs are constructable
violationsAddAdtDefs :: SortContext a => a -> [ADTDef] -> Maybe MinError
violationsAddAdtDefs context as
    | not $ null nonUniqueNames       = Just $ MinError (T.pack ("Non unique names : " ++ show nonUniqueNames))
    | not $ null unknownRefs          = Just $ MinError (T.pack ("Unknown references : " ++ show unknownRefs))
    | not $ null nonConstructableADTs = Just $ MinError (T.pack ("Non constructable ADTs : " ++ show nonConstructableADTs))
    | otherwise                       = Nothing
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
        getFields = fields . viewConstructorDef
        
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

-- | A minimal instance of 'SortContext'.
newtype MinimalSortContext = MinimalSortContext { _adtDefs :: Map.Map (RefByName ADTDef) ADTDef 
                                                } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext MinimalSortContext where
    empty = MinimalSortContext Map.empty
    adtDefs = _adtDefs
    addAdtDefs ctx as = case violationsAddAdtDefs ctx as of
                                Just e  -> Left e
                                Nothing -> Right $ ctx { _adtDefs = Map.union (_adtDefs ctx) (toMapByName as) }

-- | Generic Pretty Printer for all instance of 'SortContext'.
prettyPrintSortContext :: SortContext a => Options -> a -> TxsString
prettyPrintSortContext o sc = TxsString (T.intercalate (T.pack "\n") (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o) (Map.elems (adtDefs sc))))

instance PrettyPrint MinimalSortContext where
    prettyPrint = prettyPrintSortContext
