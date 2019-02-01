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
( -- * Sort Context (part without error handling)
  SortSplit (..)
  -- * Sort Context (including error handling)
, SortContext (..)
, elemSort
, prettyPrintSortContext
, violationsAddAdtDefs
)
where
import           Control.Monad.Reader
import qualified Data.HashMap        as Map
import qualified Data.List           as List
import           Data.Maybe          (mapMaybe)
import qualified Data.Text           as T

import           TorXakis.Error      ( Error, MinError(MinError) )
import           TorXakis.Name       ( Name, getName, repeatedByNameIncremental, RefByName( toName ) )
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.SortADT    ( ADTDef, constructors
                                     , ConstructorDef, fields
                                     , FieldDef(sort)
                                     , Sort(SortADT)
                                     )

-- See https://stackoverflow.com/questions/23983374/how-to-handle-functions-of-a-multi-parameter-typeclass-who-not-need-every-type
-- why we splitted into two smaller type classes

-- | The part of a Sort Context without error handling
class SortSplit a where
    -- | An empty sort context (initial state)
    empty :: a
    -- | Accessor for ADTDefs
    adtDefs :: a -> Map.Map (RefByName ADTDef) ADTDef

-- | Is the provided sort a element of the context?
elemSort :: SortSplit a => Sort  -- ^ the sort
                        -> Reader a Bool
elemSort (SortADT a) = do
    as <- reader adtDefs
    return $ Map.member a as
elemSort _           = return True

-- | Generic Pretty Printer for all instance of 'SortContext'.
prettyPrintSortContext :: SortSplit a => Options -> a -> TxsString
prettyPrintSortContext o sc = TxsString (T.intercalate (T.pack "\n") (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o sc) (Map.elems (adtDefs sc))))

-- | A Sort Context instance 
-- contains all definitions to work with sorts and references thereof.
-- The Sort Context includes error handling of a generic error.
class (SortSplit a, Error e) => SortContext a e where
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
    addAdtDefs :: a -> [ADTDef] -> Either e a


-- | Validation function that reports whether an error will occurs when the list of 'ADTDef's are added to the given context.
--   The error reflects the violations of any of the following constraints:
--
--   * The 'Name's of ADTDef are unique
--
--   * All references are known
--
--   * All ADTs are constructable
violationsAddAdtDefs :: SortSplit a => a -> [ADTDef] -> Maybe MinError
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
