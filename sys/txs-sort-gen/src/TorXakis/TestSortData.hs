{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestSortData
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Sort Context for Test: 
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
module TorXakis.TestSortData
(-- * Test Sort Context
  TestSortData
, sortSize
, adtSize
, constructorSize
, empty
, afterAddADTs
)
where
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import           Data.Maybe          (catMaybes, fromMaybe)
import           GHC.Generics        (Generic)

import           TorXakis.Name
import           TorXakis.Sort

-- | Data needed for test purposes (such as QuickCheck).
data TestSortData = TestSortData 
                        { mapSortSize :: Map.Map Sort Int
                        , mapAdtMapConstructorSize :: Map.Map (RefByName ADTDef) (Map.Map (RefByName ConstructorDef) Int)
                        } deriving (Eq, Ord, Read, Show, Generic, Data)
-- | Sort Size
--   The size of the provided 'TorXakis.Sort' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.Sort' reference.
sortSize :: Sort -> TestSortData -> Int
sortSize s tsd = fromMaybe (error ("sort not contained in context " ++ show s))
                           $ Map.lookup s (mapSortSize tsd)

-- |  adt Size
--   The size of the provided reference to 'TorXakis.ADTDef' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef' and any related 'TorXakis.Sort' references.
adtSize :: RefByName ADTDef -> TestSortData -> Int
adtSize r tsd = fromMaybe (error ("reference not contained in context " ++ show r))
                          $ Map.lookup (SortADT r) (mapSortSize tsd)

-- |  constructor Size
--   The size of the provided constructor as specified by the references to 'TorXakis.ADTDef' and 'TorXakis.ConstructorDef' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef', 'TorXakis.ConstructorDef' and any related 'TorXakis.Sort' references.
constructorSize :: RefByName ADTDef -> RefByName ConstructorDef -> TestSortData -> Int
constructorSize r c tsd = case Map.lookup r (mapAdtMapConstructorSize tsd) of
                                Nothing -> error ("ADT reference not contained in context " ++ show r)
                                Just m  -> fromMaybe (error ("component reference " ++ show c ++ " not contained in ADT " ++ show r))
                                                     $ Map.lookup c m

-- | Constructor of empty Test Sort Data
empty :: TestSortData
empty = TestSortData primitiveSortSize Map.empty
      where
        primitiveSortSize :: Map.Map Sort Int
        primitiveSortSize = Map.fromList $ zip [ SortBool
                                               , SortInt
                                               --, SortChar
                                               , SortString
                                               --, SortRegex
                                               ]
                                               (repeat 0)

-- | Update TestSortData to remain consistent after
-- a successful addition of ADTs to the context.
afterAddADTs :: a -> [ADTDef] -> TestSortData -> TestSortData
afterAddADTs _ as tsd =
    let
      newMapSortSize = addToMapSortSize as (mapSortSize tsd)
      newMapAdtMapConstructorSize = addToMapAdtMapConstructorSize (mapAdtMapConstructorSize tsd) newMapSortSize as
    in
       TestSortData newMapSortSize
                    newMapAdtMapConstructorSize
      where
            addToMapAdtMapConstructorSize :: Map.Map (RefByName ADTDef) (Map.Map (RefByName ConstructorDef) Int)
                                          -> Map.Map Sort Int
                                          -> [ADTDef]
                                          -> Map.Map (RefByName ADTDef) (Map.Map (RefByName ConstructorDef) Int)
            addToMapAdtMapConstructorSize cMap sMap =
                foldl addConstructorSizes cMap
              where
                addConstructorSizes :: Map.Map (RefByName ADTDef) (Map.Map (RefByName ConstructorDef) Int) 
                                    -> ADTDef 
                                    -> Map.Map (RefByName ADTDef) (Map.Map (RefByName ConstructorDef) Int)
                addConstructorSizes iMap adef =
                    let ra :: RefByName ADTDef
                        ra = (RefByName . adtName) adef in
                        if Map.member ra iMap 
                            then error ("Invariant violated: adding already contained ADTDef " ++ show adef)
                            else Map.insert ra (Map.fromList (map (\c -> (RefByName (constructorName c), getConstructorSize sMap c) ) (elemsConstructor adef) ) ) iMap

            addToMapSortSize :: [ADTDef] -> Map.Map Sort Int -> Map.Map Sort Int
            addToMapSortSize adefs defined =
                let newDefined = foldl addCurrent defined adefs
                    in if newDefined == defined 
                        then if any (`Map.notMember` newDefined) (map (SortADT . RefByName . adtName) adefs)
                                then error ("Invariant violated: non constructable ADTDefs in " ++ show adefs)
                                else newDefined
                        else addToMapSortSize adefs newDefined
              where 
                addCurrent :: Map.Map Sort Int -> ADTDef -> Map.Map Sort Int
                addCurrent mp aDef = case getKnownAdtSize mp aDef of
                                        Nothing -> mp
                                        Just i  -> Map.insert ((SortADT . RefByName . adtName) aDef) i mp

                getKnownAdtSize :: Map.Map Sort Int -> ADTDef -> Maybe Int
                getKnownAdtSize mp adef =
                    case catMaybes knownConstructorSizes of
                        [] -> Nothing
                        cs -> Just $ minimum cs             -- complexity sort is minimum of complexity of its constructors
                      where
                        knownConstructorSizes :: [Maybe Int]
                        knownConstructorSizes = map (getKnownConstructorSize mp) (elemsConstructor adef)

            getKnownConstructorSize :: Map.Map Sort Int -> ConstructorDef -> Maybe Int
            getKnownConstructorSize defined cdef =
                    sum <$> sequence fieldSizes         -- sum of complexity fields
                                                        -- e.g. Nil() has size 0
                where
                    fieldSizes :: [Maybe Int]
                    fieldSizes = map (\s -> case Map.lookup (sort s) defined of
                                                Just x -> Just $ x + 1              -- complexity of field = 1 + complexity of sort of field 
                                                                                    -- complexity of sort starts at zero (due to QuickCheck), we want the more fields the more complexity
                                                Nothing -> Nothing
                                      )
                                      (elemsField cdef)

            getConstructorSize :: Map.Map Sort Int -> ConstructorDef -> Int
            getConstructorSize defined cdef = fromMaybe (error ("Invariant violated: unable to calculate size of ConstructorDef " ++ show cdef) )
                                                        (getKnownConstructorSize defined cdef)
