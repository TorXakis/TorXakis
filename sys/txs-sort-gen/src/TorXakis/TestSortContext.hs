{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestSortContext
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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.TestSortContext
(-- * Test Sort Context
  TestSortContext (..)
, ContextTestSort (mapSortSize, mapAdtMapConstructorSize)
, TorXakis.TestSortContext.empty
  -- dependencies, yet part of interface
, module TorXakis.SortContext
, ConstructorDef
)
where
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import           Data.Maybe          (catMaybes, fromMaybe)
import           GHC.Generics        (Generic)

import           TorXakis.ContextSort
import           TorXakis.Sort
import           TorXakis.SortContext



-- | A TestSortContext instance contains all definitions to work with sort and reference thereof for test purposes
class SortContext a => TestSortContext a where
    -- | Sort Size
    --   The size of the provided 'TorXakis.Sort' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.Sort' reference.
    sortSize :: Sort -> a -> Int
    
    -- |  adt Size
    --   The size of the provided 'TorXakis.ADTDef' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef' and any related 'TorXakis.Sort' references.
    adtSize :: Ref ADTDef -> a -> Int

    -- |  constructor Size
    --   The size of the provided constructor as specified by the references to 'TorXakis.ADTDef' and 'TorXakis.ConstructorDef' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef', 'TorXakis.ConstructorDef' and any related 'TorXakis.Sort' references.
    constructorSize :: Ref ADTDef -> Ref ConstructorDef -> a -> Int

-- | An instance of 'TestSortContext'.
data ContextTestSort = ContextTestSort 
                        { basis :: ContextSort
                        , mapSortSize :: Map.Map Sort Int
                        , mapAdtMapConstructorSize :: Map.Map (Ref ADTDef) (Map.Map (Ref ConstructorDef) Int)
                        } deriving (Eq, Ord, Read, Show, Generic, Data)

-- | Constructor of empty TestSortContext
empty :: ContextTestSort
empty = ContextTestSort TorXakis.ContextSort.empty primitiveSortSize Map.empty
      where
        primitiveSortSize :: Map.Map Sort Int
        primitiveSortSize = Map.fromList $ zip [ SortBool
                                               , SortInt
                                               , SortChar
                                               , SortString
                                               , SortRegex
                                               ]
                                               (repeat 0)

instance SortContext ContextTestSort where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = addADTs as (basis ctx) >>= (\newBasis ->
                                             let
                                               newMapSortSize = addToMapSortSize as (mapSortSize ctx)
                                               newMapAdtMapConstructorSize = addToMapAdtMapConstructorSize (mapAdtMapConstructorSize ctx) newMapSortSize as
                                             in
                                                Right $ ContextTestSort  newBasis
                                                                         newMapSortSize
                                                                         newMapAdtMapConstructorSize
                                             )
      where
            addToMapAdtMapConstructorSize :: Map.Map (Ref ADTDef) (Map.Map (Ref ConstructorDef) Int)
                                          -> Map.Map Sort Int
                                          -> [ADTDef]
                                          -> Map.Map (Ref ADTDef) (Map.Map (Ref ConstructorDef) Int)
            addToMapAdtMapConstructorSize cMap sMap =
                foldl addConstructorSizes cMap
              where
                addConstructorSizes :: Map.Map (Ref ADTDef) (Map.Map (Ref ConstructorDef) Int) 
                                    -> ADTDef 
                                    -> Map.Map (Ref ADTDef) (Map.Map (Ref ConstructorDef) Int)
                addConstructorSizes iMap adef =
                    let ra = toRef adef in
                        if Map.member ra iMap 
                            then error ("Invariant violated: adding already contained ADTDef " ++ show adef)
                            else Map.insert ra (Map.fromList (map (\c -> (toRef c, getConstructorSize sMap c) ) (elemsConstructor adef) ) ) iMap

            addToMapSortSize :: [ADTDef] -> Map.Map Sort Int -> Map.Map Sort Int
            addToMapSortSize adefs defined =
                let newDefined = foldl addCurrent defined adefs
                    in if newDefined == defined 
                        then if any (`Map.notMember` newDefined) (map (SortADT . toRef) adefs)
                                then error ("Invariant violated: non constructable ADTDefs in " ++ show adefs)
                                else newDefined
                        else addToMapSortSize adefs newDefined
              where 
                addCurrent :: Map.Map Sort Int -> ADTDef -> Map.Map Sort Int
                addCurrent mp aDef = case getKnownAdtSize mp aDef of
                                        Nothing -> mp
                                        Just i  -> Map.insert (SortADT (toRef aDef)) i mp

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
                                      (fields cdef)

            getConstructorSize :: Map.Map Sort Int -> ConstructorDef -> Int
            getConstructorSize defined cdef = fromMaybe (error ("Invariant violated: unable to calculate size of ConstructorDef " ++ show cdef) )
                                                        (getKnownConstructorSize defined cdef)

instance TestSortContext ContextTestSort where
    sortSize s ctx = fromMaybe (error ("sort not contained in context " ++ show s))
                               $ Map.lookup s (mapSortSize ctx)
    
    adtSize r ctx = fromMaybe (error ("reference not contained in context " ++ show r))
                              $ Map.lookup (SortADT r) (mapSortSize ctx)

    constructorSize r c ctx = case Map.lookup r (mapAdtMapConstructorSize ctx) of
                                Nothing -> error ("ADT reference not contained in context " ++ show r)
                                Just m -> fromMaybe (error ("component reference " ++ show c ++ " not contained in ADT " ++ show r))
                                                    $ Map.lookup c m
