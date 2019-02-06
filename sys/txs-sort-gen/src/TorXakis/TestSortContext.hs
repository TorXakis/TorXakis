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
, ContextTestSort(..)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import           Data.Maybe          (catMaybes, fromMaybe)
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort


-- | A TestSortContext instance contains all definitions to work with sort and reference thereof for test purposes
class SortContext a => TestSortContext a where
    -- | Sort Size
    --   A size of complexity (indicated by an 'Int') is returned when the following constraint is satisfied:
    --
    --   * The context contains the 'TorXakis.Sort' reference.
    --
    --   Otherwise an error is return. The error reflects the violations of the formentioned constraint.
    sortSize :: a -> Sort -> Either Error Int
    
    -- |  adt Size
    --   A size of complexity (indicated by an 'Int') is returned when the following constraint is satisfied:
    --
    --   * The context contains the 'TorXakis.ADTDef' reference.
    --
    --   Otherwise an error is return. The error reflects the violations of the formentioned constraint.
    adtSize :: a -> RefByName ADTDef -> Either Error Int

    -- |  constructor Size
    --   A size of complexity (indicated by an 'Int') is returned when the following constraints are satisfied:
    --
    --   * The context contains the 'TorXakis.ADTDef' reference.
    --
    --   * The context contains the 'TorXakis.ConstructorDef' reference for the referred 'TorXakis.ADTDef'.
    --
    --   Otherwise an error is return. The error reflects the violations of any of the formentioned constraints.
    constructorSize :: a -> RefByName ADTDef -> RefByName ConstructorDef -> Either Error Int

-- | An instance of 'TestSortContext'.
data ContextTestSort = ContextTestSort 
                        { basis :: ContextSort
                        , mapSortSize :: Map.Map Sort Int
                        , mapAdtMapConstructorSize :: Map.Map (RefByName ADTDef) (Map.Map (RefByName ConstructorDef) Int)
                        } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortReadContext ContextTestSort where
    memberSort   = memberSort . basis

    memberADT = memberADT . basis

    lookupADT = lookupADT . basis

    elemsADT  = elemsADT . basis

instance SortContext ContextTestSort where
    empty = ContextTestSort empty primitiveSortSize Map.empty
      where
        primitiveSortSize :: Map.Map Sort Int
        primitiveSortSize = Map.fromList $ zip [ SortBool
                                               , SortInt
                                               , SortChar
                                               , SortString
                                               , SortRegex
                                               ]
                                               (repeat 0)

    addADTs context as = addADTs (basis context) as >>= (\newBasis ->
                                             let
                                               newMapSortSize = addToMapSortSize (mapSortSize context) as 
                                               newMapAdtMapConstructorSize = addToMapAdtMapConstructorSize (mapAdtMapConstructorSize context) newMapSortSize as 
                                             in
                                                Right $ ContextTestSort  newBasis
                                                                         newMapSortSize
                                                                         newMapAdtMapConstructorSize
                                             )
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
                    let ra = toRefByName adef in
                        if Map.member ra iMap 
                            then error ("Invariant violated: adding already contained ADTDef " ++ show adef)
                            else Map.insert ra (Map.fromList (map (\c -> (toRefByName c, getConstructorSize sMap c) ) (elemsConstructor adef) ) ) iMap

            addToMapSortSize :: Map.Map Sort Int -> [ADTDef] -> Map.Map Sort Int
            addToMapSortSize defined adefs =
                let newDefined = foldl addCurrent defined adefs
                    in if newDefined == defined 
                        then if any (`Map.notMember` newDefined) (map (SortADT . toRefByName) adefs)
                                then error ("Invariant violated: non constructable ADTDefs in " ++ show adefs)
                                else newDefined
                        else addToMapSortSize newDefined adefs
              where 
                addCurrent :: Map.Map Sort Int -> ADTDef -> Map.Map Sort Int
                addCurrent mp aDef = case getKnownAdtSize mp aDef of
                                        Nothing -> mp
                                        Just i  -> Map.insert (SortADT (toRefByName aDef)) i mp

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
    sortSize ctx s = case Map.lookup s (mapSortSize ctx) of
                        Just i  -> Right i
                        Nothing -> Left $ Error ("sort not contained in context " ++ show s)
    
    adtSize ctx r = case Map.lookup (SortADT r) (mapSortSize ctx) of
                        Just i  -> Right i
                        Nothing -> Left $ Error ("reference not contained in context " ++ show r)

    constructorSize ctx r c = case Map.lookup r (mapAdtMapConstructorSize ctx) of
                                Nothing -> Left $ Error ("ADT reference not contained in context " ++ show r)
                                Just m -> case Map.lookup c m of
                                            Nothing -> Left $ Error ("component reference " ++ show c ++ " not contained in ADT " ++ show r)
                                            Just i -> Right i
