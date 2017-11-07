{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.String.Utils   as Utils
import qualified Data.Text           as T
import           System.Exit

--import qualified Debug.Trace as Trace

import           Test.QuickCheck

import           TxsAlex
import           TxsDefs
import           TxsHappy

import           Sigs

-- QuickCheck Extension

subset :: Ord a => Set.Set a -> Gen (Set.Set a)
subset s =
    let l = Set.toList s in
        do
            n <- choose (0, length l)
            shuffled <- shuffle l
            return $ Set.fromList (take n shuffled)

neSubset :: Ord a => Set.Set a -> Gen (Set.Set a)
neSubset s =
    if Set.size s == 0
    then error "neSubset: non empty subset of empty set"
    else
        let l = Set.toList s in
            do
                n <- choose (1, length l)
                shuffled <- shuffle l
                return $ Set.fromList (take n shuffled)

disjointNESubsets :: Ord a => Set.Set a -> Gen [Set.Set a]
disjointNESubsets s =
    if Set.size s == 0
        then return []
        else do
            sb <- neSubset s
            let remaining = Set.difference s sb
            rest <- disjointNESubsets remaining
            return (sb : rest)

splitInSubsets :: Ord a => Int -> Set.Set a -> Gen [Set.Set a]
--splitInSubsets 0 s = return []
splitInSubsets 1 s = return [s]
splitInSubsets m s = do
    n <- choose (0, Set.size s)
    if n == 0
        then do
            rest <- splitInSubsets (m-1) s
            return (Set.empty : rest)
        else do
            shuffled <- shuffle (Set.toList s)
            let sb = Set.fromList (take n shuffled)
            rest <- splitInSubsets (m-1) (Set.difference s sb)
            return (sb : rest)

-- Define own type & generators

smallIds :: [String]
smallIds = [ "aap"
           , "a123"
           , "a_"
           , "geen"
           , "gEEN"
           , "null"
           , "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"]

newtype SmallId = SmallId String
     deriving (Eq,Ord,Read,Show)

instance Arbitrary SmallId where
    arbitrary = elements (map SmallId smallIds)

genUniqueSmallIds :: Gen (Set.Set SmallId)
genUniqueSmallIds = subset (Set.fromList (map SmallId smallIds) )

capIds :: [String]
capIds = [ "Aap"
         , "A123"
         , "A_"
         , "GEEN"
         , "Geen"
         , "Int"
         , "Boolean"
         , "Insert"
         , "NULL"
         , "Null"
         , "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
         ]
         -- "Bool" not accepted by TorXakis in structure due to cvc4 bug
         -- "REGEX" is a reserved capId


newtype CapId = CapId String
     deriving (Eq,Ord,Read,Show)

instance Arbitrary CapId where
    arbitrary = elements (map CapId capIds)

genUniqueCapIds :: Gen (Set.Set CapId)
genUniqueCapIds = subset (Set.fromList (map CapId capIds) )

predefSort :: [String]
predefSort = [ "Int"
-- TODO CVC4 bug solved; re-add Bool
             --, "Bool" not accepted by TorXakis in structure due to cvc4 bug
             , "String"
             , "Regex"]

genUniqueUserDefinedSortNames :: Gen (Set.Set CapId)
genUniqueUserDefinedSortNames = subset (Set.map CapId (Set.difference (Set.fromList capIds) (Set.fromList predefSort)))

type TypedElement = ([SmallId],CapId)
type TypedElements = [ TypedElement ]
type Constructor = (CapId, TypedElements)
type Constructors = [ Constructor ]

data GenSortDef = GenSortDef CapId Constructors
    deriving (Eq,Ord,Read,Show)

instance Arbitrary GenSortDef where
    arbitrary = do
        sortName <- elements (map CapId (Set.toList (Set.difference (Set.fromList capIds) (Set.fromList predefSort) ) ) )
        genGenSortDef sortName (Set.insert sortName (Set.map CapId (Set.fromList predefSort) ) )

genGenSortDef :: CapId -> Set.Set CapId -> Gen GenSortDef
genGenSortDef sortName sortNames = do
    constrNames <- genUniqueCapIds
    fieldNames <- genUniqueSmallIds
    fieldNamesConstr <- splitInSubsets (Set.size constrNames) fieldNames
    fieldNamesConstrSort <- mapM disjointNESubsets fieldNamesConstr
    typedElements <- mapM (mapM (\x -> do elem <- elements (Set.toList sortNames)
                                          return (Set.toList x, elem) )
                          )
                          fieldNamesConstrSort
    return $ GenSortDef sortName (zip (Set.toList constrNames) typedElements)

genGenSortDefs :: Gen [GenSortDef]
genGenSortDefs = do
    -- generate unique names for sortdefs
    sortNames <- genUniqueCapIds
    mapM (`genGenSortDef` sortNames) (Set.toList sortNames)

createCstrId :: CapId -> TypedElements -> CapId -> String
createCstrId (CapId cstrName) fields (CapId _sortDefName) =
    cstrName ++ " { "
             ++ Utils.join " ; " (map (\(field,CapId t) -> Utils.join " , " (map (\(SmallId f) -> f) field) ++ " :: " ++ t) fields)
             ++ " }"

createGenSortDef :: GenSortDef -> String
createGenSortDef (GenSortDef sdn@(CapId sortDefName) constrs) =
   "TYPEDEF " ++ sortDefName ++ " ::=\n\t  " ++
       Utils.join "\n\t| " (map (\(constrName, fields) -> createCstrId constrName fields sdn) constrs )
   ++ "\nENDDEF"

toTorXakisDefs :: (Int, TxsDefs, Sigs VarId) -> TxsDefs
toTorXakisDefs (_, b, _) = b

parseTorXakis :: String -> TxsDefs
parseTorXakis txt = -- Trace.trace ("txt = " ++ txt) $
                        let parserOutput = txsParser (txsLexer txt) in
                            -- Trace.trace ("parser output = " ++ show(parserOutput))
                                toTorXakisDefs parserOutput

prop_defined :: GenSortDef -> Bool
prop_defined sf@(GenSortDef (CapId n) _) =
    1 == length [x | x@(SortId n' _, SortDef{}) <- Map.toList ( sortDefs (parseTorXakis (createGenSortDef sf)))
                   , T.unpack n' == n
                ]

return []

main :: IO ()
main = do
    result <- $quickCheckAll
    if result
        then exitSuccess
        else exitFailure
