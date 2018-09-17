{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.SortGenContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- With Context Generate a 'TorXakis.Sort'
-----------------------------------------------------------------------------
module TorXakis.SortGenContext
(-- * generators within context
  arbitrarySort
, arbitraryADTDefs
)
where
import qualified Data.HashMap        as Map
import qualified Data.Set            as Set
import           Test.QuickCheck

import           TorXakis.Name
import           TorXakis.Sort

import           TorXakis.NameGen
import           TorXakis.TestSortContext

-- | Select an arbitrary Sort from the given context
arbitrarySort :: TestSortContext a => a -> Gen Sort
arbitrarySort ctx =
    do
        n <- getSize
        let availableSort = Map.keys (Map.filter (<=n) (getMapSortSize ctx)) in
            elements availableSort

arbitraryFields :: [Sort] -> [Name] -> Gen [FieldDef]
arbitraryFields ss ns =
    do
        sorts <- vectorOf (length ns) (elements ss)
        return $ zipWith FieldDef ns sorts

arbitraryConstructors :: [Sort] -> [Sort] -> Gen [ConstructorDef]
arbitraryConstructors defined add =
    do
        NameGen cName <- arbitrary :: Gen NameGen
        cNameGens <- arbitrary :: Gen (Set.Set NameGen)
        let cNames = Set.toList (Set.insert cName (Set.map unNameGen cNameGens))
        
        fnameGens <- arbitrary :: Gen (Set.Set NameGen)
        let fNames = Set.toList (Set.map unNameGen fnameGens)
        
        mkConstructorDefs cNames fNames
    where
        mkConstructorDefs :: [Name] -> [Name] -> Gen [ConstructorDef]
        mkConstructorDefs []       _  = error "Non-empty list expected"
        mkConstructorDefs [cn]     ns =
            do
                fs <- arbitraryFields defined ns
                return $ case mkConstructorDef cn fs of
                            Left  _ -> error "error in generator: creating valid ConstructorDef - singleton"
                            Right x -> [x]
        mkConstructorDefs (cn:cns) ns =
            do
                n <- choose (0, length ns)
                let (hns,tns) = splitAt n ns in do
                    tl <- mkConstructorDefs cns tns
                    hfs <- arbitraryFields (defined ++ add) hns
                    return $ case mkConstructorDef cn hfs of
                                Left  _ -> error "error in generator: creating valid ConstructorDef - list"
                                Right x -> x : tl

-- | Generate ADTDefs that extend this context
arbitraryADTDefs :: TestSortContext a => a -> Gen [ADTDef]
arbitraryADTDefs ctx =
    do
        nameGens <- arbitrary :: Gen (Set.Set NameGen)
        let names :: Set.Set Name
            names = Set.map unNameGen nameGens

            uniqueNames :: [Name]
            uniqueNames = Set.toList (Set.difference names (Set.fromList (map getName (Map.elems (adtDefs ctx)))))

            toADTDefs :: [Name] -> [Sort] -> Gen [ADTDef]
            toADTDefs [] _ = return []
            toADTDefs uNames@(n:ns) s = 
                do
                    cs <- arbitraryConstructors s (map (SortADT . RefByName) uNames)
                    aDefs <- toADTDefs ns (SortADT (RefByName n) : s)
                    return $ case mkADTDef n cs of
                        Left  _    -> error "error in generator: creating valid ADTDef"
                        Right aDef -> aDef : aDefs
          in do
            toADTDefs uniqueNames (Map.keys (getMapSortSize ctx))
