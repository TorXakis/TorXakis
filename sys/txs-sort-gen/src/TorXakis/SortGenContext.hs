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

arbitraryFields :: TestSortContext a => a -> Gen [FieldDef]
arbitraryFields ctx =
    do
        nameGens <- arbitrary :: Gen (Set.Set NameGen)
        sorts <- vectorOf (Set.size nameGens) (arbitrarySort ctx)
        let listNames = map unNameGen (Set.toList nameGens)
           in
              return $ zipWith FieldDef listNames sorts

arbitraryConstructors :: TestSortContext a => a -> Gen [ConstructorDef]
arbitraryConstructors ctx =
    do
        NameGen cName <- arbitrary :: Gen NameGen
        cNameGens <- arbitrary :: Gen (Set.Set NameGen)
        let cNames = Set.insert cName (Set.map unNameGen cNameGens)
        fs <- arbitraryFields ctx
        mkConstructorDefs (Set.toList cNames) fs
    where
        mkConstructorDefs :: [Name] -> [FieldDef] -> Gen [ConstructorDef]
        mkConstructorDefs []        _ = error "Non-empty list expected"
        mkConstructorDefs [cn]     fs = return $ case mkConstructorDef cn fs of
                                                        Left  _ -> error "error in generator: creating valid ConstructorDef - singleton"
                                                        Right x -> [x]
        mkConstructorDefs (cn:cns) fs =
            do
                nf <- choose (0, length fs)
                let (hfs,tfs) = splitAt nf fs in do
                    tl <- mkConstructorDefs cns tfs
                    return $ case mkConstructorDef cn hfs of
                                Left  _ -> error "error in generator: creating valid ConstructorDef - list"
                                Right x -> x : tl

-- | Generate ADTDefs that extend this context
arbitraryADTDefs :: TestSortContext a => a -> Gen [ADTDef]
arbitraryADTDefs ctx =
    do
        NameGen aName <- arbitrary :: Gen NameGen
        aNameGens <- arbitrary :: Gen (Set.Set NameGen)
        let aNames = Set.insert aName (Set.map unNameGen aNameGens) in
            mapM toADTDef (Set.toList aNames)
    where
        toADTDef :: Name -> Gen ADTDef
        toADTDef n = do
                        cs <- arbitraryConstructors ctx
                        return $ case mkADTDef n cs of
                            Left  _ -> error "error in generator: creating valid ADTDef"
                            Right x -> x