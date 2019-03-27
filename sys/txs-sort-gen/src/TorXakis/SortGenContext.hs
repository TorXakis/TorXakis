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
, arbitraryTestSortContext
  -- dependencies, yet part of interface
, Sort
, ADTDef
, ContextTestSort
)
where
import           Data.List
import qualified Data.Set            as Set
import           Test.QuickCheck

import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext

import           TorXakis.ContextTestSort
import           TorXakis.NameGen
import           TorXakis.TestSortContext

-- | Select an arbitrary Sort from the given context
arbitrarySort :: TestSortContext c => c -> Gen Sort
arbitrarySort ctx =
    do
        n <- getSize
        let availableSort = filter (\s -> sortSize s ctx <= n)
                                   (elemsSort ctx)
         in
            case availableSort of
                [] -> error ("No Sort in context with complexity at most " ++ show n)
                _  -> elements availableSort

arbitraryField :: [TxsString] -> [Sort] -> Name -> Gen FieldDef
arbitraryField isCstrs ss n = do
        s <- elements availableSort
        return $ FieldDef n s
  where
    -- | prevent signature clashes between field access (with sort Bool) and is-made-by-constructor functions.
    availableSort :: [Sort]
    availableSort = if TxsString (TorXakis.Name.toText n) `elem` isCstrs
                        then Data.List.delete SortBool ss
                        else ss

arbitraryFields :: [TxsString] -> [Sort] -> [Name] -> Gen [FieldDef]
arbitraryFields isCstrs ss = mapM (arbitraryField isCstrs ss)

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
        mkConstructorDefs cs =
            mkConstructorDefs' (map (txsNameIsConstructor . TorXakis.Name.toText) cs) cs
        mkConstructorDefs' :: [TxsString] -> [Name] -> [Name] -> Gen [ConstructorDef]
        mkConstructorDefs' _       []       _  = error "Non-empty list expected"
        mkConstructorDefs' isCstrs [cn]     ns =
            do
                fs <- arbitraryFields isCstrs defined ns
                return $ case mkConstructorDef cn fs of
                            Left  _ -> error "error in generator: creating valid ConstructorDef - singleton"
                            Right x -> [x]
        mkConstructorDefs' isCstrs (cn:cns) ns =
            do
                n <- choose (0, Data.List.length ns)
                let (hns,tns) = splitAt n ns in do
                    tl <- mkConstructorDefs' isCstrs cns tns
                    hfs <- arbitraryFields isCstrs (defined ++ add) hns
                    return $ case mkConstructorDef cn hfs of
                                Left  _ -> error "error in generator: creating valid ConstructorDef - list"
                                Right x -> x : tl

-- | Generate ADTDefs that extend this context
arbitraryADTDefs :: TestSortContext c => c -> Gen [ADTDef]
arbitraryADTDefs ctx =
    do
        nameGens <- arbitrary :: Gen (Set.Set NameGen)
        let names :: Set.Set Name
            names = Set.map unNameGen nameGens

            uniqueNames :: [Name]
            uniqueNames = Set.toList (Set.difference names (Set.fromList (map getName (elemsADT ctx))))

            toADTDefs :: [Name] -> [Sort] -> Gen [ADTDef]
            toADTDefs [] _ = return []
            toADTDefs uNames@(n:ns) s = 
                do
                    cs <- arbitraryConstructors s (map (SortADT . RefByName) uNames)
                    aDefs <- toADTDefs ns ( (SortADT . RefByName) n : s)
                    return $ case mkADTDef n cs of
                        Left  e    -> error ("error in generator: creating valid ADTDef, yet " ++ show e)
                        Right aDef -> aDef : aDefs
          in
            toADTDefs uniqueNames (elemsSort ctx)

-- | generate an arbitrary Test Sort Context
arbitraryTestSortContext :: Gen ContextTestSort
arbitraryTestSortContext =
        let emp = TorXakis.ContextTestSort.empty in do
            incr <- arbitraryADTDefs emp
            case addADTs incr emp of
                Left e    -> error ("arbitraryTestSortContext: Invalid generator - " ++ show e)
                Right ctx -> return ctx