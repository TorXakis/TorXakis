{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ValueGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Generators associated with the Value package
-----------------------------------------------------------------------------
module TorXakis.ValueGen
( arbitraryValue
, arbitraryValueOfSort
)
where
import           Data.Char          (chr)
import           Data.Maybe         (mapMaybe)
import qualified Data.Text          as T
import           Test.QuickCheck

import TorXakis.Distribute
import TorXakis.Name
import TorXakis.Sort
import TorXakis.SortContext
import TorXakis.Value
import TorXakis.TestSortContext
import TorXakis.SortGenContext

-- | TODO: decide on name (generator versus arbitrary instance)
-- First Sort and then context or vice versa?

-- | TODO: make TestValueContext with generatorMap (make more identical to ValExpr)


-- | generate a char within the allowed range
arbitraryChar :: Gen Char
arbitraryChar = chr <$> choose (0, 255)


-- | generate a random value within a context.
arbitraryValue :: TestSortContext c => c -> Gen Value
arbitraryValue ctx = 
    do
        s <- arbitrarySort ctx
        arbitraryValueOfSort ctx s

-- | generate a random value of the given sort within a context.
-- ANY is excluded
arbitraryValueOfSort :: TestSortContext c => c -> Sort -> Gen Value
arbitraryValueOfSort _   SortBool   = Cbool <$> arbitrary
arbitraryValueOfSort _   SortInt    = Cint <$> arbitrary
arbitraryValueOfSort _   SortChar   = Cchar <$> arbitraryChar
arbitraryValueOfSort _   SortString = 
    do
        s <- listOf arbitraryChar
        return $ Cstring (T.pack s)
arbitraryValueOfSort _   SortRegex = 
    do
        s <- listOf arbitraryChar
        return $ Cregex (T.pack s)
arbitraryValueOfSort ctx (SortADT a) = 
    do
        n <- getSize
        case lookupADT (toName a) ctx of
            Nothing      -> error ("ADTDef " ++ show a ++ " not in context ")
            Just adtDef  -> let availableCstr = mapMaybe (\c -> let v = constructorSize a (toConstructorRef c) ctx in
                                                                    if v <= n
                                                                        then Just (v, c)
                                                                        else Nothing
                                                         )
                                                         (elemsConstructor adtDef)
                              in case availableCstr of
                                    [] -> error ("Unexpected: No Constructor available for " ++ show a)
                                    _  -> do
                                            (minSize, cstrDef) <- elements availableCstr
                                            let cFields = elemsField cstrDef
                                              in do
                                                additionalComplexity <- distribute (n-minSize) (length cFields)
                                                let sFields = map (flip sortSize ctx . sort) cFields
                                                    aFields = zipWith (+) sFields additionalComplexity
                                                  in do
                                                    fs <- mapM (\(c,f) -> resize c (arbitraryValueOfSort ctx (sort f))) $ zip aFields cFields
                                                    return $ Ccstr a (toConstructorRef cstrDef) fs
  where
        toConstructorRef :: ConstructorDef -> RefByName ConstructorDef
        toConstructorRef = RefByName . constructorName