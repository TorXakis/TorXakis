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
import qualified Data.HashMap       as Map
import           Data.Maybe         (mapMaybe)
import qualified Data.Text          as T
import           Test.QuickCheck

import TorXakis.Distribute
import TorXakis.Sort
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
arbitraryValue :: TestSortContext a => a -> Gen Value
arbitraryValue ctx = 
    do
        s <- arbitrarySort ctx
        arbitraryValueOfSort ctx s

-- | generate a random value of the given sort within a context.
-- ANY is excluded
arbitraryValueOfSort :: TestSortContext a => a -> Sort -> Gen Value
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
        case lookupADTDef ctx a of
            Nothing      -> error ("ADTDef " ++ show a ++ " not in context ")
            Just adtDef  -> let availableCstr = mapMaybe (\(r,d) -> case constructorSize ctx a r of
                                                                        Left e -> error ("ADTDef and constructor in context, yet no size - " ++ show e)
                                                                        Right v -> if v <= n
                                                                                    then Just (v, r, d)
                                                                                    else Nothing) 
                                                          (Map.toList (constructors adtDef))
                              in case availableCstr of
                                    [] -> error ("Unexpected: No Constructor available for " ++ show a)
                                    _  -> do
                                            (minSize, cstrRef, cstrDef) <- elements availableCstr
                                            let cFields = fields cstrDef
                                              in do
                                                additionalComplexity <- distribute (n-minSize) (length cFields)
                                                let sFields = map (\f -> case sortSize ctx (sort f) of
                                                                            Left e -> error ("Sort of Field not defined in context" ++ show e)
                                                                            Right v -> v
                                                                  ) 
                                                                  cFields
                                                    aFields = zipWith (+) sFields additionalComplexity
                                                  in do
                                                    fs <- mapM (\(c,f) -> resize c (arbitraryValueOfSort ctx (sort f))) $ zip aFields cFields
                                                    return $ Ccstr a cstrRef fs