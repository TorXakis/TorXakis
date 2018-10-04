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
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as T
import           Test.QuickCheck

import TorXakis.Sort
import TorXakis.Value
import TorXakis.TestSortContext
import TorXakis.SortGenContext

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
        case mapConstructorDefSize ctx a of
            Left e -> error ("ADTDef " ++ show a ++ " not in context " ++ show e)
            Right mpSize -> let availableCstr = Map.keys (Map.filter (<n) mpSize)
                              in case availableCstr of
                                    [] -> error ("Unexpected: No Constructor available for " ++ show a)
                                    _  -> do
                                            selected <- elements availableCstr
                                            let adtDef = fromMaybe (error ("ADTDef " ++ show a ++ " not in context")) 
                                                                   (Map.lookup a (adtDefs ctx))
                                                cstrDef = fromMaybe (error ("cstrDef " ++ show selected ++ " not in ADT " ++ show adtDef))
                                                                    (Map.lookup selected ((constructors . viewADTDef) adtDef))
                                              in do
                                                fs <- mapM (resize (n-1) . arbitraryValueOfSort ctx . sort) ((fields . viewConstructorDef) cstrDef)
                                                return $ Ccstr a selected fs
