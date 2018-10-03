{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ValExprGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Generators associated with the Value Expression package
-----------------------------------------------------------------------------
module TorXakis.ValExprGen
( arbitraryValExpr
, arbitraryValExprOfSort
)
where
import           Test.QuickCheck

import           TorXakis.Sort
import           TorXakis.SortGenContext
import           TorXakis.TestValExprContext
import           TorXakis.ValExpr
import           TorXakis.ValueGen


-- | generate a random value expression within a context.
arbitraryValExpr :: TestValExprContext a v => a -> Gen (ValExpr v)
arbitraryValExpr ctx = 
    do
        s <- arbitrarySort ctx
        arbitraryValExprOfSort ctx s

-- | generate a random value expression of the given sort within a context.
arbitraryValExprOfSort :: TestValExprContext a v => a -> Sort -> Gen (ValExpr v)
                                 -- for now only constants
arbitraryValExprOfSort ctx s = do
    val <- arbitraryValueOfSort ctx s
    case mkConst ctx val of
        Left e -> error ("Unexpected error in generator arbitraryValExprOfSort: " ++ show e)
        Right v -> return v