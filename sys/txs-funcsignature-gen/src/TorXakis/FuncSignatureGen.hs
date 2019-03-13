{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.FuncSignatureGen
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Generators associated with the Value Expression package
-----------------------------------------------------------------------------
module TorXakis.FuncSignatureGen
( arbitraryFuncSignature
)
where
import           Test.QuickCheck

import           TorXakis.FuncSignature
import           TorXakis.NameGen
import           TorXakis.SortGenContext
import           TorXakis.TestSortContext

arbitrarySignature :: TestSortContext a => a -> Gen (Name, [Sort], Sort)
arbitrarySignature ctx =
    do
        n <- arbitrary :: Gen NameGen
        ps <- listOf (arbitrarySort ctx)
        r <- arbitrarySort ctx
        if isReservedPrefixFunctionSignature ctx (unNameGen n) ps r
            then arbitrarySignature ctx -- or should we call QuickCheck's discard?
            else return (unNameGen n, ps, r)

-- | generate a random function signature within a test sort context.
-- test sort context is needed to link complexity/size to function signature for termination
arbitraryFuncSignature :: TestSortContext a => a -> Gen FuncSignature
arbitraryFuncSignature ctx = 
    do
        (n, ps, r) <- arbitrarySignature ctx
        case mkPrefixFuncSignature ctx n ps r of
            Left e  -> error ("arbitraryFuncSignature  - constructor failed\n" ++ show e)
            Right x -> return x

-- TODO: make arbitrary that doesn't generate predefined Function (takeWhile, dropWhileNot etc).