{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprGenSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'ValExprGen'.
-----------------------------------------------------------------------------
module TorXakis.ValExprGenSpec
(spec
)
where
import           Debug.Trace
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import           Test.QuickCheck

import           TorXakis.Sort
import           TorXakis.TestValExprContext
import           TorXakis.ValExpr
import           TorXakis.ValExprGen
import           TorXakis.VarDef

-- | min (min x) == x
prop_MkUnaryMinus_id :: Gen Bool
prop_MkUnaryMinus_id =
    -- TODO: add to context, to generate more value expressions of type int
    let ctx = empty :: MinimalTestValExprContext in do
        ve <- arbitraryValExprOfSort ctx SortInt :: Gen (ValExpr MinimalVarDef)
        return $ case mkUnaryMinus ctx ve of
                        Left e    -> trace ("\nUnexpected error in generator 1 " ++ show e) False
                        Right mve -> case mkUnaryMinus ctx mve of
                                        Left e     -> trace ("\nUnexpected error in generator 2 " ++ show e) False
                                        Right mmve -> trace ("ve = " ++ show ve ++ "\nmmve = " ++ show mmve) ve == mmve

spec :: Spec
spec =
  describe "mkUnaryMinus" $
    modifyMaxSuccess (const 25) $
    modifyMaxSize (const 8) $ 
        it "mkUnaryMinus mkUnaryMinus == id" $ property prop_MkUnaryMinus_id
