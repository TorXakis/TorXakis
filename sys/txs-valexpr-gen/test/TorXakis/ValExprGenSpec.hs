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
import qualified Data.Set as Set
import           Debug.Trace
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize)
import           Test.QuickCheck

import           TorXakis.Sort
import           TorXakis.TestValExprContext
import           TorXakis.ValExpr
import           TorXakis.ValExprGen
import           TorXakis.Value
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
                                        Right mmve -> ve == mmve

-- | a \/ not a <==> True
prop_AOrNotA :: Gen Bool
prop_AOrNotA =
    -- TODO: add to context, to generate more value expressions of type Bool
    let ctx = empty :: MinimalTestValExprContext in do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen (ValExpr MinimalVarDef)
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkOr ctx (Set.fromList [a,na]) of
                                        Left e  -> trace ("\nUnexpected error with mkOr " ++ show e) False
                                        Right v -> case view v of 
                                                        Vconst (Cbool True) -> True
                                                        x                   -> trace ("\nWrong value = " ++ show x) False

spec :: Spec
spec = do
            describe "mkUnaryMinus" $
                modifyMaxSuccess (const 100) $
                modifyMaxSize (const 20) $ 
                it "mkUnaryMinus mkUnaryMinus == id" $ property prop_MkUnaryMinus_id
            describe "Or" $
                modifyMaxSuccess (const 100) $
                modifyMaxSize (const 20) $ 
                it "a \\/ not a == True" $ property prop_AOrNotA
