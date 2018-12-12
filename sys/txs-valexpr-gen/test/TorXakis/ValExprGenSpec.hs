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
import           TorXakis.Value
import           TorXakis.VarDef

propertyInContext  :: (MinimalTestValExprContext MinimalVarDef -> Gen Bool) -> Gen Bool
propertyInContext prop = 
    -- TODO: add to context, to generate more value expressions of a given type
    let ctx = empty :: MinimalTestValExprContext MinimalVarDef in
        prop ctx

-- | min (min x) == x
prop_MkUnaryMinus_id :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_MkUnaryMinus_id ctx = do
        ve <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        return $ case mkUnaryMinus ctx ve of
                        Left e    -> trace ("\nUnexpected error in generator 1 " ++ show e) False
                        Right mve -> case mkUnaryMinus ctx mve of
                                        Left e     -> trace ("\nUnexpected error in generator 2 " ++ show e) False
                                        Right mmve -> ve == mmve

-- | a \/ not a <==> True
prop_AOrNotA :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_AOrNotA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpr
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkOr ctx [a,na] of
                                        Left e  -> trace ("\nUnexpected error with mkOr " ++ show e) False
                                        Right v -> case view v of 
                                                        Vconst (Cbool True) -> True
                                                        x                   -> trace ("\nWrong value = " ++ show x) False

-- | not a => a <==> a
prop_NotAImpliesAEqualsA :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_NotAImpliesAEqualsA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpr
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkImplies ctx na a of
                                        Left e  -> trace ("\nUnexpected error with mkImplies " ++ show e) False
                                        Right v -> a == v

-- | a => not a <==> not a
prop_AImpliesNotAEqualsNotA :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_AImpliesNotAEqualsNotA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpr
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkImplies ctx a na of
                                        Left e  -> trace ("\nUnexpected error with mkImplies " ++ show e) False
                                        Right v -> na == v

-- | a >= b <==> b <= a
prop_GELE :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_GELE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        return $ mkGE ctx a b == mkLE ctx b a

-- | a > b <==> b < a
prop_GTLT :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_GTLT ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        return $ mkGT ctx a b == mkLT ctx b a

-- | a > b <==> not (a <= b)
prop_GTNotLE :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_GTNotLE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        return $ case mkLE ctx a b of
                    Left e   -> trace ("\nUnexpected error with mkLE " ++ show e) False
                    Right le -> mkGT ctx a b == mkNot ctx le

-- | a < b <==> not (a >= b)
prop_LTNotGE :: MinimalTestValExprContext MinimalVarDef -> Gen Bool
prop_LTNotGE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpr
        return $ case mkGE ctx a b of
                    Left e   -> trace ("\nUnexpected error with mkGE " ++ show e) False
                    Right ge -> mkLT ctx a b == mkNot ctx ge

spec :: Spec
spec = do
            describe "mkUnaryMinus" $
                modifyMaxSuccess (const 100) $
                modifyMaxSize (const 10) $ 
                it "mkUnaryMinus mkUnaryMinus == id" $ property (propertyInContext prop_MkUnaryMinus_id)
            describe "Or" $
                modifyMaxSuccess (const 100) $
                modifyMaxSize (const 10) $ 
                it "a \\/ not a == True" $ property (propertyInContext prop_AOrNotA)
            describe "Implies" $
                modifyMaxSuccess (const 100) $
                modifyMaxSize (const 10) $ do
                    it "not a => a <==> a"     $ property (propertyInContext prop_NotAImpliesAEqualsA)
                    it "a => not a <==> not a" $ property (propertyInContext prop_AImpliesNotAEqualsNotA)
            describe "Comparisons" $ 
                modifyMaxSuccess (const 100) $
                modifyMaxSize (const 10) $ do
                    it "a >= b <==> b <= a"      $ property (propertyInContext prop_GELE)
                    it "a > b <==> b < a"        $ property (propertyInContext prop_GTLT)
                    it "a > b <==> not (a <= b)" $ property (propertyInContext prop_GTNotLE)
                    it "a < b <==> not (a >= b)" $ property (propertyInContext prop_LTNotGE)