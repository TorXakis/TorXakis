{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TestValExprContextSpec
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Test specifications for 'ValExprGen'.
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns         #-}
module TorXakis.TestValExprContextSpec
(spec
)
where
import           Debug.Trace
import qualified Data.Set               as Set
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck

import           TorXakis.ContextTestValExpr
import           TorXakis.PrettyPrint
import           TorXakis.Sort
import           TorXakis.TestValExprContext
import           TorXakis.ValExpr
import           TorXakis.Value

-- How to use generated context more efficiently
-- how to check same property multiple times within generated context?
-- replicating the property causes QuickCheck to give up 
-- code:
--    value <- mapM prop (replicate 20 ctx)
--    return (and value)
-- since:
-- divide and modulo discard zero when it is generated for the divisor -> too many discard cause qive up!
propertyInContext  :: (ContextTestValExpr -> Gen Bool) -> Gen Bool
propertyInContext prop = do
    ctx <- arbitraryContextTestValExpr
    prop ctx

-- | min (min x) == x
prop_MkUnaryMinus_id :: TestValExprContext a => a -> Gen Bool
prop_MkUnaryMinus_id ctx = do
        ve <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        -- trace   ("\nValExpr:\n" ++ show (prettyPrint (Options True True) ctx ve)) $
        return $ case mkUnaryMinus ctx ve of
                      Left e    -> trace ("\nUnexpected error in generator 1 " ++ show e) False
                      Right mve -> case mkUnaryMinus ctx mve of
                                        Left e     -> trace ("\nUnexpected error in generator 2 " ++ show e) False
                                        Right mmve -> (ve == mmve) || trace ("\nValue =\n" ++ show (prettyPrint (Options True True) ctx ve) ++
                                                                             "\nleads to wrong value =\n" ++ show (prettyPrint (Options True True) ctx mmve)) False

-- | a \/ not a <==> True
prop_AOrNotA :: TestValExprContext a => a -> Gen Bool
prop_AOrNotA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpression
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkOr ctx [a,na] of
                                        Left e  -> trace ("\nUnexpected error with mkOr " ++ show e) False
                                        Right v -> case TorXakis.ValExpr.view v of 
                                                        Vconst (TorXakis.Value.view -> Cbool True)  -> True
                                                        x                                           -> trace ("\nValue =\n" ++ show (prettyPrint (Options True True) ctx a) ++
                                                                                                              "\nleads to wrong value =\n" ++ show (prettyPrint (Options True True) ctx x)) False

-- | not a => a <==> a
prop_NotAImpliesAEqualsA :: TestValExprContext a => a -> Gen Bool
prop_NotAImpliesAEqualsA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpression
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkImplies ctx na a of
                                        Left e  -> trace ("\nUnexpected error with mkImplies " ++ show e) False
                                        Right v -> a == v

-- | a => not a <==> not a
prop_AImpliesNotAEqualsNotA :: TestValExprContext a => a -> Gen Bool
prop_AImpliesNotAEqualsNotA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpression
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkImplies ctx a na of
                                        Left e  -> trace ("\nUnexpected error with mkImplies " ++ show e) False
                                        Right v -> na == v

-- | a >= b <==> b <= a
prop_GELE :: TestValExprContext a => a -> Gen Bool
prop_GELE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        return $ mkGE ctx a b == mkLE ctx b a

-- | a > b <==> b < a
prop_GTLT :: TestValExprContext a => a -> Gen Bool
prop_GTLT ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        return $ mkGT ctx a b == mkLT ctx b a

-- | a > b <==> not (a <= b)
prop_GTNotLE :: TestValExprContext a => a -> Gen Bool
prop_GTNotLE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        --trace (  "a = " ++ show (prettyPrint (Options True True) ctx a ) ++ 
        --       "\nb = " ++ show (prettyPrint (Options True True) ctx b ) ) $
        return $ case mkLE ctx a b of
                      Left e   -> trace ("\nUnexpected error with mkLE " ++ show e) False
                      Right le -> mkGT ctx a b == mkNot ctx le

-- | a < b <==> not (a >= b)
prop_LTNotGE :: TestValExprContext a => a -> Gen Bool
prop_LTNotGE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        --trace (  "a = " ++ show (prettyPrint (Options True True) ctx a ) ++ 
        --       "\nb = " ++ show (prettyPrint (Options True True) ctx b ) ) $
        return $ case mkGE ctx a b of
                      Left e   -> trace ("\nUnexpected error with mkGE " ++ show e) False
                      Right ge -> mkLT ctx a b == mkNot ctx ge

prop_ASortUsed :: TestValExprContext a => a -> Gen Bool
prop_ASortUsed ctx = do
    e <- arbitraryValExpr ctx :: Gen ValExpression
    return $ not (Set.null (usedSorts ctx e))

spec :: Spec
spec = modifyMaxSuccess (const 100) $ do
            describe "mkUnaryMinus" $
                    it "mkUnaryMinus mkUnaryMinus == id" $ property (propertyInContext prop_MkUnaryMinus_id)
            describe "Or" $
                it "a \\/ not a == True" $ property (propertyInContext prop_AOrNotA)
            describe "Implies" $ do
                it "not a => a <==> a"     $ property (propertyInContext prop_NotAImpliesAEqualsA)
                it "a => not a <==> not a" $ property (propertyInContext prop_AImpliesNotAEqualsNotA)
            describe "Comparisons" $ do
                    it "a >= b <==> b <= a"      $ property (propertyInContext prop_GELE)
                    it "a > b <==> b < a"        $ property (propertyInContext prop_GTLT)
                    it "a > b <==> not (a <= b)" $ property (propertyInContext prop_GTNotLE)
                    it "a < b <==> not (a >= b)" $ property (propertyInContext prop_LTNotGE)
            describe "Sort" $
                it "uses at least one sort" $ property (propertyInContext prop_ASortUsed)