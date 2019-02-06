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
module TorXakis.TestValExprContextSpec
(spec
)
where
import           Debug.Trace

import           Data.Either
import qualified Data.Text              as T
import           Test.Hspec
import           Test.QuickCheck

import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.TestValExprContext
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.VarContext
import           TorXakis.VarDef

propertyInContext  :: (MinimalTestValExprContext -> Gen Bool) -> Gen Bool
propertyInContext prop =
    -- TODO: add to context, to generate more value expressions of a given type
    let ctx = empty :: MinimalTestValExprContext in
        case mkName (T.pack "i") of
            Left e   -> error ("can't make name "++ show e)
            Right ni -> case partitionEithers [ mkVarDef ctx ni SortInt
                                              ] of
                             ([], vs) -> case addVarDefs ctx vs of
                                            Right nctx  -> prop nctx
                                            Left  e     -> error ("can't add vardefs "++ show e)
                             (es, _)  -> error ("can't make vardefs "++ show es)

-- | min (min x) == x
prop_MkUnaryMinus_id :: MinimalTestValExprContext -> Gen Bool
prop_MkUnaryMinus_id ctx = do
        ve <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        trace   ("ValExpr " ++ show ve) $
                return $ case mkUnaryMinus ctx ve of
                                Left e    -> trace ("\nUnexpected error in generator 1 " ++ show e) False
                                Right mve -> case mkUnaryMinus ctx mve of
                                                Left e     -> trace ("\nUnexpected error in generator 2 " ++ show e) False
                                                Right mmve -> ve == mmve

-- | a \/ not a <==> True
prop_AOrNotA :: MinimalTestValExprContext -> Gen Bool
prop_AOrNotA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpression
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkOr ctx [a,na] of
                                        Left e  -> trace ("\nUnexpected error with mkOr " ++ show e) False
                                        Right v -> case view v of 
                                                        Vconst (Cbool True) -> True
                                                        x                   -> trace ("\nWrong value = " ++ show x) False

-- | not a => a <==> a
prop_NotAImpliesAEqualsA :: MinimalTestValExprContext -> Gen Bool
prop_NotAImpliesAEqualsA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpression
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkImplies ctx na a of
                                        Left e  -> trace ("\nUnexpected error with mkImplies " ++ show e) False
                                        Right v -> a == v

-- | a => not a <==> not a
prop_AImpliesNotAEqualsNotA :: MinimalTestValExprContext -> Gen Bool
prop_AImpliesNotAEqualsNotA ctx = do
        a <- arbitraryValExprOfSort ctx SortBool :: Gen ValExpression
        return $ case mkNot ctx a of
                        Left e   -> trace ("\nUnexpected error with mkNot " ++ show e) False
                        Right na -> case mkImplies ctx a na of
                                        Left e  -> trace ("\nUnexpected error with mkImplies " ++ show e) False
                                        Right v -> na == v

-- | a >= b <==> b <= a
prop_GELE :: MinimalTestValExprContext -> Gen Bool
prop_GELE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        return $ mkGE ctx a b == mkLE ctx b a

-- | a > b <==> b < a
prop_GTLT :: MinimalTestValExprContext -> Gen Bool
prop_GTLT ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        return $ mkGT ctx a b == mkLT ctx b a

-- | a > b <==> not (a <= b)
prop_GTNotLE :: MinimalTestValExprContext -> Gen Bool
prop_GTNotLE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        return $ case mkLE ctx a b of
                    Left e   -> trace ("\nUnexpected error with mkLE " ++ show e) False
                    Right le -> mkGT ctx a b == mkNot ctx le

-- | a < b <==> not (a >= b)
prop_LTNotGE :: MinimalTestValExprContext -> Gen Bool
prop_LTNotGE ctx = do
        a <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        b <- arbitraryValExprOfSort ctx SortInt :: Gen ValExpression
        return $ case mkGE ctx a b of
                    Left e   -> trace ("\nUnexpected error with mkGE " ++ show e) False
                    Right ge -> mkLT ctx a b == mkNot ctx ge

spec :: Spec
spec = do
            describe "mkUnaryMinus" $
                it "mkUnaryMinus mkUnaryMinus == id" $ property (propertyInContext prop_MkUnaryMinus_id)
            describe "Or" $
                it "a \\/ not a == True" $ property (propertyInContext prop_AOrNotA)
            describe "Implies" $
                    it "not a => a <==> a"     $ property (propertyInContext prop_NotAImpliesAEqualsA)
                    it "a => not a <==> not a" $ property (propertyInContext prop_AImpliesNotAEqualsNotA)
            describe "Comparisons" $ 
                    it "a >= b <==> b <= a"      $ property (propertyInContext prop_GELE)
                    it "a > b <==> b < a"        $ property (propertyInContext prop_GTLT)
                    it "a > b <==> not (a <= b)" $ property (propertyInContext prop_GTNotLE)
                    it "a < b <==> not (a >= b)" $ property (propertyInContext prop_LTNotGE)