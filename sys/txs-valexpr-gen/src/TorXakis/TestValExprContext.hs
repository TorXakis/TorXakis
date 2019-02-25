{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- ValExpression Context for Test: 
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.TestValExprContext
(-- * Test ValExpression Context
  TestValExprContext(..)
, ContextTestValExpr(..)
)
where
--import           Debug.Trace

import qualified Data.List           as List
import qualified Data.Text           as T
import           Test.QuickCheck

import           TorXakis.Error
import           TorXakis.FuncContext
import qualified TorXakis.GenCollection
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.Sort
import           TorXakis.SortGen
import           TorXakis.TestVarContext
import           TorXakis.TestFuncSignatureContext
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.ValueGen
import           TorXakis.VarContext
import           TorXakis.Var

-------------------------------------------------------------------------------
-- Generic Generators
-------------------------------------------------------------------------------
genValExprVar :: TestValExprContext a => RefByName VarDef -> a -> Gen ValExpression
genValExprVar v ctx = do
    case mkVar ctx v of
        Left e  -> error ("genValExprVar constructor with " ++ show v ++ " fails " ++ show e)
        Right x -> return x

genValExprValueOfSort :: TestValExprContext a => Sort -> a -> Gen ValExpression
genValExprValueOfSort s ctx = do
    v <- arbitraryValueOfSort ctx s
    case mkConst ctx v of
        Left e  -> error ("genValExprValueOfSort constructor with value " ++ show v ++ " of sort " ++ show s ++ " fails " ++ show e)
        Right x -> return x
-------------------------------------------------------------------------------
-- Boolean Generators
-------------------------------------------------------------------------------
genValExprNot :: TestValExprContext a => a -> Gen ValExpression
genValExprNot ctx = do
    n <- getSize
    arg <- resize (n-1) (arbitraryValExprOfSort ctx SortBool)
    case mkNot ctx arg of
         Left e  -> error ("genValExprNot constructor fails " ++ show e)
         Right x -> return x
-------------------------------------------------------------------------------
-- Integer Generators
-------------------------------------------------------------------------------
nonZero :: TestValExprContext a => a -> Gen ValExpression
nonZero ctx = do
    n <- arbitraryValExprOfSort ctx SortInt
    case view n of
        Vconst (Cint 0) -> nonZero ctx
        _               -> return n

division :: TestValExprContext a => a -> Gen (ValExpression, ValExpression)
division ctx = do
    n <- getSize
    let available = n - 2 in do -- distrubte available size over two intervals
        t <- choose (0, available)
        teller <- resize t             (arbitraryValExprOfSort ctx SortInt)
        noemer <- resize (available-t) (nonZero ctx)
        return (teller, noemer)

genValExprModulo :: TestValExprContext a => a -> Gen ValExpression
genValExprModulo ctx = do
    (teller, noemer) <- division ctx
    case mkModulo ctx teller noemer of
         Left e  -> error ("genValExprModulo constructor fails " ++ show e)
         Right x -> return x

genValExprDivide :: TestValExprContext a => a -> Gen ValExpression
genValExprDivide ctx = do
    (teller, noemer) <- division ctx
    case mkDivide ctx teller noemer of
         Left e  -> error ("genValExprDivide constructor fails " ++ show e)
         Right x -> return x
-------------------------------------------------------------------------------------------------------------
-- Test Func Context
-------------------------------------------------------------------------------------------------------------
class (TestFuncSignatureContext a, FuncContext a) => TestFuncContext a

class (TestFuncContext a, VarContext a) => TestValExprContext a where
    arbitraryValExpr :: a -> Gen ValExpression
    arbitraryValExpr ctx = do
        s <- arbitrarySort ctx
        arbitraryValExprOfSort ctx s
    arbitraryValExprOfSort :: a -> Sort -> Gen ValExpression

data ContextTestValExpr = ContextTestValExpr 
                                    { testVarContext :: ContextTestVar
                                    , funcDefs :: HashMap.Map FuncSignature FuncDef
                                    , _genMap :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                                    -- to be added
                                    -- 0. predefined operators (modulo, and, not, sum , etc)
                                    -- 1. value generation for all defined sorts
                                    -- 2. generators related to ADTs (constructors and accessors)
                                    -- 3. If THEN Else etc.
                                    -- 4. generators for variables (based on variables defined in context)
                                    -- 5. generators for funcInstantiation (based on funcSignatures defined in context)
                                    }

initialGenMap :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
initialGenMap =   addSuccess SortBool   0 (genValExprValueOfSort SortBool)
                $ addSuccess SortBool   1 genValExprNot
                $ addSuccess SortInt    0 (genValExprValueOfSort SortInt)
                $ addSuccess SortInt    2 genValExprModulo
                $ addSuccess SortInt    2 genValExprDivide
                $ addSuccess SortChar   0 (genValExprValueOfSort SortChar)
                $ addSuccess SortString 0 (genValExprValueOfSort SortString)
                $ addSuccess SortRegex  0 (genValExprValueOfSort SortRegex)
                  TorXakis.GenCollection.empty
    where
         addSuccess :: Sort
                    -> Int
                    -> (ContextTestValExpr -> Gen ValExpression) 
                    -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                    -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
         addSuccess s n g c = let ctx = ContextTestValExpr TorXakis.SortGen.empty TorXakis.GenCollection.empty TorXakis.NameMap.empty
                                in 
                                    case TorXakis.GenCollection.add ctx s n g c of
                                        Left e -> error ("empty - successful add expected, yet " ++ show e)
                                        Right c' -> c'

empty :: ContextTestValExpr
empty = ContextTestValExpr TorXakis.SortGen.empty
                           initialGenMap
                           TorXakis.NameMap.empty
    
instance SortContext ContextTestValExpr where
    memberSort r = memberSort r . testVarContext

    memberADT r = memberADT r . testVarContext

    lookupADT r = lookupADT r . testVarContext

    elemsADT  = elemsADT . testVarContext

    addADTs as ctx = case addADTs as (testVarContext ctx) of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx { _genMap = addValueGens (_genMap ctx) as
                                                      -- TODO: add constructors and accessors
                                                    , testVarContext = vctx
                                                    }
        where
            addValueGens :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                        -> [ADTDef]
                        -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
            addValueGens = foldl addValueGen
                where
                    addValueGen :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                                -> ADTDef
                                -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                    addValueGen m a = let srt = SortADT ((RefByName . adtName) a)
                                        in
                                          case TorXakis.GenCollection.add ctx srt 0 (genValExprValueOfSort srt) m of
                                            Left e -> error ("addADTs - successful add expected, yet " ++ show e)
                                            Right c -> c

instance TestSortContext ContextTestValExpr where
    sortSize r = sortSize r . testVarContext
    adtSize a = adtSize a . testVarContext
    constructorSize a c = constructorSize a c . testVarContext

addVarGen :: ContextTestValExpr
          -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
          -> VarDef
          -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
addVarGen ctx m v = case TorXakis.GenCollection.add ctx s useSize (genValExprVar (RefByName n)) m of
                            Left e -> error ("addVars - successful add expected, yet " ++ show e)
                            Right x -> x
    where
        s = TorXakis.Var.sort v
        n = TorXakis.Var.name v
        useSize = sortSize s ctx + 1

instance VarContext ContextTestValExpr where
    memberVar v = member v . testVarContext

    lookupVar v = TorXakis.NameMap.lookup v testVarContext

    elemsVar = elemsVar . testVarContext

    addVars vs ctx = case addVars vs (testVarContext ctx) of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx { testVarContext = vctx
                                                    , _genMap = foldl (addVarGen vctx) (_genMap ctx) vs
                                                    }

instance TestVarContext ContextTestValExpr

instance FuncSignatureContext ContextTestValExpr where

instance TestFuncSignatureContext ContextTestValExpr

instance FuncContext ContextTestValExpr where

instance ValExprConstructionContext ContextTestValExpr

instance ValExprConstructionContext ContextTestValExpr

instance TestValExprContext ContextTestValExpr where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case TorXakis.GenCollection.get (_genMap ctx) s n of
            [] -> error ("No Generators for " ++ show s ++ " at " ++ show n)
            xs -> do
                    generator <- elements xs
                    generator ctx


