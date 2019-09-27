{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextTestValExpr
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
module TorXakis.ContextTestValExpr
(-- * Context Test ValExpression
  ContextTestValExpr(..)
, TorXakis.ContextTestValExpr.empty
, fromValExprContext
, arbitraryContextTestValExpr
)
where
--import           Debug.Trace
import           Test.QuickCheck

import           TorXakis.ContextValExpr
import           TorXakis.FuncDef
import           TorXakis.FunctionNameGen
import qualified TorXakis.GenCollection
import           TorXakis.SortGen
import           TorXakis.TestValExprContext
import           TorXakis.TestValExprData
import           TorXakis.Var
import           TorXakis.VarsDeclGen

-------------------------------------------------------------------------------------------------------------
-- Test Func Context
-------------------------------------------------------------------------------------------------------------
-- | data for ContextTestValExpr
data ContextTestValExpr =
        ContextTestValExpr
            { basis :: ContextValExpr
            , tvecd :: TestValExprData ContextTestValExpr
            }

-- | empty constructor
empty :: ContextTestValExpr
empty = let ctx = TorXakis.ContextValExpr.empty in
            ContextTestValExpr ctx (TorXakis.TestValExprData.empty ctx)

-- | Constructor from ValExprContext
fromValExprContext :: ValExprContext b => b -> ContextTestValExpr
fromValExprContext ctx = ContextTestValExpr newContext newData
        where
            newContext :: ContextValExpr
            newContext = case TorXakis.TestValExprContext.addADTs (elemsADT ctx) TorXakis.ContextValExpr.empty >>=
                              TorXakis.TestValExprContext.addVars (elemsVar ctx) >>=
                              TorXakis.TestValExprContext.addFuncs (elemsFunc ctx) of
                           Left e -> error ("Context should adhere to all constraint and thus must be copyable, yet " ++ show e)
                           Right x -> x
            newData :: TestValExprData ContextTestValExpr
            newData =   TorXakis.TestValExprData.afterAddFuncs ctx (elemsFunc ctx) $
                        TorXakis.TestValExprData.afterAddVars ctx (elemsVar ctx) $
                        TorXakis.TestValExprData.afterAddADTs ctx (elemsADT ctx) (TorXakis.TestValExprData.empty ctx)


instance SortContext ContextTestValExpr where
    memberSort r = memberSort r . basis

    memberADT r = memberADT r . basis

    lookupADT r = lookupADT r . basis

    elemsADT  = elemsADT . basis

    addADTs as ctx = case addADTs as (basis ctx) of
                          Left e       -> Left e
                          Right basis' -> Right $ ContextTestValExpr basis' (TorXakis.TestValExprData.afterAddADTs basis' as (tvecd ctx))

instance TestSortContext ContextTestValExpr where
    sortSize r = TorXakis.TestValExprData.sortSize r . tvecd
    adtSize a = TorXakis.TestValExprData.adtSize a . tvecd
    constructorSize a c = TorXakis.TestValExprData.constructorSize a c . tvecd

instance VarContext ContextTestValExpr where
    memberVar v = memberVar v . basis

    lookupVar v = lookupVar v . basis

    elemsVar = elemsVar . basis

    addVars vs ctx = case addVars vs (basis ctx) of
                          Left e       -> Left e
                          Right basis' -> Right $ ContextTestValExpr basis' (TorXakis.TestValExprData.afterAddVars basis' vs (tvecd ctx))

instance TestVarContext ContextTestValExpr where
    varSize r ctx = TorXakis.TestValExprData.varSize r (basis ctx) (tvecd ctx)

instance FuncContext ContextTestValExpr where
    memberFunc r = memberFunc r . basis

    lookupFunc r = lookupFunc r . basis

    funcSignatures = funcSignatures . basis

    elemsFunc = elemsFunc . basis

    addFuncs fs ctx = case addFuncs fs (basis ctx) of
                          Left e       -> Left e
                          Right basis' -> Right $ ContextTestValExpr basis' (TorXakis.TestValExprData.afterAddFuncs basis' fs (tvecd ctx))


instance ValExprContext ContextTestValExpr

instance TestValExprContext ContextTestValExpr where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case TorXakis.GenCollection.get (genMap (tvecd ctx)) s n of                 -- TODO: hide map like sortSize?
            [] -> error ("No Generators for " ++ show s ++ " at " ++ show n)
            xs -> do
                    generator <- elements xs
                    generator ctx

    funcSize r ctx = TorXakis.TestValExprData.funcSize r (basis ctx) (tvecd ctx)

-- TODO: add recursive functions (and ensure termination when called with constant arguments)
arbitraryFuncDefs :: TestValExprContext c => c -> Gen [FuncDef]
arbitraryFuncDefs ctx = do
    funcNameGens <- listOf (arbitrary :: Gen FunctionNameGen)
    let funcNames = map unFunctionNameGen funcNameGens in
        mapM defineFunc funcNames
  where
    defineFunc :: FunctionName -> Gen FuncDef
    defineFunc n = do
        vs <- arbitraryVarsDecl ctx
        case addVars (toList vs) ctx of
             Left e     -> error ("arbitraryFuncDefs: Invalid generator - addVars " ++ show e)
             Right ctx2 -> do
                            b <- arbitraryValExpr ctx2
                            case mkFuncDef ctx n vs b of
                                 Left e -> error ("arbitraryFuncDefs: Invalid generator - mkFuncDef " ++ show e)
                                 Right d -> return d

-- | generate an arbitrary Test ValExpr Context
arbitraryContextTestValExpr :: Gen ContextTestValExpr
arbitraryContextTestValExpr =
        let ctx1 = TorXakis.ContextTestValExpr.empty in do
            incr <- arbitraryADTDefs ctx1
            case addADTs incr ctx1 of
                Left e    -> error ("arbitraryContextTestValExpr: Invalid generator - addADTs " ++ show e)
                Right ctx2 -> do
                                fs <- arbitraryFuncDefs ctx2
                                case addFuncs fs ctx2 of
                                             Left e     -> error ("arbitraryContextTestValExpr: Invalid generator - addFuncs " ++ show e)
                                             Right ctx3 ->  do
                                                                vs <- arbitraryVarsDecl ctx3
                                                                case addVars (toList vs) ctx3 of
                                                                    Left e     -> error ("arbitraryContextTestValExpr: Invalid generator - addVars " ++ show e)
                                                                    Right ctx4 -> return ctx4

