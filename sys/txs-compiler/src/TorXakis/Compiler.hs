{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
--  Interface for Compilers for the 'TorXakis' MBT tool suite.
--------------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.Compiler
    ( Compiler (..)
    , Error(..)
    )
where

import           TorXakis.Error

-- | compiler class
class Compiler c ctx where
    compile :: c -> ctx -> String -> Either Error ctx

{- discuss with Jan: how to handle the sub parsers 
   * they should support ValExpr Context (can be derived from FuncContext by ContextValExpr.fromFuncContext)
   * Variables are not automatically added to the provided context
   * Final usage compileValExpr + subst (VarsThatHaveAValue)
   
    -- compileVarDecls (define variables and optionally assign values to them)
    compileVarDecls :: ValExprContext ctx => c -> ctx -> String -> Either Error VarDecls
    where VarDecls is something like:   combination of Map Name Sort and Map Name (Maybe ValExpr)
    
    -- compileValExpr
    compileValExpr :: ValExprContext ctx => c -> ctx -> String -> Either Error ValExpr
-}