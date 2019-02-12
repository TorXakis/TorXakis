{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprConstructionContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Construction of ValExpressions.
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module TorXakis.ValExprConstructionContext
( -- * Context
  -- ** ValExpr Construction Context class
  ValExprConstructionContext(..)
  -- hide fromSortContext since is both implemented by VarContext and FuncSignatureContext
, FuncSignatureContext( memberFunc
                      , funcSignatures
                      )
, VarContext ( memberVar
             , lookupVar
             , elemsVar
             , addVars
             )
)
where
import           TorXakis.FuncSignatureContext
import           TorXakis.VarContext

-- | A ValExprConstructionContext Context instance contains all definitions to work with 'TorXakis.ValExpression'.
class (FuncSignatureContext a, VarContext a) => ValExprConstructionContext a where
    -- | Constructor from FuncSignatureContext
    fromFuncSignatureContext :: forall b . FuncSignatureModifyContext b a => b -> a
    fromFuncSignatureContext ctx = let sctx :: b
                                       sctx = TorXakis.FuncSignatureContext.fromSortContext ctx
                                      in case addFuncSignatures sctx (funcSignatures ctx) of
                                                Right x -> x
                                                Left e  -> error ("fromFuncSignatureContext should succeed since FuncSignatureContext should adhere to all constraints, yet " ++ show e)

    -- | Constructor from VarContext
    fromVarContext :: VarContext b => b -> a
    fromVarContext ctx = case addVars (TorXakis.VarContext.fromSortContext ctx) (elemsVar ctx) of
                            Right x -> x
                            Left e  -> error ("fromVarContext should succeed since VarContext should adhere to all constraints, yet " ++ show e)
