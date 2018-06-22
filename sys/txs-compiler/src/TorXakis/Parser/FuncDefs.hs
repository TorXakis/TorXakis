{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.FuncDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for function definitions.
--------------------------------------------------------------------------------
module TorXakis.Parser.FuncDefs
    ( fParamsP
    , fDeclP
    )
where

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs
import           TorXakis.Parser.ValExprDecl

-- | Parser for function declarations.
fDeclP :: TxsParser FuncDecl
fDeclP = do
    txsWhitespace
    txsSymbol "FUNCDEF"
    l  <- mkLoc
    n  <- txsLexeme lcIdentifier
    ps <- fParamsP
    txsSymbol "::"
    s  <- sortP
    txsSymbol "::="
    b <- txsLexeme valExpP
    txsSymbol "ENDDEF"
    return $ mkFuncDecl n l ps s b

-- | Parser for function parameters.
fParamsP :: TxsParser [VarDecl]
fParamsP = idOfSortsP "(" ")" mkVarDecl

