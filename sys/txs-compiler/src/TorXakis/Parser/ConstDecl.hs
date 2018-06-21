{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.ConstDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for constant declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.ConstDecl
    (constDeclsP)
where

import           Text.Parsec                 (sepBy)

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs
import           TorXakis.Parser.ValExprDecl

-- | Parse a constant declaration. Constants are treated as nullary functions.
constDeclsP :: TxsParser [FuncDecl]
constDeclsP = do
    txsSymbol "CONSTDEF"
    cs <- constDeclP `sepBy` txsSymbol ";"
    txsSymbol "ENDDEF"
    return cs

constDeclP :: TxsParser FuncDecl
constDeclP = do
    l  <- mkLoc
    n  <- txsLexeme lcIdentifier
    txsSymbol "::"
    s  <- sortP
    txsSymbol "::="
    b <- txsLexeme valExpP
    return $ mkFuncDecl n l [] s b
