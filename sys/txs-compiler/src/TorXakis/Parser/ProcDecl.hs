{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.ProcDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for process declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.ProcDecl
    (procDeclP)
where

import           Text.Parsec              (try)

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.FuncDefs

-- | Parser for process declarations.
procDeclP :: TxsParser ProcDecl
procDeclP = do
    try $ txsSymbol "PROCDEF"
    l  <- mkLoc
    n  <- identifier
    cs <- chParamsP
    vs <- fParamsP
    e  <- procExitP
    txsSymbol "::="
    be <- bexpDeclP
    txsSymbol "ENDDEF"
    return $ mkProcDecl n l cs vs e be
