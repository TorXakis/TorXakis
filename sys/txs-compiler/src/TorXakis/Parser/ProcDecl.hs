module TorXakis.Parser.ProcDecl where

import           Text.Parsec              (try, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.FuncDefs
import           TorXakis.Parser.TypeDefs

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
