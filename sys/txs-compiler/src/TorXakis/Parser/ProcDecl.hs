module TorXakis.Parser.ProcDecl where

import           Text.Parsec              (sepBy, sepBy1, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.FuncDefs
import           TorXakis.Parser.TypeDefs

procDeclP :: TxsParser ProcDecl
procDeclP = do
    txsSymbol "PROCDEF"
    l  <- mkLoc
    n  <- identifier
    cs <- chParamsP
    vs <- fParamsP
    e  <- procExitP
    txsSymbol "::="
    be <- bexpDeclP
    txsSymbol "ENDDEF"
    return $ mkProcDecl n l cs vs e be

procExitP :: TxsParser ExitSortDecl
procExitP =  (ExitD <$> (txsSymbol "EXIT" *> sortP `sepBy` txsSymbol "#"))
         <|> (txsSymbol "HIT" >> return HitD)
         <|> return NoExitD
