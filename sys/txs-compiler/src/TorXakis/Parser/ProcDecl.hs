module TorXakis.Parser.ProcDecl where

import           Text.Parsec              (sepBy, sepBy1, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanDecl
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

chParamsP :: TxsParser [ChanDecl]
chParamsP = do
    txsSymbol "["
    res <- concat <$> chanDeclsOfSortP `sepBy` txsSymbol ";"
    txsSymbol "]"
    return res

procExitP :: TxsParser ExitSortDecl
procExitP =  (ExitD <$> (txsSymbol "EXIT" *> sortP `sepBy1` txsSymbol "#"))
         <|> (txsSymbol "HIT" >> return HitD)
         <|> return NoExitD
