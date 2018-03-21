-- | 

module TorXakis.Parser.ConstDecl where

import           Text.Parsec ( sepBy )

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs
import           TorXakis.Parser.ValExprDecl

constDeclsP :: TxsParser [FuncDecl]
constDeclsP = do
    txsSymbol "CONSTDEF"
    cs <- constDeclP `sepBy` txsSymbol ";"
    txsSymbol "ENDDEF"
    return cs
    
constDeclP :: TxsParser FuncDecl
constDeclP = do
    m  <- getMetadata
    n  <- txsLexeme lcIdentifier
    txsSymbol "::"
    s  <- sortP
    txsSymbol "::="
    b <- txsLexeme valExpP
    return $ mkFuncDecl n m [] s b
