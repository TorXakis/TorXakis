module TorXakis.Parser.FuncDefs where

-- TODO: use selective imports.
import           TorXakis.Parser.Common
import           TorXakis.Parser.TypeDefs
import           TorXakis.Parser.Data

-- | Function declarations.
fdeclP :: TxsParser FuncDecl
fdeclP = do
    txsSymbol "FUNCDEF"
    m  <- getMetadata
    n  <- txsLexeme lcIdentifier
    ps <- fParamsP
    txsSymbol "::"
    s  <- sortP
    txsSymbol "::="
    b <- txsLexeme fBodyP
    txsSymbol "ENDDEF"
    return $ mkFuncDecl n m ps s b

fParamsP :: TxsParser [VarDecl]
fParamsP = idOfSortsP "(" ")" mkVarDecl

fBodyP :: TxsParser ExpDecl
fBodyP =
    mkVarExp <$> lcIdentifier <*> getMetadata
    
