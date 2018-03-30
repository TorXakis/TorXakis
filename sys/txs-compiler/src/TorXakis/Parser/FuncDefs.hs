-- TODO: rename to FuncDecl (without the 's'!).
module TorXakis.Parser.FuncDefs where

-- TODO: use selective imports.
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs
import           TorXakis.Parser.ValExprDecl

-- | Function declarations.
fdeclP :: TxsParser FuncDecl
fdeclP = do
    txsWhitespace
    txsSymbol "FUNCDEF" --TODO: adapt this so that function declarations can be separated by ';'.
    l  <- mkLoc
    n  <- txsLexeme lcIdentifier
    ps <- fParamsP
    txsSymbol "::"
    s  <- sortP
    txsSymbol "::="
    b <- txsLexeme valExpP
    txsSymbol "ENDDEF"
    return $ mkFuncDecl n l ps s b

fParamsP :: TxsParser [VarDecl]
fParamsP = idOfSortsP "(" ")" mkVarDecl

