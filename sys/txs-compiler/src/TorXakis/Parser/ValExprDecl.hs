-- |

module TorXakis.Parser.ValExprDecl where

import           Text.Parsec              (optionMaybe, sepBy, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

valExpP :: TxsParser ExpDecl
valExpP =  mkVarExp <$> mkLoc <*> lcIdentifier
       <|> mkBoolConstExp <$> mkLoc <*> txsBoolP
       <|> mkIntConstExp <$> mkLoc <*> txsIntP
       <|> mkStringConstExp <$> mkLoc <*> txsStringP
       <|> letExpP
       <|> txsITEP

letExpP :: TxsParser ExpDecl
letExpP = do
    l <- mkLoc
    txsSymbol "LET"
    vs <- letVarDeclsP
    txsSymbol "IN"
    subEx <- valExpP
    txsSymbol "NI"
    return $ mkLetExpDecl vs subEx l

letVarDeclsP :: TxsParser [LetVarDecl]
letVarDeclsP = letVarDeclP `sepBy` txsSymbol ";"

letVarDeclP :: TxsParser LetVarDecl
letVarDeclP = do
    l <- mkLoc
    v <- txsLexeme lcIdentifier
    ms <- optionMaybe ofSortP
    txsSymbol "="
    subEx <- valExpP
    return $ mkLetVarDecl v ms subEx l

txsBoolP :: TxsParser Bool
txsBoolP =  (txsSymbol "True" >> return True)
        <|> (txsSymbol "False" >> return False)

txsITEP :: TxsParser ExpDecl
txsITEP = do
    l <- mkLoc
    txsSymbol "IF"
    ex0 <- valExpP
    txsSymbol "THEN"
    ex1 <- valExpP
    txsSymbol "ELSE"
    ex2 <- valExpP
    txsSymbol "FI"
    return $ mkITEExpDecl l ex0 ex1 ex2
