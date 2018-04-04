-- |

module TorXakis.Parser.ValExprDecl where

import           Data.Text                (Text)
import qualified Data.Text                as T
import           Text.Parsec              (many1, notFollowedBy, oneOf,
                                           optionMaybe, sepBy, try, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

-- Compound value expressions.
valExpP :: TxsParser ExpDecl
valExpP =  try (atomValExpP <* notFollowedBy txsBopSymbolP)
       <|> letExpP
       <|> txsITEP
       <|> txsBopP

-- Atomic value expressions
atomValExpP :: TxsParser ExpDecl
atomValExpP =  mkVarExp <$> mkLoc <*> lcIdentifier
           <|> mkBoolConstExp <$> mkLoc <*> txsBoolP
           <|> mkIntConstExp <$> mkLoc <*> txsIntP
           <|> mkStringConstExp <$> mkLoc <*> txsStringP

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

txsBopP :: TxsParser ExpDecl
txsBopP = do
    le  <- mkLoc -- Location of the expression
    ex0 <- atomValExpP <|> valExpP
    lr  <- mkLoc -- Location of the reference to the operator name.
    opN <- txsBopSymbolP
    ex1 <- atomValExpP <|> valExpP
    return $ mkFappl le lr opN ex0 ex1

txsBopSymbolP :: TxsParser Text
txsBopSymbolP = T.pack <$> txsLexeme (many1 txsSpecialCOp)

-- | Special characters for operators.
txsSpecialCOp :: TxsParser Char
txsSpecialCOp = oneOf [ '='
                      , '+'
                      , '-'
                      , '*'
                      , '/'
                      , '\\'
                      , '^'
                      , '<'
                      , '>'
                      , '|'
                      , '@'
                      , '&'
                      , '%'
                      ]
