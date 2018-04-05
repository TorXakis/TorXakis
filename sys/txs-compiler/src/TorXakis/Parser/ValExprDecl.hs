-- |

module TorXakis.Parser.ValExprDecl where

import           Data.Text                (Text)
import qualified Data.Text                as T
import           Text.Parsec              (many1, notFollowedBy, oneOf,
                                           optionMaybe, sepBy, try, (<?>),
                                           (<|>))

import           Text.Parsec.Expr         (Assoc (AssocLeft), Operator (Infix),
                                           buildExpressionParser)
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

-- | Compound value expressions parser.
--
-- TODO: Consider using https://www.stackage.org/haddock/lts-11.3/parsec-3.1.13.0/Text-Parsec-Expr.html#v:buildExpressionParser.
--
valExpP :: TxsParser ExpDecl
valExpP =  buildExpressionParser table termP
       <?> "Value expression"
    where
      table = [[Infix f AssocLeft]]
      f = do
          le  <- mkLoc
          lr  <- mkLoc
          opN <- txsBopSymbolP
          return $ \ex0 ex1 -> (mkFappl le lr opN [ex0, ex1])

-- | Terms of the TorXakis value expressions.
termP :: TxsParser ExpDecl
termP = txsSymbol "(" *> ( valExpP <* txsSymbol ")")
    <|> txsFapplP
    <|> letExpP
    <|> txsITEP
    <|> mkVarExp         <$> mkLoc <*> lcIdentifier
    <|> mkBoolConstExp   <$> mkLoc <*> txsBoolP
    <|> mkIntConstExp    <$> mkLoc <*> txsIntP
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

-- | Function application parser.
--
txsFapplP :: TxsParser ExpDecl
txsFapplP = do
    le <- mkLoc
    lr <- mkLoc
    fN <- try (lcIdentifier <* txsSymbol "(")
    exs <- valExpP `sepBy` txsSymbol ","
    txsSymbol ")"
    return $ mkFappl le lr fN exs


