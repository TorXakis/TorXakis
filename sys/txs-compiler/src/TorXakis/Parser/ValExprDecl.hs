-- |

module TorXakis.Parser.ValExprDecl where

import           Data.Char                (isPrint)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Exts                 (fromList)
import           Text.Parsec              (many, many1, notFollowedBy, oneOf,
                                           optionMaybe, parserFail, satisfy,
                                           sepBy, try, (<?>), (<|>))

import           Text.Parsec.Expr         (Assoc (AssocLeft),
                                           Operator (Infix, Prefix),
                                           buildExpressionParser)
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

-- | Compound value expressions parser.
--
valExpP :: TxsParser ExpDecl
valExpP =  buildExpressionParser table termP
       <?> "Value expression"
    where
      table = [ [Prefix uop]
              , [Infix bop AssocLeft]
              ]
      uop = do
          le  <- mkLoc
          lr  <- mkLoc
          opN <- txsBopSymbolP
          return $ \ex0 -> mkFappl le lr opN [ex0]
      bop = do
          le  <- mkLoc
          lr  <- mkLoc
          opN <- txsBopSymbolP
          return $ \ex0 ex1 -> (mkFappl le lr opN [ex0, ex1])

-- | Terms of the TorXakis value expressions.
termP :: TxsParser ExpDecl
termP = txsSymbol "(" *> ( valExpP <* txsSymbol ")")
    <|> try (mkRegexConstExp  <$> mkLoc <*> txsRegexP)
    <|> try letExpP
    <|> try txsITEP
    <|> try txsFapplP
    <|> mkBoolConstExp   <$> mkLoc <*> try txsBoolP
    <|> mkIntConstExp    <$> mkLoc <*> txsIntP
    <|> mkStringConstExp <$> mkLoc <*> txsStringP
    <|> mkVarExp         <$> mkLoc <*> (lcIdentifier <|> ucIdentifier "")

letExpP :: TxsParser ExpDecl
letExpP = do
    l <- mkLoc
    txsSymbol "LET"
    vss <- letSeqVarDeclsP
    txsSymbol "IN"
    subEx <- valExpP
    txsSymbol "NI"
    return $ mkLetExpDecl vss subEx l

-- | Parse a list of value declarations (where each value declaration is a list
-- of the form 'x0 = v0, ..., xn=vn'), which allow to introduce values
-- sequentially.
letSeqVarDeclsP :: TxsParser [ParLetVarDecl]
letSeqVarDeclsP = letVarDeclsP `sepBy` txsSymbol ";"

letVarDeclsP :: TxsParser ParLetVarDecl
letVarDeclsP = fromList <$> letVarDeclP `sepBy` txsSymbol ","

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

txsRegexP :: TxsParser Text
txsRegexP = do
    txsSymbol "REGEX"
    txsLexeme $ txsSymbol "('"
    res <- T.pack <$> many (satisfy regexChar)
    txsLexeme $ txsSymbol "')"
    return res
    where
      regexChar c = (isPrint c && c /= '\'') || c == ' '

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
txsBopSymbolP = try $ T.pack <$> do
    op <- txsLexeme (many1 txsSpecialCOp)
    if op `elem` [">->", "|", "||", "|||", ">>>", "[>>", "[><", "##", "<-"]
        then parserFail $ "Operator \"" ++ op
                        ++ "\" not allowed in value expressions."
        else return op

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
    fN <- try (identifier <* txsSymbol "(")
    exs <- valExpP `sepBy` txsSymbol ","
    txsSymbol ")"
    return $ mkFappl le lr fN exs


