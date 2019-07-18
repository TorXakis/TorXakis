{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.ValExprDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for value expression declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.ValExprDecl
    ( valExpP
    , letSeqVarDeclsP
    , letVarDeclsP
    )
where

import           Data.Char                (isPrint)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Exts                 (fromList)
import           Text.Parsec              (many, many1, oneOf, optionMaybe,
                                           parserFail, satisfy, sepBy, try,
                                           (<?>), (<|>))
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
      -- This table defines the precedence of operators.
      table = [ [Prefix uop]
              , [Infix bop AssocLeft]
              ]
      uop = do
          (le, lr, opN) <- opP
          return $ \ex0 -> mkExpDecl le Nothing (Fappl (Name opN) lr [ex0])
      opP = do
          le  <- mkLoc
          lr  <- mkLoc
          opN <- txsBopSymbolP
          return (le, lr, opN)
      bop = do
          (le, lr, opN) <- opP
          return $ \ex0 ex1 -> mkExpDecl le Nothing (Fappl (Name opN) lr [ex0, ex1])

-- | Terms of the TorXakis value expressions.
termP :: TxsParser ExpDecl
termP = do
            l <- mkLoc
            c <- termImplicitP
            ms <- optionMaybe ofSortP
            return $ mkExpDecl l ms (childExpChild c)
    where
        termImplicitP :: TxsParser ExpChildDecl
        termImplicitP = mkNestedExp
            <|> mkRegexConstExp  <$> mkLoc <*> txsRegexP
            <|> letExpP
            <|> txsITEP
            <|> txsFapplP
            <|> mkBoolConstExp   <$> mkLoc <*> try txsBoolP
            <|> mkIntConstExp    <$> mkLoc <*> txsIntP
            <|> mkStringConstExp <$> mkLoc <*> txsStringP
            <|> mkVarExp         <$> mkLoc <*> (lcIdentifier <|> ucIdentifier "")

mkNestedExp :: TxsParser ExpChildDecl
mkNestedExp  = do
    l <- mkLoc
    try (txsSymbol "(")
    subEx <- valExpP
    txsSymbol ")"
    return $ mkLetExpDecl [] subEx l
    
letExpP :: TxsParser ExpChildDecl
letExpP = do
    l <- mkLoc
    try (txsSymbol "LET")
    vss <- letSeqVarDeclsP
    subEx <- inP valExpP
    return $ mkLetExpDecl vss subEx l

-- | Parser for a list of value declarations (where each value declaration is a list
-- of the form 'x0 = v0; ...; xn=vn'), which allow to introduce values
-- sequentially.
letSeqVarDeclsP :: TxsParser [ParLetVarDecl]
letSeqVarDeclsP = letVarDeclsP `sepBy` txsSymbol ";"

-- | Parser for let value declarations, separated by commas.
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
    try (txsSymbol "REGEX")
    txsLexeme $ txsSymbol "('"
    res <- T.pack <$> many (satisfy regexChar)
    txsLexeme $ txsSymbol "')"
    return res
    where
      regexChar c = (isPrint c && c /= '\'') || c == ' '

txsITEP :: TxsParser ExpChildDecl
txsITEP = do
    l <- mkLoc
    try (txsSymbol "IF")
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
    if op `elem` [">->", "|", "||", "|||", ">>>", "[>>", "[><", "##", "<-", "->"]
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
txsFapplP :: TxsParser ExpChildDecl
txsFapplP = do
    le <- mkLoc
    lr <- mkLoc
    fN <- try (identifier <* txsSymbol "(")
    exs <- valExpP `sepBy` txsSymbol ","
    txsSymbol ")"
    return $ mkFappl le lr fN exs
