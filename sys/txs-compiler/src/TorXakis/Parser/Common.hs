{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.Common
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Common types and functionality for the 'TorXakis' parser.
--------------------------------------------------------------------------------
module TorXakis.Parser.Common
    ( TxsParser
    , txsSymbol
    , TorXakis.Parser.Common.identifier
    , mkLoc
    , lcIdentifier
    , txsLexeme
    , ucIdentifier
    , txsIntP
    , txsStringP
    , txsWhitespace
    , inP
    , tryIdentifier
    , declP
    , declWithParamsP
    )
where

import           Control.Monad          (void, when)
import           Control.Monad.Identity (Identity)
import           Data.Char              (isUpper, isLower, isAlphaNum)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Parsec            (ParsecT, getPosition, getState,
                                         putState, sourceColumn, sourceLine,
                                         try, (<?>), (<|>))
import           Text.Parsec.Char       (alphaNum, letter, oneOf)
import           Text.Parsec.Token

import           TorXakis.Parser.Data

-- | Type of the parser intput stream.
--
type ParserInput = String

type TxsParser = ParsecT ParserInput St Identity

txsLangDef :: GenLanguageDef ParserInput St Identity
txsLangDef = LanguageDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = letter
    , identLetter     = alphaNum <|> oneOf "_"
    , opStart         = opLetter txsLangDef
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedNames   = txsReservedNames
    , reservedOpNames = []
    , caseSensitive   = True
    }

txsReservedNames :: [String]
txsReservedNames
  = [ "TYPEDEF"
    , "FUNCDEF"
    , "PROCDEF"
    , "ENDDEF"
    , "CHANDEF"
    , "MODELDEF"
    , "MAPPERDEF"
    , "CNECTDEF"
    , "CHAN"
    , "ENCODE"
    , "DECODE"
    , "IN"
    , "OUT"
    , "HOST"
    , "PORT"
    , "CLIENTSOCK"
    , "SERVERSOCK"
    , "BEHAVIOUR"
    , "IF"
    , "THEN"
    , "ELSE"
    , "FI"
    , "LET"
    , "PURPDEF"
    , "HIT"
    , "MISS"
    , "GOAL"
    , "HIDE"
    , "NI"
    , "SYNC"
    , "EXIT"
    , "STOP"
    , "ACCEPT"
    , "STAUTDEF"
    , "VAR"
    , "STATE"
    , "INIT"
    , "TRANS"
    , "ISTEP"
    , "QSTEP"
    , "CONSTDEF" ]

txsTokenP :: GenTokenParser ParserInput St Identity
txsTokenP = makeTokenParser txsLangDef

-- | Parse given symbol, discarding its result.
txsSymbol :: String -- ^ String representation of the symbol.
          -> TxsParser ()
txsSymbol = void . symbol txsTokenP

-- | Parser for lexemes.
txsLexeme :: TxsParser a -> TxsParser a
txsLexeme = lexeme txsTokenP

-- | Parser that ignores white-spaces.
txsWhitespace :: TxsParser ()
txsWhitespace = whiteSpace txsTokenP

-- | Parser for strings.
txsStringP :: TxsParser Text
txsStringP = T.pack <$> stringLiteral txsTokenP

-- | Parser for integer.
txsIntP :: TxsParser Integer
txsIntP = integer txsTokenP

-- | Make a new location while parsing.
mkLoc :: TxsParser (Loc t)
mkLoc = do
    i <- getNextId
    p <- getPosition
    return $ Loc (sourceLine p) (sourceColumn p) i

getNextId :: TxsParser Int
getNextId = do
    st <- getState
    putState $ incId st
    return (nextId st)

-- | isLetter 
isLetter :: Char -> Bool
isLetter '_' = True
isLetter c   = isAlphaNum c

-- | Parser for upper-case identifiers.
ucIdentifier :: String -> TxsParser Text
ucIdentifier what = try ( do
                               s <- Text.Parsec.Token.identifier txsTokenP
                               if isUpperCase s 
                                then return $ T.pack s
                                else fail "not uppercase"
                        ) <?> what
    where
      isUpperCase :: String -> Bool
      isUpperCase s = case s of
                            []      -> False
                            (x:xs)  -> isUpper x && all isLetter xs

-- | Parser for lower-case identifiers.
lcIdentifier :: TxsParser Text
lcIdentifier = try ( do
                           s <- Text.Parsec.Token.identifier txsTokenP
                           if isLowerCase s 
                            then return $ T.pack s
                            else fail "not lowercase"
                   ) <?> "lowercase identifier"
    where
      isLowerCase :: String -> Bool
      isLowerCase s = case s of
                            []       -> False
                            ('_':xs) -> all isLetter xs
                            (x:xs)   -> isLower x && all isLetter xs

-- | Parser for identifiers, which may start with lower or upper case letters.
identifier :: TxsParser Text
identifier = T.pack <$> Text.Parsec.Token.identifier txsTokenP <?> "identifier"

-- | Like @identifier@, but try.
tryIdentifier :: TxsParser Text
tryIdentifier = try TorXakis.Parser.Common.identifier

-- | Parse expressions of the form "IN exp NI", where 'p' is the expressions
-- parser.
inP :: TxsParser a -> TxsParser a
inP p = txsSymbol "IN" *> p <* txsSymbol "NI"

-- | Parser for declarations of the form:
--
-- @
-- XDEF n ::= p ENDEF
-- @
--
-- Where n is the name of the definition, p is the parser that parses the body
-- of the declaration, and 'XDEF' is the declaration name (e.g. 'MODELDEF').
--
declP :: String                         -- ^ Name of the declaration.
      -> (Text -> Loc l -> TxsParser a) -- ^ Parser for the body of the
                                        -- declaration. The name given as a
                                        -- parameter is the name of the
                                        -- declaration. The location given as
                                        -- parameter is the location in which
                                        -- the declaration name was found.
      -> TxsParser a
declP declName bodyP = declWithParamsP declName (return ()) (const bodyP) True

-- | Parser for declarations with parameters of the form:
-- @
-- XDEF n params ::= body ENDEF
-- @
--
-- Where 'n' is the name of the definition, 'params' are the parameters taken
-- by the declaration, 'body' is the body of the declaration, and 'XDEF' is the
-- declaration name (e.g. 'PROCDEF'). The boolean parameter of this function
-- indicates whether '"ENDDEF"' is required.
declWithParamsP :: String
                -> TxsParser params
                -> (params -> Text -> Loc l -> TxsParser a)
                -> Bool                                     -- ^ Is "ENDEF" expected?
                -> TxsParser a
declWithParamsP declName paramsP bodyP end = do
    l <- try (mkLoc <* txsSymbol declName)
    n <- txsLexeme TorXakis.Parser.Common.identifier
    params <- paramsP
    txsSymbol "::="
    res <- bodyP params n l
    when end (txsSymbol "ENDDEF")
    return res
