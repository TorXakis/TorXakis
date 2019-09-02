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
    , identifier
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
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Parsec            (ParsecT, getPosition, getState, many,
                                         putState, sourceColumn, sourceLine,
                                         try, (<?>), (<|>))
import           Text.Parsec.Char       (lower, oneOf, upper, satisfy)
import           Text.Parsec.Token      hiding (identifier)

import           TorXakis.Language
import           TorXakis.Parser.Data

-- | Type of the parser intput stream.
--
type ParserInput = String

type TxsParser = ParsecT ParserInput St Identity

txsLangDef :: GenLanguageDef ParserInput St Identity
txsLangDef = LanguageDef
    { commentStart    = toString txsCommentStart
    , commentEnd      = toString txsCommentEnd
    , commentLine     = toString txsCommentLine
    , nestedComments  = txsNestedComments
    , identStart      = satisfy satisfyTxsIdentifierHead
    , identLetter     = satisfy satisfyTxsIdentifierTail
    , opStart         = opLetter txsLangDef             -- TODO: get clear what should be here?
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedNames   = map toString (Set.toList txsKeywords)
    , reservedOpNames = []
    , caseSensitive   = True
    }


txsTokenP :: GenTokenParser ParserInput St Identity
txsTokenP = makeTokenParser txsLangDef

txsIdentLetter :: TxsParser Char
txsIdentLetter = identLetter txsLangDef

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

-- | Parser for upper-case identifiers.
ucIdentifier :: String -> TxsParser Text
ucIdentifier what = txsLexeme (identifierNE idStart) <?> what
    where
      idStart = upper

-- | Parser for lower-case identifiers.
lcIdentifier :: TxsParser Text
lcIdentifier = txsLexeme (identifierNE idStart) <?> "lowercase identifier"
    where
      idStart = lower <|> oneOf "_"

-- | Parser for identifiers, which may start with lower or upper case letters.
identifier :: TxsParser Text
identifier = txsLexeme (identifierNE idStart) <?> "identifier"
    where
      idStart = lower <|> upper <|> oneOf "_"

-- | Like @identifier@, but try.
tryIdentifier :: TxsParser Text
tryIdentifier = try identifier

-- | Parser for non-empty identifiers.
identifierNE :: TxsParser Char -> TxsParser Text
identifierNE idStart = T.cons <$> idStart <*> idEnd
    where
      idEnd  = T.pack <$> many txsIdentLetter

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
    n <- txsLexeme identifier
    params <- paramsP
    txsSymbol "::="
    res <- bodyP params n l
    when end (txsSymbol "ENDDEF")
    return res
