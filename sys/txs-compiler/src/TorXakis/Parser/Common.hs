module TorXakis.Parser.Common where

-- TODO: use selective imports.
import qualified Data.Text as T
import           Data.Text (Text)
import           Text.Parsec ( ParsecT, (<|>), many, label
                             , getPosition, sourceLine, sourceColumn
                             , getState
                             , putState
                             )
import           Text.Parsec.Token
import           Text.Parsec.Char (lower, upper, oneOf, alphaNum, letter)
import           Control.Monad (void)
import           Control.Monad.Identity (Identity)

import           TorXakis.Parser.Data

-- | Type of the parser intput stream.
--
-- TODO: for now we use String's to be able to leverage on the 'haskell' token
-- parser, in the future we might want to change this to text, and benchmark
-- what is the performance gain.
type ParserInput = String

type TxsParser = ParsecT ParserInput St Identity

txsLangDef :: GenLanguageDef ParserInput St Identity
txsLangDef = LanguageDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = letter
    , identLetter     = alphaNum <|> oneOf "_'"
    , opStart         = opLetter txsLangDef
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedNames   = ["TYPEDEF", "ENDDEF", "FUNCDEF"]
    , reservedOpNames = []
    , caseSensitive   = True
    }

txsTokenP :: GenTokenParser ParserInput St Identity
txsTokenP = makeTokenParser txsLangDef

txsIdentLetter :: TxsParser Char
txsIdentLetter = identLetter txsLangDef

txsSymbol :: String -> TxsParser ()
txsSymbol = void . symbol txsTokenP

txsLexeme :: TxsParser a -> TxsParser a
txsLexeme = lexeme txsTokenP

getMetadata :: TxsParser (Metadata t)
getMetadata = do
    i <- getNextId
    p <- getPosition
    return $ Metadata (sourceLine p) (sourceColumn p) (Loc i)

getNextId :: TxsParser Int
getNextId = do
    st <- getState
    putState $ incId st
    return (nextId st)

-- | Parser for upper case identifiers.
ucIdentifier :: String -> TxsParser Text
ucIdentifier what = identifierNE idStart
    where
      idStart = upper
                `label`
                (what ++ " must start with an uppercase character")

lcIdentifier :: TxsParser Text
lcIdentifier = identifierNE idStart
    where
      idStart = lower <|> oneOf "_"
                `label`
                "Identifiers must start with a lowercase character or '_'"

-- | Parser for non-empty identifiers.
identifierNE :: TxsParser Char -> TxsParser Text
identifierNE idStart = T.cons <$> idStart <*> idEnd
    where
      idEnd  = T.pack <$> many txsIdentLetter

