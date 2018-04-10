module TorXakis.Parser.Common where

-- TODO: use selective imports.
import           Control.Monad          (void)
import           Control.Monad.Identity (Identity)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Text.Parsec            (ParsecT, getPosition, getState, label,
                                         many, putState, sourceColumn,
                                         sourceLine, (<|>))
import           Text.Parsec.Char       (alphaNum, letter, lower, oneOf, upper)
import           Text.Parsec.Token

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
    , reservedNames   = ["TYPEDEF", "ENDDEF", "FUNCDEF", "CONSTDEF"]
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

txsWhitespace :: TxsParser ()
txsWhitespace = whiteSpace txsTokenP

txsStringP :: TxsParser Text
txsStringP = T.pack <$> stringLiteral txsTokenP

txsIntP :: TxsParser Integer
txsIntP = integer txsTokenP

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

-- | Parser for upper case identifiers.
ucIdentifier :: String -> TxsParser Text
ucIdentifier what = txsLexeme (identifierNE idStart)
    where
      idStart = upper
                `label`
                (what ++ " must start with an uppercase letter")

lcIdentifier :: TxsParser Text
lcIdentifier = txsLexeme (identifierNE idStart)
    where
      idStart = lower <|> oneOf "_"
                `label`
                "Identifiers must start with a lowercase letter or '_'"

-- | Parser for identifiers, which may start with lower or upper case letters.
identifier :: TxsParser Text
identifier = txsLexeme (identifierNE idStart)
    where
      idStart = lower <|> upper <|> oneOf "_"
                `label`
                "Identifiers must start with a letter or '_'"

-- | Parser for non-empty identifiers.
identifierNE :: TxsParser Char -> TxsParser Text
identifierNE idStart = T.cons <$> idStart <*> idEnd
    where
      idEnd  = T.pack <$> many txsIdentLetter

