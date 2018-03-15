-- |

module TorXakis.Parser.TypeDefs where

import           Data.Text (Text)
import           Text.Parsec ( ParsecT, (<|>), many, label, eof, unexpected, sepBy
                             , getPosition, sourceLine, sourceColumn
                             )
import           Text.Parsec.Char (lower, upper, oneOf, alphaNum, letter)

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data    ( St (St), nextId, FieldDecl
                                         , Field (Field), ParseTree (ParseTree)
                                         , Metadata (Metadata), SortRef (SortRef), OfSort
                                         , ADTDecl, ADT (ADT)
                                         , CstrDecl, Cstr (Cstr)
                                         )

-- | Parser of ADT's.
adtP :: TxsParser ADTDecl
adtP = do
    txsSymbol "TYPEDEF"
    m  <- getMetadata
    n  <- txsLexeme (ucIdentifier "ADT's")
    txsSymbol "::="
    cs <- cstrP `sepBy` txsSymbol "|"
    txsSymbol "ENDDEF"
    return $ ParseTree n ADT m cs

cstrP :: TxsParser CstrDecl
cstrP = do
    m  <- getMetadata
    n  <- txsLexeme (ucIdentifier "Constructors")
    fs <- "{" `fieldsP` "}"
    return $ ParseTree n Cstr m fs

-- | Parser of a list of fields, delimited by the given symbols.
--
fieldsP :: String -- ^ Start symbol for the fields declaration.
        -> String -- ^ End symbol for the fields declaration.
        -> TxsParser [FieldDecl]
fieldsP op cl = nonEmptyFieldsP <|> emptyFieldsP
    where nonEmptyFieldsP = do
              txsSymbol op
              fd <- fieldListP `sepBy` txsSymbol ";"
              txsSymbol cl
              return $ concat fd
          emptyFieldsP = return []


-- | Parser of a list of field declarations of the form:
--
-- > x, y, z :: T
--
fieldListP :: TxsParser [FieldDecl]
fieldListP =  do
    fns <- txsLexeme lcIdentifier `sepBy` txsSymbol ","
    _  <- txsSymbol "::"
    fs <- sortP
    traverse (mkFieldWithSort fs) fns
    where
      mkFieldWithSort :: OfSort -> Text -> TxsParser FieldDecl
      mkFieldWithSort fs fn = do
          m <- getMetadata
          return $ ParseTree fn Field m fs

-- | Parser for Sorts.
sortP :: TxsParser OfSort
sortP = do
    m <- getMetadata
    s <- txsLexeme (ucIdentifier "Sorts")
    return $ ParseTree s SortRef m ()
