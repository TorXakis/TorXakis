-- |

module TorXakis.Parser.TypeDefs where

import           Data.Text              (Text)
import           Text.Parsec            (sepBy, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data


-- | Parser of ADT's.
adtP :: TxsParser ADTDecl
adtP = do
    txsSymbol "TYPEDEF"
    m  <- mkLoc
    n  <- txsLexeme (ucIdentifier "ADT's")
    txsSymbol "::="
    cs <- cstrP `sepBy` txsSymbol "|"
    txsSymbol "ENDDEF"
    return $ mkADTDecl n m cs

cstrP :: TxsParser CstrDecl
cstrP = do
    m  <- mkLoc
    n  <- txsLexeme (ucIdentifier "Constructors")
    fs <- idOfSortsP "{" "}" mkFieldDecl
    return $ mkCstrDecl n m fs

-- | Parser for Sorts.
sortP :: TxsParser OfSort
sortP = do
    m <- mkLoc
    n <- txsLexeme (ucIdentifier "Sorts")
    return $ mkOfSort n m

-- | Parser of a list of identifiers declarations, with an associated sort. The
-- list of identifiers is delimited by the given start and end symbols, and
-- separated by a semi-colon.
--
-- > identifiers :: Sort
--
idOfSortsP :: String -- ^ Start symbol for the fields declaration.
           -> String -- ^ End symbol for the fields declaration.
           -> (Text -> Loc t -> OfSort -> d)
           -> TxsParser [d]
idOfSortsP op cl f = nonEmptyIdOfSortsP <|> emptyDeclsP
    where nonEmptyIdOfSortsP = do
              txsSymbol op
              fd <- idOfSortsListP f `sepBy` txsSymbol ";"
              txsSymbol cl
              return $ concat fd
          emptyDeclsP = return []

-- | Parser of a list of field declarations of the form:
--
-- > x, y, z :: T
--
idOfSortsListP :: (Text -> Loc t -> OfSort -> d) -> TxsParser [d]
idOfSortsListP f =  do
    fns <- txsLexeme lcIdentifier `sepBy` txsSymbol ","
    fs <- ofSortP
    traverse (mkIdWithSort fs) fns
    where
      mkIdWithSort s n = do
          m <- mkLoc
          return $ f n m s

-- | Parse the declaration of a sort
ofSortP :: TxsParser OfSort
ofSortP = txsSymbol "::" >> sortP
