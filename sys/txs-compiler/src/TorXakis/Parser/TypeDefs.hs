{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.TypeDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for type declarations ('TorXakis' ADT's)
--------------------------------------------------------------------------------
module TorXakis.Parser.TypeDefs
    ( adtP
    , sortP
    , idOfSortsP
    , ofSortP
    )
where

import           Data.Text              (Text)
import           Text.Parsec            (sepBy, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

-- | Parser for type declarations ('TorXakis' ADT's).
adtP :: TxsParser ADTDecl
adtP = declP "TYPEDEF" $ \n l -> mkADTDecl n l <$> cstrP `sepBy` txsSymbol "|"

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
idOfSortsP op cl f = nonEmptyIdOfSortsP <|> return []
    where nonEmptyIdOfSortsP = do
              txsSymbol op
              fd <- idOfSortsListP f `sepBy` txsSymbol ";"
              txsSymbol cl
              return $ concat fd

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

-- | Parser for the declaration of a sort.
ofSortP :: TxsParser OfSort
ofSortP = txsSymbol "::" >> sortP
