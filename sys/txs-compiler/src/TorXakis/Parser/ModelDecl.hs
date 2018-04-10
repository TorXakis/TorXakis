{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Parser.ModelDecl where

import           Control.Monad            (when)
import           Text.Parsec              (parserZero, sepBy, try, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

modelDeclP :: TxsParser ModelDecl
modelDeclP = do
    txsWhitespace
    txsSymbol "MODELDEF"
    l  <- mkLoc
    n  <- txsLexeme identifier
    txsSymbol "::="
    is <- chanRefDecls (txsSymbol "CHAN" >> txsSymbol "IN")
    os <- chanRefDecls (txsSymbol "CHAN" >> txsSymbol "OUT")
    ys <- chanRefDecls (txsSymbol "SYNC") <|> return []
    txsSymbol "BEHAVIOUR"
    be <- bexpDeclP
    txsSymbol "ENDDEF"
    return $ mkModelDecl n l is os ys be

-- | Make a parser for declarations of references to channels.
chanRefDecls :: TxsParser () -- ^ Declaration prefix
             -> TxsParser [ChanRef]
chanRefDecls pref =
    pref *> try (mkChanRef <$> chIdentifier <*> mkLoc) `sepBy` txsSymbol ","
    where
      chIdentifier = do
          i <-identifier
          when (i `elem` ["CHAN", "IN", "OUT", "SYNC", "BEHAVIOUR"] ) parserZero
          return i



