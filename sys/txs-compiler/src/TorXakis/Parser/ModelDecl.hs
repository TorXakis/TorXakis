{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Parser.ModelDecl where

import           Control.Monad            (when)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
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
    ys <- (Just <$> syncSetsP) <|> return Nothing
    txsSymbol "BEHAVIOUR"
    be <- bexpDeclP
    txsSymbol "ENDDEF"
    return $ mkModelDecl n l is os ys be

-- | Make a parser for declarations of references to channels.
chanRefDecls :: TxsParser () -- ^ Declaration prefix
             -> TxsParser [ChanRef]
chanRefDecls pref =
    pref *> try channelRefP `sepBy` txsSymbol ","

channelRefP :: TxsParser ChanRef
channelRefP = mkChanRef <$> chIdentifierP <*> mkLoc

chIdentifierP :: TxsParser Text
chIdentifierP = do
    i <-identifier
    when (i `elem` ["CHAN", "IN", "OUT", "SYNC", "BEHAVIOUR"] ) parserZero
    return i

syncSetsP :: TxsParser [Set ChanRef]
syncSetsP = do
    txsSymbol "SYNC"
    syncSetP `sepBy` txsSymbol ","

syncSetP :: TxsParser (Set ChanRef)
syncSetP = do
    txsSymbol "{"
    syncChs <- channelRefP `sepBy` txsSymbol "|"
    txsSymbol "}"
    return $ Set.fromList syncChs

