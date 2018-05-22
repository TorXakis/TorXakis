{-# LANGUAGE OverloadedStrings #-}

module TorXakis.Parser.ChanRef where

import           Control.Monad          (when)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import           Text.Parsec            (parserZero, sepBy, try, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

chansInDecl :: TxsParser [ChanRef]
chansInDecl = chanRefDecls (txsSymbol "CHAN" >> txsSymbol "IN")

chansOutDecl :: TxsParser [ChanRef]
chansOutDecl = chanRefDecls (txsSymbol "CHAN" >> txsSymbol "OUT")

chansSyncDecl :: TxsParser (Maybe [Set ChanRef])
chansSyncDecl = (Just <$> syncSetsP) <|> return Nothing

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

