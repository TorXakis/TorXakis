{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.ChanRef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for channel references.
--------------------------------------------------------------------------------
module TorXakis.Parser.ChanRef
    ( chansSyncDecl
    , chansOutDecl
    , chansInDecl
    , channelRefP
    )
where

import           Control.Monad          (when)
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import           Text.Parsec            (parserZero, sepBy, try, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

-- | Parser for input channel declarations.
chansInDecl :: TxsParser [ChanRef]
chansInDecl = chanRefDecls (txsSymbol "CHAN" >> txsSymbol "IN")

-- | Parser for output channel declarations.
chansOutDecl :: TxsParser [ChanRef]
chansOutDecl = chanRefDecls (txsSymbol "CHAN" >> txsSymbol "OUT")

-- | Parser for synchronizing channel declarations.
chansSyncDecl :: TxsParser (Maybe [Set ChanRef])
chansSyncDecl = (Just <$> syncSetsP) <|> return Nothing

-- | Make a parser for declarations of references to channels.
chanRefDecls :: TxsParser () -- ^ Declaration prefix
             -> TxsParser [ChanRef]
chanRefDecls pref =
    pref *> try channelRefP `sepBy` txsSymbol ","

-- | Parser for channel references.
channelRefP :: TxsParser ChanRef
channelRefP = mkChanRef <$> chIdentifierP <*> mkLoc

-- | Parser for valid channel identifiers.
chIdentifierP :: TxsParser Text
chIdentifierP = do
    i <-identifier
    when (i `elem` ["CHAN", "IN", "OUT", "SYNC", "BEHAVIOUR"] ) parserZero
    return i

-- | Parser for synchronization sets.
syncSetsP :: TxsParser [Set ChanRef]
syncSetsP = do
    txsSymbol "SYNC"
    syncSetP `sepBy` txsSymbol ","

-- | Parser for synchronization channels.
syncSetP :: TxsParser (Set ChanRef)
syncSetP = do
    txsSymbol "{"
    syncChs <- channelRefP `sepBy` txsSymbol "|"
    txsSymbol "}"
    return $ Set.fromList syncChs
