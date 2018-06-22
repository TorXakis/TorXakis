{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.ChanDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for channel declarations
--------------------------------------------------------------------------------
module TorXakis.Parser.ChanDecl
    ( chanDeclsOfSortP
    , chanDeclsP
    )
where

import           Control.Monad            (replicateM)
import           Text.Parsec              (sepBy, sepBy1, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

chanDeclsP :: TxsParser [ChanDecl]
chanDeclsP = declP "CHANDEF" $ \_ _ ->
    concat <$> chanDeclsOfSortP `sepBy1` txsSymbol ";"

chanDeclsOfSortP :: TxsParser [ChanDecl]
chanDeclsOfSortP = do
    chNames <- identifier `sepBy` txsSymbol ","
    chSorts <- sortsProdP
    chdLocs <- replicateM (length chNames) mkLoc
    let mkChanDeclWithSort (n, l) = mkChanDecl n l chSorts
    return $ mkChanDeclWithSort <$> zip chNames chdLocs

sortsProdP :: TxsParser [OfSort]
sortsProdP = neSortsProdP <|> return []
    where
      neSortsProdP = txsSymbol "::" *> sortP `sepBy1` txsSymbol "#"
