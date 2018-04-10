module TorXakis.Parser.ChanDecl where

import           Control.Monad            (replicateM)
import           Text.Parsec              (sepBy, sepBy1, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

chanDeclsP :: TxsParser [ChanDecl]
chanDeclsP = do
    txsSymbol "CHANDEF"
    _ <- identifier -- The `CHANDEF` name is not used.
    txsSymbol "::="
    res <- concat <$> chanDeclsOfSortP `sepBy1` txsSymbol ";"
    txsSymbol "ENDDEF"
    return res

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
