{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.CnectDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for connect declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.CnectDecl
    (cnectDeclP)
where

import           Data.Either              (partitionEithers)
import           Text.Parsec              (many, try, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

-- | Parser for connect declarations.
cnectDeclP :: TxsParser CnectDecl
cnectDeclP = declP "CNECTDEF" $ \n l -> do
    ct <- cnectTypeP
    (is, cs) <- partitionEithers <$>
        many (fmap Left cnectItemP <|> fmap Right codecItemP)
    return $ mkCnectDecl n l ct is cs

cnectTypeP :: TxsParser CnectType
cnectTypeP =   (txsSymbol "CLIENTSOCK" >> return CTClient)
           <|> (txsSymbol "SERVERSOCK" >> return CTServer)

cnectItemP :: TxsParser CnectItem
cnectItemP = do
    txsSymbol "CHAN"
    cType <-  (txsSymbol "IN" >> return ChanIn)
          <|> (txsSymbol "OUT" >> return ChanOut)
    ch    <- channelRefP
    txsSymbol "HOST"
    h     <- txsStringP
    txsSymbol "PORT"
    p     <- txsIntP
    return $ CnectItem ch cType h p

codecItemP :: TxsParser CodecItem
codecItemP = do
    (cType, str) <-  try $   (txsSymbol "DECODE" >> return (Decode, "<-"))
                         <|> (txsSymbol "ENCODE" >> return (Encode, "->"))
    offr         <- offerP
    txsSymbol str
    chOffr       <- chanOfferP
    return $ CodecItem offr chOffr cType
