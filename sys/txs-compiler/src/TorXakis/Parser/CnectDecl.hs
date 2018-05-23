module TorXakis.Parser.CnectDecl where

import           Data.Either              (partitionEithers)
import           Text.Parsec              (many, try, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

cnectDeclP :: TxsParser CnectDecl
cnectDeclP = do
    txsSymbol "CNECTDEF"
    l <- mkLoc
    n <- txsLexeme identifier
    txsSymbol "::="
    ct <- cnectTypeP
    (is, cs) <- partitionEithers <$>
        many (fmap Left cnectItemP <|> fmap Right codecItemP)
    txsSymbol "ENDDEF"
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
                         <|> (txsSymbol "ENCODE" >> return (Decode, "->"))
    offr         <- offerP
    txsSymbol str
    chOffr       <- chanOfferP
    return $ CodecItem offr chOffr cType
