module TorXakis.Parser.BExpDecl (bexpDeclP) where

import qualified Data.Text                   as T
import           Text.Parsec                 (many, sepBy, sepBy1, try, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.ValExprDecl
import           TorXakis.Parser.VarDecl

bexpDeclP :: TxsParser BExpDecl
bexpDeclP =  try stopP
         <|> try letBExpP
         <|> try procInstP
         <|> actPrefixP

stopP :: TxsParser BExpDecl
stopP = txsSymbol "STOP" >> return Stop

actPrefixP :: TxsParser BExpDecl
actPrefixP = ActPref <$> actOfferP <*> actContP
    where
      actContP = (txsSymbol ">->" >> bexpDeclP)
             <|> return Stop

actOfferP :: TxsParser ActOfferDecl
actOfferP = ActOfferDecl <$> offersP <*> actConstP
    where
      actConstP :: TxsParser (Maybe ExpDecl)
      actConstP =  fmap Just (txsSymbol "[[" *> valExpP <* txsSymbol "]]")
               <|> return Nothing

      offersP :: TxsParser [OfferDecl]
      offersP =  predefOffer "ISTEP"
             <|> predefOffer "QSTEP"
             <|> predefOffer "HIT"
             <|> predefOffer "MISS"
             <|> offerP `sepBy1` txsSymbol "|"
          where predefOffer str = try $ do
                    l <- mkLoc
                    txsSymbol str
                    return [OfferDecl (mkChanRef (T.pack str) l) []]

      offerP :: TxsParser OfferDecl
      offerP = do
          l     <- mkLoc
          n     <- identifier
          chOfs <- chanOffersP
          return $ OfferDecl (mkChanRef n l) chOfs

      chanOffersP :: TxsParser [ChanOfferDecl]
      chanOffersP = many (try questOfferVDP <|> exclOfferP)
          where
            questOfferVDP = do
                txsSymbol "?"
                l <- mkLoc -- This offer always introduce a new implicit variable
                n <- identifier
                return $ QuestD (mkIVarDecl n l)
            exclOfferP = ExclD <$> (txsSymbol "!" *> valExpP)

letBExpP :: TxsParser BExpDecl
letBExpP = do
    txsSymbol "LET"
    vs <- letVarDeclsP
    txsSymbol "IN"
    subEx <- bexpDeclP
    txsSymbol "NI"
    return $ LetBExp vs subEx

procInstP :: TxsParser BExpDecl
procInstP = do
    l    <- mkLoc
    pN   <- identifier
    crs  <-    txsSymbol "["
            *> (mkChanRef <$> identifier <*> mkLoc) `sepBy` txsSymbol ","
            <* txsSymbol "]"
    exps <-   txsSymbol "("
           *> valExpP `sepBy` txsSymbol ","
           <* txsSymbol ")"
    return $ Pappl (procRefName pN) l crs exps
