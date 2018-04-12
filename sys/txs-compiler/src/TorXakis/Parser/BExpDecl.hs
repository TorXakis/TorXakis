-- |

module TorXakis.Parser.BExpDecl (bexpDeclP) where

import qualified Data.Text                   as T
import           Text.Parsec                 (many, sepBy, sepBy1, try, (<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.ValExprDecl
import           TorXakis.Parser.VarDecl

bexpDeclP :: TxsParser BExpDecl
bexpDeclP =  try stopP
         <|> actPrefixP

stopP :: TxsParser BExpDecl
stopP = txsSymbol "STOP" >> return Stop

actPrefixP :: TxsParser BExpDecl
actPrefixP = ActPref <$> actOfferP <*> actContP
    where
      actContP = (txsSymbol ">->" >> bexpDeclP)
             <|> return Stop

actOfferP :: TxsParser ActOffer
actOfferP = ActOffer <$> offersP <*> actConstP
    where
      actConstP :: TxsParser (Maybe ExpDecl)
      actConstP =  fmap Just (txsSymbol "[[" *> valExpP <* txsSymbol "]]")
               <|> return Nothing

      offersP :: TxsParser [Offer]
      offersP =  predefOffer "ISTEP"
             <|> predefOffer "QSTEP"
             <|> predefOffer "HIT"
             <|> predefOffer "MISS"
             <|> offerP `sepBy1` txsSymbol "|"
          where predefOffer str = try $ do
                    l <- mkLoc
                    txsSymbol str
                    return [Offer (mkChanRef (T.pack str) l) []]

      offerP :: TxsParser Offer
      offerP = do
          l     <- mkLoc
          n     <- identifier
          chOfs <- chanOffersP
          return $ Offer (mkChanRef n l) chOfs

      chanOffersP :: TxsParser [ChanOffer]
      chanOffersP = many (try questOfferVDP <|> questOfferP <|> exclOfferP)
          where
            questOfferVDP = do
                txsSymbol "?"
                vd <- varDeclP
                return $ Quest (Right vd)
            questOfferP = do
                txsSymbol "?"
                l <- mkLoc
                n <- identifier
                return $ Quest (Left (mkVarRef n l))
            exclOfferP = Excl <$> (txsSymbol "!" *> valExpP)
