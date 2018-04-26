module TorXakis.Parser.BExpDecl (bexpDeclP) where

import           Control.Monad               (void)
import qualified Data.Text                   as T
import           Text.Parsec                 (many, notFollowedBy, optionMaybe,
                                              sepBy, sepBy1, try, (<?>), (<|>))
import           Text.Parsec.Expr            (Assoc (AssocLeft),
                                              Operator (Infix),
                                              buildExpressionParser)

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs
import           TorXakis.Parser.ValExprDecl
import           TorXakis.Parser.VarDecl

bexpDeclP :: TxsParser BExpDecl
bexpDeclP = buildExpressionParser table bexpTermP
    <?> "Behavior expression"
    where
      table = [ [Infix parOpP AssocLeft]
              , [ Infix enableP AssocLeft
                , Infix disableP AssocLeft
                , Infix interruptP AssocLeft]
              ]
      enableP :: TxsParser (BExpDecl -> BExpDecl ->  BExpDecl)
      enableP = do
          l <- mkLoc
          txsSymbol ">>>"
          return $ \be0 be1 -> Enable l be0 be1
      disableP :: TxsParser (BExpDecl -> BExpDecl -> BExpDecl)
      disableP = do
          l <- mkLoc
          try (txsSymbol "[>>")
          return $ \be0 be1 -> Disable l be0 be1
      interruptP :: TxsParser (BExpDecl -> BExpDecl -> BExpDecl)
      interruptP = do
          l <- mkLoc
          try (txsSymbol "[><")
          return $ \be0 be1 -> Interrupt l be0 be1
      parOpP :: TxsParser (BExpDecl -> BExpDecl ->  BExpDecl)
      parOpP = do
          l <- mkLoc
          txsSymbol "|"
          sOn <-
              -- '|||'  operator
                  (try (txsSymbol "||") >> return (OnlyOn []))
              -- '[..]' operator
              <|> (fmap OnlyOn chanrefsP <* txsSymbol "|")
              -- '||'   operator
              <|> (txsSymbol "|" >> return All)
          return $ \be0 be1 -> Par l sOn be0 be1

bexpTermP :: TxsParser BExpDecl
bexpTermP =  txsSymbol "(" *> ( bexpDeclP <* txsSymbol ")")
         <|> try acceptP
         <|> try stopP
         <|> try letBExpP
         <|> try procInstP
         <|> actPrefixP

stopP :: TxsParser BExpDecl
stopP = txsSymbol "STOP" >> return Stop

actPrefixP :: TxsParser BExpDecl
actPrefixP = ActPref <$> actOfferP <*> actContP
    where
      actContP = (try (txsSymbol ">->") >> bexpTermP)--bexpDeclP)
             <|> return Stop

actOfferP :: TxsParser ActOfferDecl
actOfferP = ActOfferDecl <$> offersP <*> actConstP
    where
      actConstP :: TxsParser (Maybe ExpDecl)
      actConstP =  fmap Just (try (txsSymbol "[[") *> valExpP <* txsSymbol "]]")
               <|> return Nothing

      offersP :: TxsParser [OfferDecl]
      offersP =  predefOffer "ISTEP"
             <|> predefOffer "QSTEP"
             <|> predefOffer "HIT"
             <|> predefOffer "MISS"
             <|> offerP `sepBy1` try (
                     txsSymbol "|"
                     >> notFollowedBy ( txsSymbol "|" <|> txsSymbol "[" )
                 )
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
chanOffersP = many (try questOfferP <|> exclOfferP)
    where
      questOfferP = do
          txsSymbol "?"
          l  <- mkLoc
          n  <- identifier
          ms <- optionMaybe ofSortP
          return $ QuestD  (mkIVarDecl n l ms)
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
    crs  <- chanrefsP
    exps <-   txsSymbol "("
           *> valExpP `sepBy` txsSymbol ","
           <* txsSymbol ")"
    return $ Pappl (procRefName pN) l crs exps

acceptP :: TxsParser BExpDecl
acceptP = do
    l <- mkLoc
    txsSymbol "ACCEPT"
    ofrs <- chanOffersP
    txsSymbol "IN"
    be <- bexpDeclP
    txsSymbol "NI"
    return $ Accept l ofrs be

chanrefsP :: TxsParser [ChanRef]
chanrefsP = txsSymbol "["
            *> (mkChanRef <$> identifier <*> mkLoc) `sepBy` txsSymbol ","
            <* txsSymbol "]"
