{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.BExpDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for behavior expression declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.BExpDecl
    ( bexpDeclP
    , procExitP
    , chParamsP
    , offerP
    , chanOfferP
    , actOfferP
    , offersP
    )
where

import qualified Data.Text                   as T
import           Text.Parsec                 (many, notFollowedBy, optionMaybe,
                                              optional, sepBy, sepBy1, try,
                                              (<?>), (<|>))
import           Text.Parsec.Expr            (Assoc (AssocLeft),
                                              Operator (Infix),
                                              buildExpressionParser)

import           TorXakis.Parser.ChanDecl
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs
import           TorXakis.Parser.ValExprDecl

-- | Parser for behavior expressions declarations.
bexpDeclP :: TxsParser BExpDecl
bexpDeclP = buildExpressionParser table bexpTermP
    <?> "Behavior expression"
    where
      -- This table defines the precedence of operators.
      table = [ [ Infix choiceP AssocLeft]
              , [ Infix parOpP AssocLeft]
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
      choiceP :: TxsParser (BExpDecl -> BExpDecl -> BExpDecl)
      choiceP = do
          l <- mkLoc
          try (txsSymbol "##")
          return $ \be0 be1 -> Choice l be0 be1
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
         <|> acceptP
         <|> stopP
         <|> letBExpP
         <|> hideP
         <|> try procInstP
         <|> try guardP
         <|> actPrefixP

stopP :: TxsParser BExpDecl
stopP = try (txsSymbol "STOP") >> return Stop

actPrefixP :: TxsParser BExpDecl
actPrefixP = ActPref <$> mkLoc <*> actOfferP <*> actContP
    where
      actContP = (try (txsSymbol ">->") >> bexpTermP)
             <|> return Stop

guardP :: TxsParser BExpDecl
guardP = do
    g <- try (txsSymbol "[[") *> valExpP <* txsSymbol "]]"
    txsSymbol "=>>"
    Guard g <$> bexpTermP

-- | Parser for action offers.
actOfferP :: TxsParser ActOfferDecl
actOfferP = ActOfferDecl <$> offersP <*> actConstP
    where
      actConstP :: TxsParser (Maybe ExpDecl)
      actConstP =  fmap Just (try (txsSymbol "[[") *> valExpP <* txsSymbol "]]")
               <|> return Nothing

-- | Parser for offers.
offersP :: TxsParser [OfferDecl]
offersP = optional (txsSymbol "{")
           *> actOrOffer `sepBy1` pipe
           <* optional (txsSymbol "}")
    where
      actOrOffer = predefAct <|> offerP
      pipe = try $ do
        txsSymbol "|"
        notFollowedBy ( txsSymbol "|" <|> txsSymbol "[" )

-- | Predefined action parser.
predefAct
    :: TxsParser OfferDecl
predefAct =  predefOffer "ISTEP"
         <|> predefOffer "QSTEP"
         <|> predefOffer "HIT"
         <|> predefOffer "MISS"
    where predefOffer str = try $ do
            l <- mkLoc
            txsSymbol str
            return $ OfferDecl (mkChanRef (T.pack str) l) []

-- | Parser for offers on channels, of the form:
--
-- > ch ? v0 ? ... ? vn
--
-- or
--
-- > ch ! exp0 ! ... ! expn
--
offerP :: TxsParser OfferDecl
offerP = do
    l     <- mkLoc
    n     <- identifier
    OfferDecl (mkChanRef n l) <$> chanOffersP

chanOffersP :: TxsParser [ChanOfferDecl]
chanOffersP = many chanOfferP

-- | Parser for Channel offers.
chanOfferP :: TxsParser ChanOfferDecl
chanOfferP = try questOfferP <|> exclOfferP
    where
      questOfferP = do
          txsSymbol "?"
          l  <- mkLoc
          n  <- identifier
          ms <- optionMaybe ofSortP
          return $ QuestD (mkIVarDecl n l ms)
      exclOfferP = ExclD <$> (txsSymbol "!" *> valExpP)

letBExpP :: TxsParser BExpDecl
letBExpP = do
    try (txsSymbol "LET")
    vss <- letSeqVarDeclsP
    subEx <- inP bexpDeclP
    return $ LetBExp vss subEx

hideP :: TxsParser BExpDecl
hideP = do
    l <- mkLoc
    try (txsSymbol "HIDE")
    crs <- chParamsP
    subEx <- inP bexpDeclP
    return $ Hide l crs subEx

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
    try (txsSymbol "ACCEPT")
    ofrs <- chanOffersP
    be <- inP bexpDeclP
    return $ Accept l ofrs be

chanrefsP :: TxsParser [ChanRef]
chanrefsP = txsSymbol "["
            *> (mkChanRef <$> identifier <*> mkLoc) `sepBy` txsSymbol ","
            <* txsSymbol "]"

-- | Parser for channel parameters.
chParamsP :: TxsParser [ChanDecl]
chParamsP = do
    txsSymbol "["
    res <- concat <$> chanDeclsOfSortP `sepBy` txsSymbol ";"
    txsSymbol "]"
    return res

-- | Parser for exit declarations.
procExitP :: TxsParser ExitSortDecl
procExitP =  (ExitD <$> (txsSymbol "EXIT" *> sortP `sepBy` txsSymbol "#"))
         <|> (txsSymbol "HIT" >> return HitD)
         <|> return NoExitD
