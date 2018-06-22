{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.StautDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for state automata declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.StautDecl
    (stautDeclP)
where

import           Text.Parsec                 (many, sepBy, sepBy1, try, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.FuncDefs
import           TorXakis.Parser.ValExprDecl
import           TorXakis.Parser.VarDecl

stautDeclP :: TxsParser StautDecl
stautDeclP = declWithParamsP "STAUTDEF" paramsP bodyP True
    where paramsP = do
              cs <- chParamsP
              vs <- fParamsP
              e  <- procExitP
              return (cs, vs, e)
          bodyP (cs, vs, e) n l = mkStautDecl n l cs vs e <$> many stautItemP

stautItemP :: TxsParser StautItem
stautItemP
    =   fmap States      (txsSymbol "STATE" *> (statesDecP `sepBy1` txsSymbol ","))
    <|> fmap StVarDecl   (txsSymbol "VAR"   *> varDeclsP)
    <|> fmap mkInitState (txsSymbol "INIT"  *> stateRefP) <*> stUpdatesP
    <|> fmap Trans       (txsSymbol "TRANS" *> many transitionP)
    where
      stateRefP :: TxsParser StateRef
      stateRefP = mkStateRef <$> tryIdentifier <*> mkLoc

      stUpdatesP :: TxsParser [StUpdate]
      stUpdatesP = txsSymbol "{" *> stUpdateP `sepBy` txsSymbol ";" <* txsSymbol "}"
                 <|> return []

      stUpdateP :: TxsParser StUpdate
      stUpdateP =
          StUpdate <$> (varRefP `sepBy1` txsSymbol ",") <*> (txsSymbol ":=" *> valExpP)

      varRefP :: TxsParser VarRef
      varRefP = flip mkVarRef <$> mkLoc <*> tryIdentifier

      transitionP :: TxsParser Transition
      transitionP = try $ do
          src <- stateRefP
          txsSymbol "->"
          ofrs <- actOfferP
          upds <- stUpdatesP
          txsSymbol "->"
          dst <- stateRefP
          return $ Transition src ofrs upds dst


statesDecP :: TxsParser StateDecl
statesDecP = mkStateDecl <$> tryIdentifier <*> mkLoc



