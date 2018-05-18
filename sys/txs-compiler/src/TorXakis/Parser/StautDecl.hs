module TorXakis.Parser.StautDecl where

import           Text.Parsec                 (many, sepBy, sepBy1, try, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.FuncDefs
import           TorXakis.Parser.ValExprDecl
import           TorXakis.Parser.VarDecl

stautDeclP :: TxsParser StautDecl
stautDeclP = do
    txsSymbol "STAUTDEF"
    l  <- mkLoc
    n  <- identifier
    cs <- chParamsP
    vs <- fParamsP
    e  <- procExitP
    txsSymbol "::="
    is <- many stautItemP
    txsSymbol "ENDDEF"
    return $ mkStautDecl n l cs vs e is

stautItemP :: TxsParser StautItem
stautItemP
    =   fmap States    (txsSymbol "STATE" *> (statesDecP `sepBy1` txsSymbol ","))
    <|> fmap StVarDecl (txsSymbol "VAR"   *> varDeclsP)
    <|> fmap InitState (txsSymbol "INIT"  *> stateRefP) <*> stUpdatesP
    <|> fmap Trans     (txsSymbol "TRANS" *> many transitionP)
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



