{-# LANGUAGE OverloadedStrings #-}

module TorXakis.Parser.PurpDecl where

import           Text.Parsec              (many)

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

purpDeclP :: TxsParser PurpDecl
purpDeclP = do
    txsWhitespace
    txsSymbol "PURPDEF"
    l <- mkLoc
    n <- txsLexeme identifier
    txsSymbol "::="
    is <- chansInDecl
    os <- chansOutDecl
    ys <- chansSyncDecl
    ts <- testGoalsP
    txsSymbol "ENDDEF"
    return $ mkPurpDecl n l is os ys ts

testGoalsP :: TxsParser [TestGoalDecl]
testGoalsP = many testGoalP

testGoalP :: TxsParser TestGoalDecl
testGoalP = do
    txsSymbol "GOAL"
    l <-  mkLoc
    n <- txsLexeme identifier
    txsSymbol "::="
    be <- bexpDeclP
    return $ mkTestGoalDecl n l be
