{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.PurpDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for purpose declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.PurpDecl
    (purpDeclP)
where

import           Text.Parsec              (many)

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

-- | Parser for purpose declarations.
purpDeclP :: TxsParser PurpDecl
purpDeclP = declP "PURPDEF" $ \n l -> do
    is <- chansInDecl
    os <- chansOutDecl
    ys <- chansSyncDecl
    ts <- testGoalsP
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
