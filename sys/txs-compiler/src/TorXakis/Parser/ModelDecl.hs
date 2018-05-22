{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Parser.ModelDecl where

import           Control.Monad            (when)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import           Text.Parsec              (parserZero, sepBy, try, (<|>))

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

modelDeclP :: TxsParser ModelDecl
modelDeclP = do
    txsWhitespace
    txsSymbol "MODELDEF"
    l  <- mkLoc
    n  <- txsLexeme identifier
    txsSymbol "::="
    is <- chansInDecl
    os <- chansOutDecl
    ys <- chansSyncDecl
    txsSymbol "BEHAVIOUR"
    be <- bexpDeclP
    txsSymbol "ENDDEF"
    return $ mkModelDecl n l is os ys be
