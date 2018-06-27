{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.ModelDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for model declarations.
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Parser.ModelDecl
    (modelDeclP)
where

import           TorXakis.Parser.BExpDecl
import           TorXakis.Parser.ChanRef
import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

-- | Parser for model declarations.
modelDeclP :: TxsParser ModelDecl
modelDeclP = declP "MODELDEF" $ \n l -> do
    is <- chansInDecl
    os <- chansOutDecl
    ys <- chansSyncDecl
    txsSymbol "BEHAVIOUR"
    be <- bexpDeclP
    return $ mkModelDecl n l is os ys be
