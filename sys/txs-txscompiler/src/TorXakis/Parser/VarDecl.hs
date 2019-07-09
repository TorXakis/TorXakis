{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Parser.VarDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for variable declarations.
--------------------------------------------------------------------------------
module TorXakis.Parser.VarDecl
    (varDeclsP)
where

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data
import           TorXakis.Parser.TypeDefs

-- | Parser for variable declarations.
varDeclsP :: TxsParser [VarDecl]
varDeclsP = idOfSortsP "" "" mkVarDecl

