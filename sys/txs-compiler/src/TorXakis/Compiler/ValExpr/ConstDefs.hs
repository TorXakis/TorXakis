{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.ConstDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation to 'TorXakis' @Const@'s.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.ConstDefs
    (constToConstDef)
where

import qualified ConstDefs
import           SortId               (SortId)

import           TorXakis.Parser.Data (Const (AnyConst, BoolConst, IntConst, RegexConst, StringConst))

-- | Transform a constant declaration into a 'TorXakis' @Const@.
constToConstDef :: SortId -> Const -> ConstDefs.Const
constToConstDef _ (BoolConst b)   = ConstDefs.Cbool b
constToConstDef _ (IntConst i)    = ConstDefs.Cint i
constToConstDef _ (StringConst s) = ConstDefs.Cstring s
constToConstDef _ (RegexConst s)  = ConstDefs.Cregex s
constToConstDef sId AnyConst      = ConstDefs.Cany sId
