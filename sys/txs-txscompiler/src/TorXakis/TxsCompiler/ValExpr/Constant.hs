{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TxsCompiler.ValExpr.Constant
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation to 'TorXakis' @Constant@'s.
--------------------------------------------------------------------------------
module TorXakis.TxsCompiler.ValExpr.Constant
    (constToConstant)
where

import           TorXakis.Sort               (Sort)
import           TorXakis.Value

import           TorXakis.Parser.Data (Const (AnyConst, BoolConst, IntConst, RegexConst, StringConst))

-- | Transform a constant declaration into a 'TorXakis' @Constant@.
constToConstant :: Sort -> Const -> Value
constToConstant _ (BoolConst b)   = Cbool b
constToConstant _ (IntConst i)    = Cint i
constToConstant _ (StringConst s) = Cstring s
constToConstant _ (RegexConst s)  = Cregex s
constToConstant s AnyConst        = Cany s
