{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.Constant
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation to 'TorXakis' @Constant@'s.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.Constant
    (constToConstant)
where

import qualified Constant
import           SortId               (SortId)

import           TorXakis.Parser.Data (Const (AnyConst, BoolConst, IntConst, RegexConst, StringConst))

-- | Transform a constant declaration into a 'TorXakis' @Constant@.
constToConstant :: SortId -> Const -> Constant.Constant
constToConstant _ (BoolConst b)   = Constant.Cbool b
constToConstant _ (IntConst i)    = Constant.Cint i
constToConstant _ (StringConst s) = Constant.Cstring s
constToConstant _ (RegexConst s)  = Constant.Cregex s
constToConstant sId AnyConst      = Constant.Cany sId
