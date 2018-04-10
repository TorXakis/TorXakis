-- |

module TorXakis.Compiler.ValExpr.ConstDefs where

import qualified ConstDefs
import           SortId               (SortId)

import           TorXakis.Parser.Data

constToConstDef :: SortId -> Const -> ConstDefs.Const
constToConstDef _ (BoolConst b)   = ConstDefs.Cbool b
constToConstDef _ (IntConst i)    = ConstDefs.Cint i
constToConstDef _ (StringConst s) = ConstDefs.Cstring s
constToConstDef _ (RegexConst s)  = ConstDefs.Cregex s
constToConstDef sId AnyConst      = ConstDefs.Cany sId
