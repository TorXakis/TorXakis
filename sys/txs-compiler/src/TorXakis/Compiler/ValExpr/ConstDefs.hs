-- |

module TorXakis.Compiler.ValExpr.ConstDefs where

import qualified ConstDefs
import           TorXakis.Parser.Data
import qualified ValExpr

constToConstDef :: Const -> ConstDefs.Const
constToConstDef (BoolConst b)   = ConstDefs.Cbool b
constToConstDef (IntConst i)    = ConstDefs.Cint i
constToConstDef (StringConst s) = ConstDefs.Cstring s
