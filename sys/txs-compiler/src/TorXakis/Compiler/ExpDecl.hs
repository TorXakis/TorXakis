-- | 

module TorXakis.Compiler.ExpDecl where

import           Data.Map (Map)

import TorXakis.Parser.Data
import TorXakis.Compiler.Data

generateVarDecls :: (HasSortIds e)
                 => e -> [FuncDecl] -> CompilerM (Map (Loc Exp) FieldDecl)
generateVarDecls = undefined
