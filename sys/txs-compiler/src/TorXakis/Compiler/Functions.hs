-- | This module defines functions to compile parsed definitions into
-- functions.

module TorXakis.Compiler.Functions where

import           Data.Text (Text)

import           Sigs                          (Sigs)
import           VarId                         (VarId (VarId))
import           FuncTable                     (FuncTable (FuncTable), SignHandler)
import           TorXakis.Sort.Name            (getName)
import           TorXakis.Sort.ADTDefs         (ADTDefs, Sort)
import           TorXakis.Sort.ConstructorDefs (ConstructorDef)

import           TorXakis.Compiler.Data (CompilerM)

-- > newtype FuncTable v = FuncTable { toMap :: Map.Map Text (SignHandler v) }
-- >
-- > type SignHandler v = Map.Map Signature (Handler v)
-- > data Signature = Signature  { sortArgs :: [SortId]
-- >                             , sortRet  :: SortId
-- >                             }
-- >
-- > type Handler v = [ValExpr v] -> ValExpr v
-- >
adtDefsToFuncTable :: ADTDefs -> CompilerM (FuncTable VarId)
adtDefsToFuncTable ds = undefined ds
    

-- | Create a function from a constructor.
cstrToCstrFunc :: ConstructorDef Sort -> CompilerM (Text, SignHandler VarId)
cstrToCstrFunc c = undefined c
