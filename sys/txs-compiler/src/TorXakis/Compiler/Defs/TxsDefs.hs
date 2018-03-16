-- | 

module TorXakis.Compiler.Defs.TxsDefs where

import           Data.Map (Map)
import qualified Data.Map as Map

import           TxsDefs (TxsDefs, empty, sortDefs, cstrDefs, funcDefs)
import           SortId (SortId)
import           SortDef (SortDef (SortDef))
    
import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.CstrDef
import           TorXakis.Compiler.ValExpr.FuncDef
    
adtsToTxsDefs :: Env -> [ADTDecl] -> CompilerM TxsDefs
adtsToTxsDefs e ds = do
    lCstrDefs <- compileToCstrDefs e ds
    return $ empty
        { sortDefs = envToSortDefs e
        , cstrDefs = lCstrDefs
        }

funcsToTxsDef :: Env -> [FuncDecl] -> CompilerM TxsDefs
funcsToTxsDef e fs = do
    lFuncDefs <- funcDeclsToFuncDefs e fs
    return $ empty { funcDefs = lFuncDefs }

-- TODO: put this in a 'SortDef' package.
envToSortDefs :: Env -> Map SortId SortDef    
envToSortDefs e = Map.fromList $
    zip (Map.elems. sortsMap $ e) (repeat SortDef)
