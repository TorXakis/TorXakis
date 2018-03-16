-- | 

module TorXakis.Compiler.ValExpr.FuncDef where

import qualified Data.Map as Map
import           Data.Map (Map)

import           VarId (VarId)
import           FuncId (FuncId)
import           FuncDef (FuncDef (FuncDef))
import           ValExpr (cstrVar)

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.FuncId

funcDeclsToFuncDefs :: Env -> [FuncDecl] -> CompilerM (Map FuncId (FuncDef VarId))
funcDeclsToFuncDefs e fs = Map.fromList <$> traverse (funcDeclToFuncDef e) fs

funcDeclToFuncDef :: Env -> FuncDecl -> CompilerM (FuncId, FuncDef VarId)
funcDeclToFuncDef e f = do
    fId   <- funcDeclToFuncId e f
    aVids <- traverse (findVarDefM e) (funcParams . child $ f)
    let Var _ m = funcBody . child $ f
    vId <- findVarUseM e (uid m) 
    return (fId, FuncDef aVids (cstrVar vId))


