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

funcDeclsToFuncDefs :: (HasSortIds e, HasVarDecls e, HasVarIds e)
                    => e
                    -> [FuncDecl]
                    -> CompilerM (Map (Loc Func) FuncId, Map FuncId (FuncDef VarId))
funcDeclsToFuncDefs e fs = do
    iFidFds <- traverse (funcDeclToFuncDef e) fs
    return ( Map.fromList $ zip (fst' <$> iFidFds) (snd' <$> iFidFds)
           , Map.fromList $ zip (snd' <$> iFidFds) (thr <$> iFidFds)
           )
    where
      fst' (a, _, _) = a
      snd' (_, b, _) = b
      thr (_, _, c) = c

funcDeclToFuncDef :: (HasSortIds e, HasVarDecls e, HasVarIds e)
                  => e -> FuncDecl -> CompilerM (Loc Func, FuncId, FuncDef VarId)
funcDeclToFuncDef e f = do
    fId   <- funcDeclToFuncId e f
    aVids <- traverse (findVarIdM e . getLoc) (funcParams . child $ f)
    let VarExp _ m = funcBody . child $ f
    fDecl <- findVarDeclM e (loc m)
    vId   <- findVarIdM e (getLoc fDecl)
    return (loc . nodeMdata $ f, fId, FuncDef aVids (cstrVar vId))
