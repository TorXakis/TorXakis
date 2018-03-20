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
import           TorXakis.Compiler.ValExpr.FuncId

funcDeclsToFuncDefs :: (HasSortIds e, HasVarDecls e, HasVarIds e)
                    => e
                    -> [FuncDecl]
                    -> CompilerM (Map (Loc FuncDeclE) FuncId, Map FuncId (FuncDef VarId))
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
                  => e -> FuncDecl -> CompilerM (Loc FuncDeclE, FuncId, FuncDef VarId)
funcDeclToFuncDef e f = do
    fId   <- funcDeclToFuncId e f
    aVids <- traverse (findVarIdM e . getLoc) (funcParams f)
    fDecl <- findVarDeclM e (getLoc (funcBody f))
    vId   <- findVarIdM e (getLoc fDecl)
    return (getLoc f, fId, FuncDef aVids (cstrVar vId))
