-- |

module TorXakis.Compiler.ValExpr.FuncDef where

import           Data.Map                          (Map)
import qualified Data.Map                          as Map

import           FuncDef                           (FuncDef (FuncDef))
import           FuncId                            (FuncId)
import           ValExpr                           (cstrVar)
import           VarId                             (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

funcDeclsToFuncDefs :: (HasSortIds e, HasVarDecls e, HasVarIds e)
                    => e
                    -> [FuncDecl]
                    -> CompilerM (Map (Loc FuncDeclE) FuncId, Map FuncId (FuncDef VarId))
funcDeclsToFuncDefs e fs = do
    iFidFds <- traverse (funcDeclToFuncDef e) fs
    return ( Map.fromList $ zip (fst' <$> iFidFds) (snd' <$> iFidFds)
           , Map.fromList $ zip (snd' <$> iFidFds) (thr' <$> iFidFds)
           )
    where
      fst' (a, _, _) = a
      snd' (_, b, _) = b
      thr' (_, _, c) = c

funcDeclToFuncDef :: (HasSortIds e, HasVarDecls e, HasVarIds e)
                  => e -> FuncDecl -> CompilerM (Loc FuncDeclE, FuncId, FuncDef VarId)
funcDeclToFuncDef e f = do
    fId  <- funcDeclToFuncId e f
    pIds <- traverse (findVarIdM e . getLoc) (funcParams f)
    vExp <- expDeclToValExpr e (funcBody f)
    return (getLoc f, fId, FuncDef pIds vExp)

