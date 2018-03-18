module TorXakis.Compiler.ValExpr.VarId where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Id (Id (Id))
import           VarId (VarId (VarId))

import TorXakis.Parser.Data
import TorXakis.Compiler.Data

generateVarIds :: (HasSortIds e)
               => e -> [FuncDecl] -> CompilerM (Map (Loc Field) VarId)
generateVarIds e fs = Map.fromList . concat <$>
    traverse (varIdsFromFuncDecl e) fs

varIdsFromFuncDecl :: (HasSortIds e)
                   => e -> FuncDecl -> CompilerM [(Loc Field, VarId)]
varIdsFromFuncDecl e fd =
    traverse (varIdsFromFieldDecl e) (funcParams . child $ fd)

varIdsFromFieldDecl :: (HasSortIds e)
                    => e -> FieldDecl -> CompilerM (Loc Field, VarId)
varIdsFromFieldDecl e f = do
    sId <- findSortIdM e (nodeNameT . child $ f)
    vId <- getNextId
    return (getLoc f, VarId (nodeNameT f) (Id vId) sId)
