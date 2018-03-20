module TorXakis.Compiler.ValExpr.VarId where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Id (Id (Id))
import           VarId (VarId (VarId))

import TorXakis.Parser.Data
import TorXakis.Compiler.Data

generateVarIds :: (HasSortIds e)
               => e -> [FuncDecl] -> CompilerM (Map (Loc VarDeclE) VarId)
generateVarIds e fs = Map.fromList . concat <$>
    traverse (varIdsFromFuncDecl e) fs

varIdsFromFuncDecl :: (HasSortIds e)
                   => e -> FuncDecl -> CompilerM [(Loc VarDeclE, VarId)]
varIdsFromFuncDecl e fd =
    traverse (varIdsFromFieldDecl e) (funcParams fd)

varIdsFromFieldDecl :: (HasSortIds e)
                    => e -> VarDecl -> CompilerM (Loc VarDeclE, VarId)
varIdsFromFieldDecl e f = do
    sId <- findSortIdM e (varDeclSort f)
    vId <- getNextId
    return (getLoc f, VarId (varName f) (Id vId) sId)
