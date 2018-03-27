{-# LANGUAGE FlexibleContexts #-}
module TorXakis.Compiler.ValExpr.VarId where

import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Id                     (Id (Id))
import           VarId                  (VarId (VarId))

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

generateVarIds :: (HasVarSortIds e)
               => e -> [FuncDecl] -> CompilerM (Map (Loc VarDeclE) VarId)
generateVarIds e fs = Map.fromList . concat <$>
    traverse (varIdsFromFuncDecl e) fs

varIdsFromFuncDecl :: (HasVarSortIds e)
                   => e -> FuncDecl -> CompilerM [(Loc VarDeclE, VarId)]
varIdsFromFuncDecl e fd = do
    pVids <- traverse (varIdsFromVarDecl e) (funcParams fd)
    bVids <- varIdsFromExpDecl e (funcBody fd)
    return $ pVids ++ bVids

varIdsFromVarDecl :: (HasVarSortIds e, IsVariable v, HasLoc v VarDeclE)
                  => e -> v -> CompilerM (Loc VarDeclE, VarId)
varIdsFromVarDecl e v = do
    sId <- findVarDeclSortIdM e (getLoc v)
    vId <- getNextId
    return (getLoc v, VarId (varName v) (Id vId) sId)

varIdsFromExpDecl :: (HasVarSortIds e)
                  => e -> ExpDecl -> CompilerM [(Loc VarDeclE, VarId)]
varIdsFromExpDecl e ex = case expChild ex of
    LetExp vs subEx -> do
        vdMap  <- traverse (varIdsFromVarDecl e) vs
        subMap <- varIdsFromExpDecl e subEx
        return $ vdMap ++ subMap
    VarRef _ _      ->
         -- No variables are declared when a variable is referred in an
         -- expression.
        return []
    ConstLit _      ->
        -- No variable is declared in a constant.
        return []

