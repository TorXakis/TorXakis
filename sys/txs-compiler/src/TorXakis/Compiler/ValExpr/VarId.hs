{-# LANGUAGE FlexibleContexts #-}
module TorXakis.Compiler.ValExpr.VarId where

import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Id                     (Id (Id))
import           VarId                  (VarId (VarId))
import           SortId                 (SortId)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data
import           TorXakis.Compiler.MapsTo

generateVarIds :: (MapsTo (Loc VarDeclE) SortId mm)
               => mm -> [FuncDecl] -> CompilerM (Map (Loc VarDeclE) VarId)
generateVarIds mm fs = Map.fromList . concat <$>
    traverse (varIdsFromFuncDecl mm) fs

varIdsFromFuncDecl :: (MapsTo (Loc VarDeclE) SortId mm)
                   => mm -> FuncDecl -> CompilerM [(Loc VarDeclE, VarId)]
varIdsFromFuncDecl e fd = do
    pVids <- traverse (varIdsFromVarDecl e) (funcParams fd)
    bVids <- varIdsFromExpDecl e (funcBody fd)
    return $ pVids ++ bVids

varIdsFromVarDecl :: ( MapsTo (Loc VarDeclE) SortId mm
                     , IsVariable v, HasLoc v VarDeclE)
                  => mm -> v -> CompilerM (Loc VarDeclE, VarId)
varIdsFromVarDecl mm v = do
    sId <- lookupM (getLoc v) mm
    vId <- getNextId
    return (getLoc v, VarId (varName v) (Id vId) sId)

-- | Generate 'VarId''s for each variable declaration.
--
-- TODO: property to check:
--
-- the number of 'Loc VarDeclE' equals the length of the list returned by this function.
--
-- This will imply that each location of a variable declaration introduces a 'VarId'.
varIdsFromExpDecl :: (MapsTo (Loc VarDeclE) SortId mm)
                  => mm -> ExpDecl -> CompilerM [(Loc VarDeclE, VarId)]
varIdsFromExpDecl mm ex = case expChild ex of
    LetExp vs subEx -> do
        vdMap  <- traverse (varIdsFromVarDecl mm) vs
        vdExpMap <- concat <$> traverse (varIdsFromExpDecl mm) (varDeclExp <$> vs)
        subMap <- varIdsFromExpDecl mm subEx
        return $ vdMap ++ subMap ++ vdExpMap
    _ ->
        concat <$> traverse (varIdsFromExpDecl mm) (childExps ex)


