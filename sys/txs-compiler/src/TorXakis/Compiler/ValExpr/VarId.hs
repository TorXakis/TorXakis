{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    pVids <- traverse (varIdFromVarDecl e) (funcParams fd)
    bVids <- varIdsFromExpDecl e (funcBody fd)
    return $ pVids ++ bVids

varIdFromVarDecl :: ( MapsTo (Loc VarDeclE) SortId mm
                     , IsVariable v, HasLoc v VarDeclE)
                 => mm -> v -> CompilerM (Loc VarDeclE, VarId)
varIdFromVarDecl mm v = do
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
        vdMap  <- traverse (varIdFromVarDecl mm) vs
        vdExpMap <- concat <$> traverse (varIdsFromExpDecl mm) (varDeclExp <$> vs)
        subMap <- varIdsFromExpDecl mm subEx
        return $ vdMap ++ subMap ++ vdExpMap
    _ ->
        concat <$> traverse (varIdsFromExpDecl mm) (childExps ex)


class DeclaresVariables e where
    mkVarIds :: MapsTo (Loc VarDeclE) SortId mm
             => mm -> e -> CompilerM [(Loc VarDeclE, VarId)]

instance DeclaresVariables BExpDecl where
    mkVarIds _  Stop                  = return []
    mkVarIds mm (ActPref ao be)       = (++) <$> mkVarIds mm ao <*> mkVarIds mm be
    mkVarIds mm (LetBExp vs be)       = (++) <$> mkVarIds mm vs <*> mkVarIds mm be
    mkVarIds mm (Pappl _ _ _ exs)     = mkVarIds mm exs
    mkVarIds mm (Par _ _ be0 be1)     = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Enable _ be0 be1)    = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Accept _ ofrs be)    = (++) <$> mkVarIds mm ofrs <*> mkVarIds mm be
    mkVarIds mm (Disable _ be0 be1)   = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Interrupt _ be0 be1) = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1    

instance DeclaresVariables ActOfferDecl where
    mkVarIds mm (ActOfferDecl os mEx) = (++) <$> mkVarIds mm os <*> mkVarIds mm mEx

instance DeclaresVariables e => DeclaresVariables (Maybe e) where
    mkVarIds mm = maybe (return []) (mkVarIds mm)

instance DeclaresVariables e => DeclaresVariables [e] where
    mkVarIds mm es = concat <$> traverse (mkVarIds mm) es

instance DeclaresVariables OfferDecl where
    mkVarIds mm (OfferDecl _ os) = mkVarIds mm os

instance DeclaresVariables ChanOfferDecl where
    mkVarIds mm (QuestD vd)  = return <$> varIdFromVarDecl mm vd
    mkVarIds mm (ExclD  ex)  = mkVarIds mm ex

instance DeclaresVariables ExpDecl where
    -- TODO: rename 'varIdsFromExpDecl' to 'mkVarIds'.
    mkVarIds = varIdsFromExpDecl 

instance DeclaresVariables LetVarDecl where
    mkVarIds mm v = return <$> varIdFromVarDecl mm v
