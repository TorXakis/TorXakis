{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.VarId
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' variable id's.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.VarId
    ( mkVarIds
    , varIdFromVarDecl
    , DeclaresVariables
    , generateVarIds
    )
where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           GHC.Exts                 (toList)

import           Id                       (Id (Id))
import           SortId                   (SortId)
import           VarId                    (VarId (VarId))

import           TorXakis.Compiler.Data   (CompilerM, getNextId)
import           TorXakis.Compiler.MapsTo (MapsTo, lookupM)
import           TorXakis.Parser.Data     (ActOfferDecl (ActOfferDecl), BExpDecl (Accept, ActPref, Choice, Disable, Enable, Guard, Hide, Interrupt, LetBExp, Pappl, Par, Stop),
                                           ChanOfferDecl (ExclD, QuestD),
                                           ExpChild (LetExp), ExpDecl, FuncDecl,
                                           HasLoc, IsVariable, LetVarDecl, Loc,
                                           OfferDecl (OfferDecl), ParLetVarDecl,
                                           TestGoalDecl,
                                           Transition (Transition), VarDecl,
                                           VarDeclE, childExps, expChild,
                                           funcBody, funcParams, getLoc,
                                           testGoalDeclBExp, varDeclExp,
                                           varName)

-- | Generate the variable ids that are defined in a list of function
-- declarations.
generateVarIds :: (MapsTo (Loc VarDeclE) SortId mm)
               => mm -> [FuncDecl] -> CompilerM (Map (Loc VarDeclE) VarId)
generateVarIds mm fs = Map.fromList . concat <$>
    traverse (varIdsFromFuncDecl mm) fs

-- | Generate the variable ids that are defined in a function declaration.
varIdsFromFuncDecl :: (MapsTo (Loc VarDeclE) SortId mm)
                   => mm -> FuncDecl -> CompilerM [(Loc VarDeclE, VarId)]
varIdsFromFuncDecl e fd = do
    pVids <- traverse (varIdFromVarDecl e) (funcParams fd)
    bVids <- varIdsFromExpDecl e (funcBody fd)
    return $ pVids ++ bVids

-- | Generate a variable id from a variable declaration.
varIdFromVarDecl :: ( MapsTo (Loc VarDeclE) SortId mm
                     , IsVariable v, HasLoc v VarDeclE)
                 => mm -> v -> CompilerM (Loc VarDeclE, VarId)
varIdFromVarDecl mm v = do
    sId <- lookupM (getLoc v) mm
    vId <- getNextId
    return (getLoc v, VarId (varName v) (Id vId) sId)

-- | Generate 'VarId''s for each variable declaration.
--
varIdsFromExpDecl :: (MapsTo (Loc VarDeclE) SortId mm)
                  => mm -> ExpDecl -> CompilerM [(Loc VarDeclE, VarId)]
varIdsFromExpDecl mm ex = case expChild ex of
-- NOTE: property to check:
--
-- the number of 'Loc VarDeclE' equals the length of the list returned by this function.
--
-- This will imply that each location of a variable declaration introduces a 'VarId'.
--
-- Maybe this function can be replaced by the use of @getKvs@ in the @DefinesAMap@ class.
    LetExp vs subEx -> do
        vdMap  <- concat <$> traverse (traverse (varIdFromVarDecl mm)) (toList <$> vs)
        vdExpMap <- concat <$>
            traverse (fmap concat . traverse (varIdsFromExpDecl mm) . fmap varDeclExp) (toList <$> vs)
        subMap <- varIdsFromExpDecl mm subEx
        return $ vdMap ++ subMap ++ vdExpMap
    _ ->
        concat <$> traverse (varIdsFromExpDecl mm) (childExps ex)

-- | Expressions that declare variables.
class DeclaresVariables e where
    -- | Create variable id's from the given expression.
    mkVarIds :: MapsTo (Loc VarDeclE) SortId mm
             => mm -> e -> CompilerM [(Loc VarDeclE, VarId)]

instance DeclaresVariables VarDecl where
    mkVarIds mm = fmap pure . varIdFromVarDecl mm

instance DeclaresVariables BExpDecl where
    mkVarIds _  Stop                  = return []
    mkVarIds mm (ActPref _ ao be)     = (++) <$> mkVarIds mm ao <*> mkVarIds mm be
    mkVarIds mm (LetBExp vs be)       = (++) <$> mkVarIds mm vs <*> mkVarIds mm be
    mkVarIds mm (Pappl _ _ _ exs)     = mkVarIds mm exs
    mkVarIds mm (Par _ _ be0 be1)     = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Enable _ be0 be1)    = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Accept _ ofrs be)    = (++) <$> mkVarIds mm ofrs <*> mkVarIds mm be
    mkVarIds mm (Disable _ be0 be1)   = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Interrupt _ be0 be1) = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Choice _ be0 be1)    = (++) <$> mkVarIds mm be0 <*> mkVarIds mm be1
    mkVarIds mm (Guard ex be)         = (++) <$> mkVarIds mm ex <*> mkVarIds mm be
    mkVarIds mm (Hide _ _ be)         = mkVarIds mm be

instance DeclaresVariables ParLetVarDecl where
    mkVarIds mm = mkVarIds mm . toList

instance DeclaresVariables ActOfferDecl where
    mkVarIds mm (ActOfferDecl os mEx) = (++) <$> mkVarIds mm os <*> mkVarIds mm mEx

instance DeclaresVariables e => DeclaresVariables (Maybe e) where
    mkVarIds mm = maybe (return []) (mkVarIds mm)

instance DeclaresVariables e => DeclaresVariables [e] where
    mkVarIds mm es = concat <$> traverse (mkVarIds mm) es

instance DeclaresVariables OfferDecl where
    mkVarIds mm (OfferDecl _ os) = mkVarIds mm os

instance DeclaresVariables ChanOfferDecl where
    mkVarIds mm (QuestD vd) = return <$> varIdFromVarDecl mm vd
    mkVarIds mm (ExclD  ex) = mkVarIds mm ex

instance DeclaresVariables ExpDecl where
    mkVarIds = varIdsFromExpDecl

instance DeclaresVariables LetVarDecl where
    mkVarIds mm v = return <$> varIdFromVarDecl mm v

instance DeclaresVariables Transition where
    mkVarIds mm (Transition _ ofr _ _ ) = mkVarIds mm ofr

instance DeclaresVariables TestGoalDecl where
    mkVarIds mm gd = mkVarIds mm (testGoalDeclBExp gd)
