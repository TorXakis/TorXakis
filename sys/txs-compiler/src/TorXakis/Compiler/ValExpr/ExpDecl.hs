{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.ExpDecl
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to expression declarations.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.ExpDecl
    ( HasVarReferences
    , mapRefToDecls
    )
where

import           Control.Monad                (foldM, unless)
import           Control.Monad.Error.Class    (catchError, throwError)
import           Data.List.Unique             (repeated)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import           GHC.Exts                     (toList)

import           TorXakis.Compiler.Data       (CompilerM)
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps       ((.@!!))
import           TorXakis.Compiler.MapsTo     (MapsTo, (<.+>))
import           TorXakis.Compiler.Validation
import           TorXakis.Parser.Data         (ActOfferDecl (ActOfferDecl), BExpDecl (Accept, ActPref, Choice, Disable, Enable, Guard, Hide, Interrupt, LetBExp, Pappl, Par, Stop),
                                               ChanOfferDecl (ExclD, QuestD),
                                               CnectDecl, CodecItem (CodecItem),
                                               CodecType (Decode, Encode),
                                               ExpChild (ConstLit, Fappl, If, LetExp, VarRef),
                                               ExpDecl, FuncDecl, FuncDeclE,
                                               HasLoc,
                                               InitStateDecl (InitStateDecl),
                                               IsVariable, LetVarDecl, Loc,
                                               MapperDecl, ModelDecl,
                                               OfferDecl (OfferDecl),
                                               ParLetVarDecl, ProcDecl,
                                               PurpDecl, StUpdate (StUpdate),
                                               StautDecl,
                                               StautItem (InitState, StVarDecl, States, Trans),
                                               TestGoalDecl,
                                               Transition (Transition), VarDecl,
                                               VarDeclE, VarRef, VarRefE,
                                               actOfferDecls, asVarReflLoc,
                                               chanOfferDecls, cnectDeclCodecs,
                                               expChild, funcBody, funcParams,
                                               getLoc, mapperBExp, modelBExp,
                                               offerDecls, procDeclBody,
                                               procDeclParams, purpDeclGoals,
                                               stautDeclComps,
                                               stautDeclInnerVars,
                                               stautDeclParams,
                                               testGoalDeclBExp, toText,
                                               varDeclExp, varName)

-- | Expressions that have variable references.
class HasVarReferences e where
    -- | Map variable references to the entities they refer to.
    --
    -- NOTE: property to check:
    --
    -- the number of 'Loc FuncDeclE' entities in the function declaration
    -- should equal the length of the list returned by this function.
    --
    -- This ensures that the mapping returned is complete.
    --
    -- This class could be replaced by `DefinesAMap` which already includes
    -- such an invariant check.
    mapRefToDecls :: ( MapsTo Text [Loc FuncDeclE] mm
                     , MapsTo Text (Loc VarDeclE) mm )
                  => mm  -- ^ Predefined functions
                  -> e
                  -> CompilerM [(Loc VarRefE, Either (Loc VarDeclE) [Loc FuncDeclE])]

instance HasVarReferences e => HasVarReferences [e] where
    mapRefToDecls mm = fmap concat . traverse (mapRefToDecls mm)

instance HasVarReferences ProcDecl where
    mapRefToDecls mm pd = do
        checkUnique (getErrorLoc pd, Variable, "Process variable parameter")
                    (varName <$> procDeclParams pd)
        pNtoD <- mkVdMap (procDeclParams pd)
        mapRefToDecls (pNtoD <.+> mm) (procDeclBody pd)

-- | Make a map from variable names to the location in which a variable with
-- that name is declared.
--
-- If a variable name in the given list is duplicated, then an error is
-- returned per-each occurrence of the duplicated variable name.
mkVdMap :: (IsVariable v, HasLoc v VarDeclE)
        => [v] -> CompilerM (Map Text (Loc VarDeclE))
mkVdMap vs = do
    let rs = repeated . fmap varName $ vs
        mkErr v =
          Error (MultipleDefinitions Variable)
                (getErrorLoc . getLoc $ v)
                ("Duplicated variable declaration for " <> varName v)
    unless (null rs) $  throwError
                     $  Errors
                     $  mkErr
                    <$> filter ((`elem` rs) . varName) vs
    return $ Map.fromList $ zip (varName <$> vs) (getLoc <$> vs)

instance HasVarReferences BExpDecl where
    mapRefToDecls _ Stop               = return []

    mapRefToDecls mm (ActPref _ ao be) = do
        -- An action offer introduces new variables in the case of actions of
        -- the form 'Ch ? v':
        aoVds <- mkVdMap (actOfferDecls ao)
        (++) <$> mapRefToDecls mm ao <*> mapRefToDecls (aoVds <.+> mm) be

    mapRefToDecls mm (LetBExp vss be) = do
        (mm', letRefs) <- foldM letRefToDecls (mm, []) (toList <$> vss)
        subExRefs      <- mapRefToDecls mm' be
        return $ letRefs ++ subExRefs

    mapRefToDecls mm (Pappl _ _ _ exs)  =
        mapRefToDecls mm exs

    mapRefToDecls mm (Par _ _ be0 be1)  =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1

    mapRefToDecls mm (Enable _ be0 be1) =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1

    mapRefToDecls mm (Accept _ ofrs be) = do
        ovVds <- mkVdMap (concatMap chanOfferDecls ofrs)
        (++) <$> mapRefToDecls mm ofrs
             <*> mapRefToDecls (ovVds <.+> mm) be

    mapRefToDecls mm (Disable _ be0 be1) =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1

    mapRefToDecls mm (Interrupt _ be0 be1) =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1

    mapRefToDecls mm (Choice _ be0 be1) =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1

    mapRefToDecls mm (Guard ex be) =
        (++) <$> mapRefToDecls mm ex <*> mapRefToDecls mm be

    mapRefToDecls mm (Hide _ _ be) =
        mapRefToDecls mm be

instance HasVarReferences ActOfferDecl where
    mapRefToDecls mm ao@(ActOfferDecl os mc) = do
        --  Variables introduced in the action offer (by means of actions of
        --  the form 'Ch ? v') are available at the constraint.
        aoVds <- mkVdMap (actOfferDecls ao)
        (++) <$> mapRefToDecls mm os <*> mapRefToDecls (aoVds <.+> mm) mc

instance HasVarReferences e => HasVarReferences (Maybe e) where
    mapRefToDecls mm = maybe (return []) (mapRefToDecls mm)

instance HasVarReferences ExpDecl where
    mapRefToDecls mm ex = case expChild ex of
        VarRef n rLoc -> do
            dLoc <- fmap Left (mm .@!! (toText n, rLoc))
                    `catchError`
                    const (fmap Right (mm .@!! (toText n, rLoc)))
            return [(rLoc, dLoc)]
        ConstLit _ ->
            return []
        LetExp vss subEx -> do
            (mm', letRefs) <- foldM letRefToDecls (mm, []) (toList <$> vss)
            subExRefs <- mapRefToDecls mm' subEx
            return $ letRefs ++ subExRefs
        If ex0 ex1 ex2 ->
            mapRefToDecls mm [ex0, ex1, ex2]
        Fappl n rLoc exs -> do
            dLocs   <- mm .@!! (toText n, rLoc)
            vrVDExs <- mapRefToDecls mm exs
            return $ (rLoc, Right dLocs) : vrVDExs

instance HasVarReferences ParLetVarDecl where
    mapRefToDecls mm = fmap snd . letRefToDecls (mm, []) . toList

letRefToDecls :: ( MapsTo Text (Loc VarDeclE) mm
                 , MapsTo Text [Loc FuncDeclE] mm )
              => (mm, [(Loc VarRefE, Either (Loc VarDeclE) [Loc FuncDeclE])])
              -> [LetVarDecl]
              -> CompilerM (mm, [(Loc VarRefE, Either (Loc VarDeclE) [Loc FuncDeclE])])
letRefToDecls (mm, xs) vs = do
    letVds <- mkVdMap vs
    -- The expressions of the let variable declarations cannot contain a
    -- variable declared in 'vs', therefore the map 'mm' does not have to be
    -- augmented.
    ys <- mapRefToDecls mm (varDeclExp <$> vs)
    -- A variable declaration in a var expression refers to itself
    let zs = zip (asVarReflLoc . getLoc <$> vs) (Left . getLoc <$> vs)
    return (letVds <.+> mm, xs ++ ys ++ zs)

instance HasVarReferences OfferDecl where
    mapRefToDecls mm (OfferDecl _ os) = mapRefToDecls mm os

instance HasVarReferences ChanOfferDecl where
    mapRefToDecls _ (QuestD vd) =
        -- A variable declared in an input action refers to itself.
        return [(asVarReflLoc . getLoc $ vd, Left . getLoc $ vd)]
    mapRefToDecls mm (ExclD ex)  = mapRefToDecls mm ex

instance HasVarReferences FuncDecl where
    mapRefToDecls mm f = do
        checkUnique (getErrorLoc f, Variable, "Function variable parameter")
                    (varName <$> funcParams f)
        fparamsVDecls <- mkVdMap (funcParams f)
        mapRefToDecls (fparamsVDecls <.+> mm) (funcBody f)

instance HasVarReferences ModelDecl where
    mapRefToDecls mm = mapRefToDecls mm . modelBExp

instance HasVarReferences MapperDecl where
    mapRefToDecls mm = mapRefToDecls mm . mapperBExp

instance HasVarReferences StautDecl where
    mapRefToDecls mm staut = do
        paramVarDecls <- mkVdMap (stautDeclParams staut)
        innerVarDecls <- mkVdMap (stautDeclInnerVars staut)
        -- We give the inner variables precedence over the state automaton parameters.
        let stautVarDecls = innerVarDecls <> paramVarDecls
        mapRefToDecls (stautVarDecls <.+> mm) (stautDeclComps staut)

instance HasVarReferences StautItem where
    mapRefToDecls _  (States _)                        = return []
    mapRefToDecls _  (StVarDecl _)                     = return []
    mapRefToDecls mm (InitState (InitStateDecl _ uds)) = mapRefToDecls mm uds
    mapRefToDecls mm (Trans ts)                        = mapRefToDecls mm ts

instance HasVarReferences StUpdate where
    mapRefToDecls mm (StUpdate vs e) =
        (++) <$> mapRefToDecls mm vs <*> mapRefToDecls mm e

instance HasVarReferences VarRef where
    mapRefToDecls mm vr = do
        let rLoc = getLoc vr
            n = varName vr
        dLoc <- fmap Left (mm .@!! (n, rLoc))
                `catchError`
                const (fmap Right (mm .@!! (n, rLoc)))
        return [(rLoc, dLoc)]

instance HasVarReferences Transition where
    mapRefToDecls mm (Transition _ offr uds _) = do
        aoVds <- mkVdMap (actOfferDecls offr)
        (++) <$> mapRefToDecls mm offr <*> mapRefToDecls (aoVds <.+> mm) uds

instance HasVarReferences PurpDecl where
    mapRefToDecls mm = mapRefToDecls mm . purpDeclGoals

instance HasVarReferences TestGoalDecl where
    mapRefToDecls mm = mapRefToDecls mm . testGoalDeclBExp

instance HasVarReferences CnectDecl where
    mapRefToDecls mm cd = mapRefToDecls mm (cnectDeclCodecs cd)

instance HasVarReferences CodecItem where
    mapRefToDecls mm (CodecItem offr chOffr Decode) = do
        chOffrVd <- mkVdMap (chanOfferDecls chOffr)
        (++) <$> mapRefToDecls (chOffrVd <.+> mm) offr <*> mapRefToDecls mm chOffr

    mapRefToDecls mm (CodecItem offr chOffr Encode) = do
        offrVd <- mkVdMap (offerDecls offr)
        (++) <$> mapRefToDecls mm offr <*> mapRefToDecls (offrVd <.+> mm) chOffr

instance HasVarReferences VarDecl where
    mapRefToDecls _ vd = return [(asVarReflLoc . getLoc $ vd, Left . getLoc $ vd)]
