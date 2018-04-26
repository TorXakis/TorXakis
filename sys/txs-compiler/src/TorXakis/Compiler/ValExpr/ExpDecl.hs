{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TorXakis.Compiler.ValExpr.ExpDecl where

import           Control.Monad.Error.Class (liftEither)
import           Control.Arrow             ((+++), second)
import           Control.Monad.Error.Class (catchError)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Text                 (Text)
import Data.Semigroup ((<>))
import           Data.Either               (partitionEithers)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Error
import           TorXakis.Parser.Data


class HasVarReferences e where
    -- | Map variable references to the entities they refer to.
    --
    -- TODO: property to check:
    --
    -- the number of 'Loc FuncDeclE' entities in the function declaration
    -- should equal the length of the list returned by this function.
    --
    -- This ensures that the mapping returned is complete.
    mapRefToDecls :: ( MapsTo Text [FuncDefInfo] mm
                     , MapsTo Text (Loc VarDeclE) mm )
                  => mm  -- ^ Predefined functions
                  -> e
                  -> CompilerM [(Loc VarRefE, Loc VarDeclE :| [FuncDefInfo])]

instance HasVarReferences e => HasVarReferences [e] where
    mapRefToDecls mm = fmap concat . traverse (mapRefToDecls mm)

instance HasVarReferences ProcDecl where
    mapRefToDecls mm pd = mapRefToDecls (pNtoD <.+> mm) (procDeclBody pd)
        where
          pNtoD = mkVdMap (procDeclParams pd)

-- | Make a map from variable names to variable the location in which a
-- variable with that name is declared.
mkVdMap :: (IsVariable v, HasLoc v VarDeclE)
        => [v] -> Map Text (Loc VarDeclE)
mkVdMap vs =
    Map.fromList $ zip (varName <$> vs) (getLoc <$> vs)

instance HasVarReferences BExpDecl where
    mapRefToDecls _ Stop                = return []
    mapRefToDecls mm (ActPref ao be)    =
        (++) <$> mapRefToDecls mm ao <*> mapRefToDecls (aoVds <.+> mm) be
        where
          -- An action offer introduces new variables in the case of actions of
          -- the form 'Ch ? v':
          aoVds = mkVdMap (actOfferDecls ao)
    mapRefToDecls mm (LetBExp vs be)    =
        let letVds = mkVdMap vs in
            (++) <$> mapRefToDecls mm (varDeclExp <$> vs)
                 <*> mapRefToDecls (letVds <.+> mm) be
    mapRefToDecls mm (Pappl _ _ _ exs)  =
        mapRefToDecls mm exs
    mapRefToDecls mm (Par _ _ be0 be1)  =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1
    mapRefToDecls mm (Enable _ be0 be1) =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1
    mapRefToDecls mm (Accept _ ofrs be) =
        (++) <$> mapRefToDecls mm ofrs
             <*> mapRefToDecls (ovVds <.+> mm) be
        where
          ovVds = mkVdMap (concatMap chanOfferDecls ofrs)
    mapRefToDecls mm (Disable _ be0 be1) =
        (++) <$> mapRefToDecls mm be0 <*> mapRefToDecls mm be1          

instance HasVarReferences ActOfferDecl where
    mapRefToDecls mm ao@(ActOfferDecl os mc) =
        (++) <$> mapRefToDecls mm os <*> mapRefToDecls (aoVds <.+> mm) mc
        where
          --  Variables introduced in the action offer (by means of actions of
          --  the form 'Ch ? v') are available at the constraint.
          aoVds = mkVdMap (actOfferDecls ao)

instance HasVarReferences e => HasVarReferences (Maybe e) where
    mapRefToDecls mm = maybe (return []) (mapRefToDecls mm)

instance HasVarReferences ExpDecl where
    -- TODO: replace varDeclsFromExpDecl by this function
    mapRefToDecls mm ex = case expChild ex of
        VarRef n rLoc -> do
            dLoc <- fmap Left (mm .@!! (toText n, rLoc))
                    `catchError`
                    const (fmap Right (mm .@!! (toText n, rLoc)))
            return [(rLoc, dLoc)]
        ConstLit _ ->
            return []
        LetExp vs subEx ->
            let letVds = mkVdMap vs in
                (++) <$> mapRefToDecls mm (varDeclExp <$> vs)
                     <*> mapRefToDecls (letVds <.+> mm) subEx
        If ex0 ex1 ex2 ->
            mapRefToDecls mm [ex0, ex1, ex2]
        Fappl n rLoc exs -> do
            dLocs   <- mm .@!! (toText n, rLoc)
            vrVDExs <- mapRefToDecls mm exs
            return $ (rLoc, Right dLocs) : vrVDExs        

instance HasVarReferences OfferDecl where
    mapRefToDecls mm (OfferDecl _ os) = mapRefToDecls mm os

instance HasVarReferences ChanOfferDecl where
    mapRefToDecls _ (QuestD vd) =
        -- A variable declared in an input action refers to itself.
        return [(asVarReflLoc . getLoc $ vd, Left . getLoc $ vd)]
    mapRefToDecls mm (ExclD ex)  = mapRefToDecls mm ex
    
instance HasVarReferences FuncDecl where
    mapRefToDecls mm f = mapRefToDecls (mkVdMap (funcParams f) <.+> mm) (funcBody f)

instance HasVarReferences ModelDecl where
    mapRefToDecls mm = mapRefToDecls mm . modelBExp
