{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.BExpr.BExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module introduces definitions related to behaviour expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.BExpr.BExpr
( -- * Behaviour Expression type and view
  BExpression (..)
, BExpressionView (..)
, BExpr
, BExprView
  -- Action Offer
, ActOffer (..)
, containsEXIT
)
where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Data.Data
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)

import           TorXakis.BExpr.ExitKind
import           TorXakis.ChanDef
import           TorXakis.FreeVars
import           TorXakis.ProcSignature
import           TorXakis.Relabel
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.VarDef

-- | BExpressionView: the public view of Behaviour Expression `BExpression`
data BExpressionView v = ActionPref  (Set.Set v) (ActOffer v) (BExpression v)
                       | Guard       (ValExpression v) (BExpression v)
                       | Choice      (Set.Set (BExpression v))
                       | Parallel    (Set.Set ChanDef) [BExpression v] -- actually (MultiSet.MultiSet BExpr) but that has lousy performance (due to sorting which needs more evaluation?)
                       | Enable      (BExpression v) [v] (BExpression v)
                       | Disable     (BExpression v) (BExpression v)
                       | Interrupt   (BExpression v) (BExpression v)
                       | ProcInst    ProcSignature [ChanDef] [ValExpression v]
                       | Hide        (Set.Set ChanDef) (BExpression v)
  deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | BExpression: behaviour expression
--
-- 1. User can't directly construct BExpression (such that invariants will always hold)
--
-- 2. User can still pattern match on BExpression using 'BExpressionView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype BExpression v = BExpression {
            -- | View on Behaviour Expression
            view :: BExpressionView v
        }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | type synonym BExpr
type BExpr = BExpression MinimalVarDef

-- | type synonym BExprView
type BExprView = BExpressionView MinimalVarDef

-- | ActOffer
-- Offer on multiple channels with constraints
data ActOffer v = ActOffer { -- | Offers over channels
                             offers     :: Map.Map ChanDef [ValExpression v]
                             -- | constraint of ActOffer
                           , constraint :: ValExpression v
                           } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Contains EXIT is equal to the presence of EXIT in the ActOffer.
containsEXIT :: ActOffer v -> Bool
containsEXIT ao = chanExit `Map.member` offers ao

-- | ExitKind related to ChanDef ValExpression tuple
toExitKind :: VarDef v => (ChanDef , [ValExpression v]) -> ExitKind
toExitKind (cd, cs) | cd == chanExit     = Exit (map getSort cs)
toExitKind (cd, _)  | cd == chanQstep    = Hit
toExitKind (cd, _)  | cd == chanHit      = Hit
toExitKind (cd, _)  | cd == chanMiss     = Hit
toExitKind _                             = NoExit

instance VarDef v => HasExitKind (ActOffer v) where
    getExitKind a = case foldM (<<+>>) NoExit (map toExitKind (Map.toList (offers a))) of
                        Right v -> v
                        Left e  -> error ("Smart constructor created invalid ActOffer " ++ show e)


instance VarDef v => HasExitKind (BExpression v) where
    getExitKind = getExitKind . TorXakis.BExpr.BExpr.view

instance VarDef v => HasExitKind (BExpressionView v) where
    getExitKind (ActionPref _ a b)  = case getExitKind a <<+>> getExitKind b of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid ActionPref " ++ show e)
    getExitKind (Guard _ b)         = getExitKind b
    getExitKind (Choice s)          = case foldM (<<+>>) NoExit (map getExitKind (Set.toList s)) of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid Choice " ++ show e)
    getExitKind (Parallel _ l)      = case foldM (<<->>) NoExit (map getExitKind l) of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid Parallel " ++ show e)
    getExitKind (Enable _ _ b)      = getExitKind b
    getExitKind (Disable a b)       = case getExitKind a <<+>> getExitKind b of
                                            Right v -> v
                                            Left e  -> error ("Smart constructor created invalid Disable " ++ show e)
    getExitKind (Interrupt a _)     = getExitKind a
    getExitKind (ProcInst ps _ _)   = exitKind ps
    getExitKind (Hide _ b)          = getExitKind b


instance VarDef v => FreeVars BExpression v where
    freeVars = freeVars . TorXakis.BExpr.BExpr.view

instance VarDef v => FreeVars BExpressionView v where
    freeVars (ActionPref vs a b)    = Set.difference (Set.unions [freeVars a, freeVars b]) vs
    freeVars (Guard v b)            = Set.unions [freeVars v, freeVars b]
    freeVars (Choice s)             = Set.unions $ map freeVars (Set.toList s)
    freeVars (Parallel _ bs)        = Set.unions $ map freeVars bs
    freeVars (Enable b1 vs b2)      = Set.unions [freeVars b1, Set.difference (freeVars b2) (Set.fromList vs)]
    freeVars (Disable b1 b2)        = Set.unions $ map freeVars [b1, b2]
    freeVars (Interrupt b1 b2)      = Set.unions $ map freeVars [b1, b2]
    freeVars (ProcInst _ _ vs)      = Set.unions $ map freeVars vs
    freeVars (Hide _ b)             = freeVars b

instance VarDef v => FreeVars ActOffer v where
    freeVars (ActOffer m c) = Set.unions (freeVars c: map freeVars (concat (Map.elems m)))

instance Ord v => Relabel (BExpression v) where
    relabel' m b = BExpression $ relabel' m (TorXakis.BExpr.BExpr.view b)

instance Ord v => Relabel (BExpressionView v) where
    relabel' m (ActionPref vs a b)   = ActionPref vs (relabel' m a) (relabel' m b)
    relabel' m (Guard v b)           = Guard v (relabel' m b)
    relabel' m (Choice s)            = Choice (Set.map (relabel' m) s)
    relabel' m (Parallel cs bs)      = Parallel (Set.map (relabel' m) cs) (map (relabel' m) bs)
    relabel' m (Enable b1 vs b2)     = Enable (relabel' m b1) vs (relabel' m b2)
    relabel' m (Disable b1 b2)       = Disable (relabel' m b1) (relabel' m b2)
    relabel' m (Interrupt b1 b2)     = Interrupt (relabel' m b1) (relabel' m b2)
    relabel' m (ProcInst p cs vs)    = ProcInst p (map (relabel' m) cs) vs
                                       -- TODO: What to remove only keys or also values? 
                                       --       What is Relabel (A->B and B-A) on HIDE [A] IN A>-> B NI anyway?
    relabel' m (Hide cs b)           = case mkRelabelMap (Map.filterWithKey undefined (toMap m)) of
                                            Left e -> error ("Relabel: removing elements from a valid RelabelMap should result in a valid RelabelMap, hence unexpected error:\n" ++ show e)
                                            Right n -> Hide cs (relabel n b)
                                        
instance Relabel (ActOffer v) where
    relabel' m (ActOffer offs cnrs) = let newOffs = Map.fromList (map (first (relabel' m)) (Map.toList offs)) in
                                                 ActOffer newOffs cnrs

