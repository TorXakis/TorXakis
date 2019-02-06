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

import           TorXakis.ChanDef
import           TorXakis.FreeVars
import           TorXakis.Name
import           TorXakis.ProcExit
import           TorXakis.ProcSignature
import           TorXakis.Relabel
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.VarContext
import           TorXakis.VarDef
import           TorXakis.VarsDecl

-- | BExpressionView: the public view of Behaviour Expression `BExpression`
data BExpressionView = ActionPref  VarsDecl ActOffer BExpression
                     | Guard       ValExpression BExpression
                     | Choice      (Set.Set BExpression)
                     | Parallel    (Set.Set ChanDef) [BExpression] -- actually (MultiSet.MultiSet BExpr) but that has lousy performance (due to sorting which needs more evaluation?)
                     | Enable      BExpression VarsDecl BExpression
                     | Disable     BExpression BExpression
                     | Interrupt   BExpression BExpression
                     | ProcInst    ProcSignature [ChanDef] [ValExpression]
                     | Hide        (Set.Set ChanDef) BExpression
  deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | BExpression: behaviour expression
--
-- 1. User can't directly construct BExpression (such that invariants will always hold)
--
-- 2. User can still pattern match on BExpression using 'BExpressionView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype BExpression = BExpression {
            -- | View on Behaviour Expression
            view :: BExpressionView
        }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | ActOffer
-- Offer on multiple channels with constraints
data ActOffer = ActOffer { -- | Offers over channels
                           offers     :: Map.Map ChanDef [ValExpression]
                           -- | constraint of ActOffer
                         , constraint :: ValExpression
                         } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Contains EXIT is equal to the presence of EXIT in the ActOffer.
containsEXIT :: ActOffer -> Bool
containsEXIT ao = chanExit `Map.member` offers ao

-- | ExitKind related to ChanDef ValExpression tuple
toProcExit :: VarContext a => a -> (ChanDef , [ValExpression]) -> ProcExit
toProcExit ctx (cd, cs) | cd == chanExit     = Exit (map (getSort ctx) cs)
toProcExit _   (cd, _)  | cd == chanQstep    = Hit
toProcExit _   (cd, _)  | cd == chanHit      = Hit
toProcExit _   (cd, _)  | cd == chanMiss     = Hit
toProcExit _   _                             = NoExit

instance VarContext a => HasProcExit a ActOffer where
    getProcExit ctx a = case foldM (<<+>>) NoExit (map (toProcExit ctx) (Map.toList (offers a))) of
                             Right v -> v
                             Left e  -> error ("Smart constructor created invalid ActOffer " ++ show e)


instance VarContext a => HasProcExit a BExpression where
    getProcExit ctx = getProcExit ctx . TorXakis.BExpr.BExpr.view

instance VarContext a => HasProcExit a BExpressionView where
    getProcExit ctx (ActionPref _ a b)  = case getProcExit ctx a <<+>> getProcExit ctx b of
                                               Right v -> v
                                               Left e  -> error ("Smart constructor created invalid ActionPref " ++ show e)
    getProcExit ctx (Guard _ b)         = getProcExit ctx b
    getProcExit ctx (Choice s)          = case foldM (<<+>>) NoExit (map (getProcExit ctx) (Set.toList s)) of
                                               Right v -> v
                                               Left e  -> error ("Smart constructor created invalid Choice " ++ show e)
    getProcExit ctx (Parallel _ l)      = case foldM (<<->>) NoExit (map (getProcExit ctx) l) of
                                               Right v -> v
                                               Left e  -> error ("Smart constructor created invalid Parallel " ++ show e)
    getProcExit ctx (Enable _ _ b)      = getProcExit ctx b
    getProcExit ctx (Disable a b)       = case getProcExit ctx a <<+>> getProcExit ctx b of
                                               Right v -> v
                                               Left e  -> error ("Smart constructor created invalid Disable " ++ show e)
    getProcExit ctx (Interrupt a _)     = getProcExit ctx a
    getProcExit _   (ProcInst ps _ _)   = exit ps
    getProcExit ctx (Hide _ b)          = getProcExit ctx b


instance FreeVars BExpression where
    freeVars = freeVars . TorXakis.BExpr.BExpr.view

instance FreeVars BExpressionView where
    freeVars (ActionPref vs a b)    = Set.difference (Set.unions [freeVars a, freeVars b]) (Set.fromList (map toRefByName (toList vs)))
    freeVars (Guard v b)            = Set.unions [freeVars v, freeVars b]
    freeVars (Choice s)             = Set.unions $ map freeVars (Set.toList s)
    freeVars (Parallel _ bs)        = Set.unions $ map freeVars bs
    freeVars (Enable b1 vs b2)      = Set.unions [freeVars b1, Set.difference (freeVars b2) (Set.fromList (map toRefByName (toList vs)))]
    freeVars (Disable b1 b2)        = Set.unions $ map freeVars [b1, b2]
    freeVars (Interrupt b1 b2)      = Set.unions $ map freeVars [b1, b2]
    freeVars (ProcInst _ _ vs)      = Set.unions $ map freeVars vs
    freeVars (Hide _ b)             = freeVars b

instance FreeVars ActOffer where
    freeVars (ActOffer m c) = Set.unions (freeVars c: map freeVars (concat (Map.elems m)))

instance Relabel BExpression where
    relabel' m b = BExpression $ relabel' m (TorXakis.BExpr.BExpr.view b)

instance Relabel BExpressionView where
    relabel' m (ActionPref vs a b)   = ActionPref vs (relabel' m a) (relabel' m b)
    relabel' m (Guard v b)           = Guard v (relabel' m b)
    relabel' m (Choice s)            = Choice (Set.map (relabel' m) s)
    relabel' m (Parallel cs bs)      = Parallel (Set.map (relabel' m) cs) (map (relabel' m) bs)
    relabel' m (Enable b1 vs b2)     = Enable (relabel' m b1) vs (relabel' m b2)
    relabel' m (Disable b1 b2)       = Disable (relabel' m b1) (relabel' m b2)
    relabel' m (Interrupt b1 b2)     = Interrupt (relabel' m b1) (relabel' m b2)
    relabel' m (ProcInst p cs vs)    = ProcInst p (map (relabel' m) cs) vs
                                       -- TODO: What to remove only keys or also values? 
                                       --       What is Relabel (A->B and B->A) on HIDE [A] IN A>-> B NI anyway?
    relabel' m (Hide cs b)           = case mkRelabelMap (Map.filterWithKey undefined (toMap m)) of
                                            Left e -> error ("Relabel: removing elements from a valid RelabelMap should result in a valid RelabelMap, hence unexpected error:\n" ++ show e)
                                            Right n -> Hide cs (relabel n b)
                                        
instance Relabel ActOffer where
    relabel' m (ActOffer offs cnrs) = let newOffs = Map.fromList (map (first (relabel' m)) (Map.toList offs)) in
                                                 ActOffer newOffs cnrs

