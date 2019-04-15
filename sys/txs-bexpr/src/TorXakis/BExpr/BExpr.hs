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
  -- * ActOffer: the atomic communication step
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

import           TorXakis.Chan
import           TorXakis.ChanContext
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.ProcSignature
import           TorXakis.Relabel
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.VarContext
import           TorXakis.Var

-- | BExpressionView: the public view of Behaviour Expression `BExpression`
data BExpressionView = ActionPref  VarsDecl ActOffer BExpression
                                    -- Hidden Variables are declared but not used in offers of ActOffer (maybe used in constraint of ActOffer).
                         
                     | Guard       ValExpression BExpression
                     | Choice      (Set.Set BExpression)
                     | Parallel    (Set.Set ChanRef) [BExpression] -- actually (MultiSet.MultiSet BExpr) but that has lousy performance (due to sorting which needs more evaluation?)
                     | Enable      BExpression VarsDecl BExpression
                     | Disable     BExpression BExpression
                     | Interrupt   BExpression BExpression
                     | ProcInst    ProcSignature [ChanRef] [ValExpression]
                     | Hide        (Map.Map ChanRef ChanDef) BExpression
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

-- | Goal for TestPurpose
data Goal =  Hit
           | Miss
           deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | ActOffer
-- Offer on multiple channels with constraints
data ActOffer = QuiescenceStep  -- for test purposes
                                -- TODO: Should we explicitly extend models with their Quiescence Step, or should it `just` be a function of a state?
                                --       in that case we might need to add a constraint to QuiescenceStep as well...
              | ActOffer { -- | Offers over channels
                           offers     :: Map.Map ChanRef [ValExpression]
                           -- | constraint of ActOffer
                         , constraint :: ValExpression
                           -- | Executing this ActOffer might impact a test goal.   ( | HIT or | MISS )
                         , mGoal      :: Maybe Goal
                         } 
              deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Contains EXIT is equal to the presence of EXIT in the ActOffer.
containsEXIT :: ActOffer -> Bool
containsEXIT = Map.member ChanRefExit . offers


instance VarContext a => HasProcExit a ActOffer where
    getProcExit _   QuiescenceStep  = TorXakis.ProcSignature.Hit
    getProcExit ctx a@ActOffer{}    = case Map.lookup ChanRefExit (offers a) of
                                           Nothing -> case mGoal a of
                                                           Nothing -> NoExit
                                                           Just _  -> TorXakis.ProcSignature.Hit
                                           Just vs -> Exit (map (getSort ctx) vs)

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

instance (VarContext c, ChanContext c) => UsedSorts c BExpression where
    usedSorts ctx = usedSorts ctx . TorXakis.BExpr.BExpr.view

instance (VarContext c, ChanContext c) => UsedSorts c BExpressionView where
    usedSorts ctx (ActionPref vs _ b) = Set.unions $ [ usedSorts ctx vs
                                                      --, usedSorts ctx ao  -- TODO: usedSorts for ActOffer
                                                      , usedSorts ctx b
                                                      ]
    usedSorts ctx (Guard v b)          = Set.unions $ [ usedSorts ctx v
                                                      , usedSorts ctx b
                                                      ]
    usedSorts ctx (Choice s)           = Set.unions $ map (usedSorts ctx) (Set.toList s)
    usedSorts ctx (Parallel _ bs)      = Set.unions $ map (usedSorts ctx) bs -- TODO: lookup Channels and add sort
    usedSorts ctx (Enable a vs b)      = Set.unions $ (usedSorts ctx vs : map (usedSorts ctx) [a,b])
    usedSorts ctx (Disable a b)        = Set.unions $ map (usedSorts ctx) [a,b]
    usedSorts ctx (Interrupt a b)      = Set.unions $ map (usedSorts ctx) [a,b]
    usedSorts ctx (ProcInst p _ vs)    = Set.unions (usedSorts ctx p : map (usedSorts ctx) vs) -- Under the assumption of a correct ProcInst and return type is a set of Sorts:
                                                                                            -- Sorts of Channels are already in ProcSignature, so no need to lookup Channels and add sort
    usedSorts ctx (Hide m b)           = Set.unions (usedSorts ctx b : map (usedSorts ctx . chanSort) (Map.elems m))    -- TODO: once ChanDef is final: are all cases covered
    
instance UsedFuncSignatures BExpression where
    usedFuncSignatures = usedFuncSignatures . TorXakis.BExpr.BExpr.view

instance UsedFuncSignatures BExpressionView where
    usedFuncSignatures = undefined -- TODO
    
instance UsedProcSignatures BExpression where
    usedProcSignatures = usedProcSignatures . TorXakis.BExpr.BExpr.view

instance UsedProcSignatures BExpressionView where
    usedProcSignatures (ActionPref _ _ b) = usedProcSignatures b
    usedProcSignatures (Choice s)         = Set.unions $ map usedProcSignatures (Set.toList s)
    usedProcSignatures (Guard _ b)        = usedProcSignatures b
    usedProcSignatures (Parallel _ l)     = Set.unions $ map usedProcSignatures l
    usedProcSignatures (Enable a _ b)     = Set.unions $ map usedProcSignatures [a, b]
    usedProcSignatures (Disable a b)      = Set.unions $ map usedProcSignatures [a, b]
    usedProcSignatures (Interrupt a b)    = Set.unions $ map usedProcSignatures [a, b]
    usedProcSignatures (ProcInst p _ _)   = Set.singleton p
    usedProcSignatures (Hide _ b)         = usedProcSignatures b

instance FreeVars BExpression where
    freeVars = freeVars . TorXakis.BExpr.BExpr.view

instance FreeVars BExpressionView where
    freeVars (ActionPref vs a b)    = Set.difference (Set.unions [freeVars a, freeVars b]) (Set.fromList (map (RefByName . name) (TorXakis.Var.toList vs)))
    freeVars (Guard v b)            = Set.unions [freeVars v, freeVars b]
    freeVars (Choice s)             = Set.unions $ map freeVars (Set.toList s)
    freeVars (Parallel _ bs)        = Set.unions $ map freeVars bs
    freeVars (Enable b1 vs b2)      = Set.unions [freeVars b1, Set.difference (freeVars b2) (Set.fromList (map (RefByName . name) (TorXakis.Var.toList vs)))]
    freeVars (Disable b1 b2)        = Set.unions $ map freeVars [b1, b2]
    freeVars (Interrupt b1 b2)      = Set.unions $ map freeVars [b1, b2]
    freeVars (ProcInst _ _ vs)      = Set.unions $ map freeVars vs
    freeVars (Hide _ b)             = freeVars b

instance FreeVars ActOffer where
    freeVars ao = Set.unions (freeVars (constraint ao): map freeVars (concat (Map.elems (offers ao))))

instance FreeChans BExpression where
    freeChans = freeChans . TorXakis.BExpr.BExpr.view

instance FreeChans BExpressionView where
    freeChans (ActionPref _ a b) = Set.unions [freeChans a, freeChans b]
    freeChans (Guard _ b)        = freeChans b
    freeChans (Choice s)         = Set.unions $ map freeChans (Set.toList s)
    freeChans (Parallel cs bs)   = Set.unions (Set.map chanRef cs : map freeChans bs)
    freeChans (Enable b1 _ b2)   = Set.unions $ map freeChans [b1, b2]
    freeChans (Disable b1 b2)    = Set.unions $ map freeChans [b1, b2]
    freeChans (Interrupt b1 b2)  = Set.unions $ map freeChans [b1, b2]
    freeChans (ProcInst _ cs _)  = Set.fromList $ map chanRef cs     -- use: only user defined channels can be used in proc instantiation
    freeChans (Hide m b)         = Set.difference (freeChans b) $ Set.fromList (map chanRef (Map.keys m))

instance FreeChans ActOffer where
    freeChans = Set.unions . map freeChans . Map.keys . offers

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
    relabel' m (Hide c b)            = let m' = hide (Set.fromList (map (toName . chanRef) (Map.keys c))) m 
                                           c' = Map.fromList (map (first (relabel m')) (Map.toList c))
                                       in
                                            Hide c' (relabel m' b)

instance Relabel ActOffer where
    relabel' _ QuiescenceStep          = QuiescenceStep
    relabel' m (ActOffer offs cnrs mg) = let newOffs = Map.fromList (map (first (relabel' m)) (Map.toList offs))
                                            in ActOffer newOffs cnrs mg
