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
, ActOffer(..)
, containsEXIT
, declareVars
  -- Channel Offer
, ChanOffer(..)
, declareVar
)
where

import           Control.DeepSeq
import           Control.Monad
import           Data.Data
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)

import           TorXakis.BExpr.ExitKind
import           TorXakis.ChanDef
import           TorXakis.FreeVars
import           TorXakis.ProcSignature
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.VarDef

-- | BExpressionView: the public view of Behaviour Expression `BExpression`
data BExpressionView v = ActionPref  (ActOffer v) (BExpression v)
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
data ActOffer v = ActOffer  { offers     :: Map.Map ChanDef [ChanOffer v]
                            , hiddenvars :: Set.Set v
                            , constraint :: ValExpression v
                            } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Contains EXIT is equal to the presence of EXIT in the ActOffer.
containsEXIT :: ActOffer v -> Bool
containsEXIT ao = chanExit `Map.member` offers ao

-- | The variables declared in an `ActOffer`
declareVars :: ActOffer v -> [v]
declareVars = mapMaybe declareVar . concat . Map.elems . offers

-- | Channel Offer
-- Offer of a single value
data  ChanOffer v   = Quest  v
                    | Exclam (ValExpression v)
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance VarDef v => HasSort (ChanOffer v) where
    getSort (Quest  var)    = getSort var
    getSort (Exclam vexpr)  = getSort vexpr

-- | The variable possibly declared in a `ChanOffer`
declareVar :: ChanOffer v -> Maybe v
declareVar (Quest v)    = Just v
declareVar (Exclam _)   = Nothing

-- | ExitKind related to ChanDef ChanOffers tuple
toExitKind :: VarDef v => (ChanDef , [ChanOffer v]) -> ExitKind
toExitKind (cd, cs) | cd == chanExit     = Exit (map getSort cs)
toExitKind (cd, _)  | cd == chanIstep    = NoExit
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
    getExitKind (ActionPref a b)    = case getExitKind a <<+>> getExitKind b of
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
    freeVars = undefined -- TODO implement