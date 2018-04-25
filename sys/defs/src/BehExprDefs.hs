{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BehExprDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module introduces definitions related to behaviour expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ViewPatterns       #-}
module BehExprDefs
( 
  -- * Behaviour Expression type and view
  BExprView(..)
, BExpr
, BehExprDefs.view

, ActOffer(..)
, Offer(..)
, ChanOffer(..)
, Trans(..)
, (~~)
  -- * Smart Constructors and checks for Behaviour Expressions
, stop
, isStop
, actionPref
, guard
, choice
, parallel
, enable
, disable
, interrupt
, procInst
, hide
, valueEnv
, stAut
  -- * Debugging
, valid
)
where

import qualified Data.Set        as Set

import           Control.DeepSeq
import           Data.Data
import           GHC.Generics    (Generic)

import           ChanId
import           ConstDefs
import           Id
import           ProcId
import           SortOf
import           StatId
import           ValExpr
import           VarEnv
import           VarId


-- | BExprView: the public view of Behaviour Expression `BExpr`
data BExprView = ActionPref  ActOffer BExpr
               | Guard       VExpr BExpr
               | Choice      [BExpr]                -- Distinct Ascending List, does not contain a Choice as element of this list
               | Parallel    [ChanId] [BExpr]
               | Enable      BExpr [ChanOffer] BExpr
               | Disable     BExpr BExpr
               | Interrupt   BExpr BExpr
               | ProcInst    ProcId [ChanId] [VExpr]
               | Hide        [ChanId] BExpr
               | ValueEnv    VEnv BExpr
               | StAut       StatId VEnv [Trans]
  deriving (Eq,Ord,Read,Show, Generic, NFData, Data)
instance Resettable BExprView
    where 
--        reset (Choice bs)       = Choice (Set.toAscList (Set.fromList (reset bs))) -- reorder to ensure invariant
--        reset x                 = over uniplate reset x
        reset (ActionPref a b)  = ActionPref (reset a) (reset b)
        reset (Guard v b)       = Guard (reset v) (reset b)
        reset (Choice bs)       = Choice (Set.toAscList (Set.fromList (reset bs))) -- reorder to ensure invariant
        reset (Parallel c b)    = Parallel (reset c) (reset b)
        reset (Enable b1 c b2)  = Enable (reset b1) (reset c) (reset b2)
        reset (Disable b1 b2)   = Disable (reset b1) (reset b2)
        reset (Interrupt b1 b2) = Interrupt (reset b1) (reset b2)
        reset (ProcInst p c v)  = ProcInst (reset p) (reset c) (reset v)
        reset (Hide c b)        = Hide (reset c) (reset b)
        reset (ValueEnv v b)    = ValueEnv (reset v) (reset b)
        reset (StAut s v t)     = StAut (reset s) (reset v) (reset t)

-- | BExpr: behaviour expression
--
-- 1. User can't directly construct BExpr (such that invariants will always hold)
--
-- 2. User can still pattern match on BExpr using 'BExprView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype BExpr = BExpr {
            -- | View on Behaviour Expression
            view :: BExprView
        }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)
instance Resettable BExpr

-- | Is behaviour expression equal to Stop behaviour?
isStop :: BExpr -> Bool
isStop (BehExprDefs.view -> Choice []) = True
isStop _                               = False

-- | Create a Stop behaviour expression.
--   The Stop behaviour is equal to dead lock.
stop :: BExpr
stop = BExpr (Choice [])

-- | Create an ActionPrefix behaviour expression.
actionPref :: ActOffer -> BExpr -> BExpr
actionPref a b = case ValExpr.view (constraint a) of
                    Vconst (Cbool False)    -> stop
                    _                       -> BExpr (ActionPref a b)

-- | Create a guard behaviour expression.
guard :: VExpr -> BExpr -> BExpr
guard (ValExpr.view -> Vconst (Cbool False)) _              = stop
guard (ValExpr.view -> Vconst (Cbool True))  b              = b
guard _ b                                     | isStop b    = stop
guard v (BehExprDefs.view -> ActionPref (ActOffer o h c) b) = actionPref (ActOffer o h (cstrAnd (Set.fromList [v,c]))) b
guard v b                                                   = BExpr (Guard v b)

-- | Create a choice behaviour expression.
--  A choice combines zero or more behaviour expressions.
choice :: [BExpr] -> BExpr
choice l = let s = flattenChoice l
               l' = Set.toAscList s       -- All elements in a set are distinct
             in 
                case l' of
                    []  -> stop
                    [a] -> a
                    _   -> BExpr (Choice l')
    where 
        -- 1. nesting of choices are flatten
        --    (p ## q) ## r == p ## q ## r 
        --    see https://wiki.haskell.org/Smart_constructors#Runtime_Optimisation_:_smart_constructors for inspiration for this implementation
        -- 2. elements in a set are distinctive
        --    hence p ## p == p
        flattenChoice :: [BExpr] -> Set.Set BExpr
        flattenChoice l' = Set.unions $ map fromBExpr l'
        
        fromBExpr :: BExpr -> Set.Set BExpr
        fromBExpr (BehExprDefs.view -> Choice l') = Set.fromDistinctAscList l'
        fromBExpr x                               = Set.singleton x

-- | Create a parallel behaviour expression.
-- The behaviour expressions must synchronize on the given set of channels (and EXIT).
parallel :: [ChanId] -> [BExpr] -> BExpr
parallel cs bs = let fbs = flattenParallel bs
                    in BExpr (Parallel cs fbs)
    where
        -- nesting of parallels over the same channel sets are flatten
        --     (p |[ G ]| q) |[ G ]| r == p |[ G ]| q |[ G ]| r
        --    see https://wiki.haskell.org/Smart_constructors#Runtime_Optimisation_:_smart_constructors for inspiration for this implementation
        flattenParallel :: [BExpr] -> [BExpr]
        flattenParallel = concatMap fromBExpr
        
        fromBExpr :: BExpr -> [BExpr]
        fromBExpr (BehExprDefs.view -> Parallel pcs pbs) | Set.fromList cs == Set.fromList pcs  = pbs
        fromBExpr bexpr                                                                         = [bexpr]

-- | Create an enable behaviour expression.
enable :: BExpr -> [ChanOffer] -> BExpr -> BExpr
enable b _ _    | isStop b = stop
enable b1 cs b2            = BExpr (Enable b1 cs b2)

-- | Create a disable behaviour expression.
disable :: BExpr -> BExpr -> BExpr
disable b1 b2 | isStop b1 = b2
disable b1 b2 | isStop b2 = b1
disable b1 b2             = BExpr (Disable b1 b2)

-- | Create an interrupt behaviour expression.
interrupt :: BExpr -> BExpr -> BExpr
interrupt b1 b2 = BExpr (Interrupt b1 b2)

-- | Create a process instantiation behaviour expression.
procInst :: ProcId -> [ChanId] -> [VExpr] -> BExpr
procInst p cs vs = BExpr (ProcInst p cs vs)

-- | Create a hide behaviour expression.
--   The given set of channels is hidden for its environment.
hide :: [ChanId] -> BExpr -> BExpr
hide cs b = BExpr (Hide cs b)

-- | Create a Value Environment behaviour expression.
valueEnv :: VEnv -> BExpr -> BExpr
valueEnv v b = BExpr (ValueEnv v b)

-- | Create a State Automaton behaviour expression.
stAut :: StatId -> VEnv -> [Trans] -> BExpr
stAut s v ts = BExpr (StAut s v ts)

-- | ActOffer
-- Offer on multiple channels with constraints
data ActOffer = ActOffer
  { offers     :: Set.Set Offer      -- PvdL -- why not? -- Map ChanId [ChanOffer]
  , hiddenvars :: Set.Set VarId
  , constraint :: VExpr
  } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable ActOffer

-- | Offer
-- Offer on a single channel (with multiple values)
data Offer = Offer
  { chanid     :: ChanId
  , chanoffers :: [ChanOffer]
  } deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable Offer

-- | Channel Offer
-- Offer of a single value
data  ChanOffer     = Quest  VarId
                    | Exclam VExpr
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable ChanOffer
instance SortOf ChanOffer where
  sortOf (Quest (VarId _nm _uid vs)) =  vs
  sortOf (Exclam vexp)               =  sortOf vexp

-- | symbolic transitions
data  Trans         = Trans  { from     :: StatId
                             , actoffer :: ActOffer
                             , update   :: VEnv
                             , to       :: StatId
                             }
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Resettable Trans

-- * Functions on behavior expressions.

-- | Equality modulo unique id's. Compare two behavior expressions for equality
-- ignoring the differences in identifiers.
(~~) :: BExpr -> BExpr -> Bool
be0 ~~ be1 = reset be0 == reset be1

-- | Test if the internal Behaviour Expression structure is valid.
valid :: BExpr -> Bool
valid (BehExprDefs.view -> Choice actual)   = actual == Set.toAscList (Set.fromList actual)
valid _                                     = True