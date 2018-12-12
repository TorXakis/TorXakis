{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  BExprConstructor
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module introduces the constructors related to behaviour expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module TorXakis.BExpr.BExprConstructor
(
  -- * Smart Constructors and checks for Behaviour Expressions
  mkStop
, isStop
, mkActionPref
, mkGuard
, mkChoice
-- , parallel
-- , enable
-- , disable
-- , interrupt
-- , procInst
-- , hide
-- , valueEnv
-- , stAut
  -- * Standard channel identifiers
-- , chanIdExit
-- , chanIdIstep
-- , chanIdQstep
-- , chanIdHit
-- , chanIdMiss
-- , containsEXIT
)
where

import           Data.Either     (partitionEithers)
import qualified Data.Set        as Set
import qualified Data.Text       as T

import           TorXakis.BExpr.BExpr
import           TorXakis.BExpr.BExprContext
--import           TorXakis.BExpr.ExitKind
import           TorXakis.Error
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.VarDef

-- | Create a Stop behaviour expression.
--   The Stop behaviour is equal to dead lock.
mkStop :: BExpression v
-- No special Stop constructor, use Choice with empty list instead
mkStop = BExpression (Choice Set.empty)

-- | Is behaviour expression equal to Stop behaviour?
isStop :: BExpression v -> Bool
isStop (TorXakis.BExpr.BExpr.view -> Choice s) | Set.null s = True
isStop _                                                    = False

-- | Create an ActionPrefix behaviour expression.
-- TODO: how to handle EXIT >-> p <==> EXIT >-> STOP
--       should we give an error when containsEXIT (offers a) && not isStop(b)
--       or should we just rewrite the expression (currently give problems with LPE)
mkActionPref :: a v -> ActOffer -> BExpression v -> Either MinError (BExpression v)
mkActionPref = unsafeActionPref

unsafeActionPref :: a v -> ActOffer -> BExpression v -> Either MinError (BExpression v)
unsafeActionPref _ a b = case TorXakis.ValExpr.view (constraint a) of
                            -- A?x [[ False ]] >-> p <==> stop
                            Vconst (Cbool False)    -> Right mkStop
                            _                       -> Right $ BExpression (ActionPref a b)

-- | Create a guard behaviour expression.
mkGuard :: BExprContext a MinimalVarDef => a MinimalVarDef -> ValExpr -> BExpr -> Either MinError BExpr
mkGuard _   c _   | getSort c /= SortBool  = Left $ MinError (T.pack ("Condition of Guard is not of expected sort Bool but " ++ show (getSort c)))
mkGuard ctx c b                            = unsafeGuard ctx c b

unsafeGuard :: BExprContext a MinimalVarDef => a MinimalVarDef -> ValExpr -> BExpr -> Either MinError BExpr
-- [[ False ]] =>> p <==> stop
unsafeGuard _   (TorXakis.ValExpr.view -> Vconst (Cbool False)) _              = Right mkStop
-- [[ True ]] =>> p <==> p
unsafeGuard _   (TorXakis.ValExpr.view -> Vconst (Cbool True))  b              = Right b
-- [[ c ]] =>> stop <==> stop
unsafeGuard _   _ b                                                 | isStop b = Right mkStop
-- [[ c1 ]] =>> A?x [[ c2 ]] >-> p <==> A?x [[ IF c1 THEN c2 ELSE False FI ]] >-> p
-- Note: smart constructor ITE handles special case: [[ c1 ]] =>> A?x [[ True ]] >-> p <==> A?x [[ c1 ]] >-> p
unsafeGuard ctx v (TorXakis.BExpr.BExpr.view -> ActionPref (ActOffer o h c) b) = mkConst ctx (Cbool False) >>= 
                                                                                 mkITE ctx v c >>= 
                                                                                 (\ite -> unsafeActionPref ctx (ActOffer o h ite) b)
--  [[ c ]] =>> (p1 ## p2) <==> ( ([[ c ]] =>> p1) ## ([[ c ]] =>> p2) )
unsafeGuard ctx v (TorXakis.BExpr.BExpr.view -> Choice s)                      = case partitionEithers (map (unsafeGuard ctx v) (Set.toList s)) of
                                                                                    ([], l) -> unsafeChoice ctx (Set.fromList l)
                                                                                    (es, _) -> Left $ MinError (T.pack ("Errors in mkGuard - distribute over choice\n" ++ show es))
unsafeGuard _   v b                                                            = Right $ BExpression (Guard v b)

-- | Create a choice behaviour expression.
--  A choice combines zero or more behaviour expressions.
mkChoice :: Ord v => a v -> Set.Set (BExpression v) -> Either MinError (BExpression v)
mkChoice = unsafeChoice

unsafeChoice :: forall a v . Ord v => a v -> Set.Set (BExpression v) -> Either MinError (BExpression v)
unsafeChoice _ s = let fs = flattenChoice s in
                        Right $ case Set.toList fs of
                                    []  -> mkStop
                                    [a] -> a
                                    _   -> BExpression (Choice fs)
    where
        -- 1. nesting of choices are flatten
        --    (p ## q) ## r <==> p ## q ## r
        --    see https://wiki.haskell.org/Smart_constructors#Runtime_Optimisation_:_smart_constructors for inspiration for this implementation
        -- 2. elements in a set are distinctive
        --    hence p ## p <==> p
        -- 3. since stop == Choice Set.empty, we automatically have p ## stop <==> p
        flattenChoice :: Set.Set (BExpression v) -> Set.Set (BExpression v)
        flattenChoice = Set.unions . map fromBExpr . Set.toList

        fromBExpr :: BExpression v -> Set.Set (BExpression v)
        fromBExpr (TorXakis.BExpr.BExpr.view -> Choice s') = s'
        fromBExpr x                                        = Set.singleton x

{-
-- | Create a parallel behaviour expression.
-- The behaviour expressions must synchronize on the given set of channels (and EXIT).
parallel :: Set.Set ChanId -> [BExpr] -> BExpr
parallel cs bs = let fbs = flattenParallel bs
                    in BExpr (Parallel cs fbs)
    where
        -- nesting of parallels over the same channel sets are flatten
        --     (p |[ G ]| q) |[ G ]| r <==> p |[ G ]| q |[ G ]| r
        --    see https://wiki.haskell.org/Smart_constructors#Runtime_Optimisation_:_smart_constructors for inspiration for this implementation
        flattenParallel :: [BExpr] -> [BExpr]
        flattenParallel = concatMap fromBExpr

        fromBExpr :: BExpr -> [BExpr]
        fromBExpr (BehExprDefs.view -> Parallel pcs pbs) | cs == pcs  = pbs
        fromBExpr bexpr                                  = [bexpr]

-- | Create an enable behaviour expression.
enable :: BExpr -> [ChanOffer] -> BExpr -> BExpr
-- stop >>> p <==> stop
enable b _ _    | isStop b = stop
enable b1 cs b2 = BExpr (Enable b1 cs b2)

-- | Create a disable behaviour expression.
disable :: BExpr -> BExpr -> BExpr
-- stop [>> p <==> p
disable b1 b2 | isStop b1 = b2
-- p [>> stop <==> p
disable b1 b2 | isStop b2 = b1
disable b1 b2 = BExpr (Disable b1 b2)

-- | Create an interrupt behaviour expression.
interrupt :: BExpr -> BExpr -> BExpr
--  p [>< stop <==> p
interrupt b1 b2 | isStop b2 = b1
interrupt b1 b2 = BExpr (Interrupt b1 b2)

-- | Create a process instantiation behaviour expression.
procInst :: ProcId -> [ChanId] -> [VExpr] -> BExpr
procInst p cs vs = BExpr (ProcInst p cs vs)

-- | Create a hide behaviour expression.
--   The given set of channels is hidden for its environment.
hide :: Set.Set ChanId -> BExpr -> BExpr
hide cs b = BExpr (Hide cs b)

-- * Standard channel identifiers
chanIdExit :: ChanId
chanIdExit  = ChanId "EXIT"  901 []
chanIdIstep :: ChanId
chanIdIstep = ChanId "ISTEP" 902 []
chanIdQstep :: ChanId
chanIdQstep = ChanId "QSTEP" 903 []
chanIdHit :: ChanId
chanIdHit   = ChanId "HIT"   904 []
chanIdMiss :: ChanId
chanIdMiss  = ChanId "MISS"  905 []

containsEXIT :: Set.Set Offer -> Bool
containsEXIT os = chanIdExit `Set.member` Set.map chanid os

-- * Functions on behavior expressions.

-- | Equality modulo unique id's. Compare two behavior expressions for equality
-- ignoring the differences in identifiers.
(~~) :: BExpr -> BExpr -> Bool
be0 ~~ be1 = reset be0 == reset be1
-}