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
, mkParallel
, mkEnable
, mkDisable
, mkInterrupt
, mkProcInst
, mkHide
  -- * Substitution
, TorXakis.BExpr.BExprConstructor.partSubst
)
where
import           Data.Either     (partitionEithers)
import           Data.Hashable
import qualified Data.HashMap    as HashMap
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Text       as T

import           TorXakis.BExpr.BExpr
import           TorXakis.BExpr.BExprContext
import           TorXakis.ChanDef
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.ProcExit
import           TorXakis.ProcSignature
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.ValExprContext
import           TorXakis.Value
import           TorXakis.VarDef

-- | Create a Stop behaviour expression.
--   The Stop behaviour is equal to dead lock.
mkStop :: BExpression
-- No special Stop constructor, use Choice with empty list instead
mkStop = BExpression (Choice Set.empty)

-- | Is behaviour expression equal to Stop behaviour?
isStop :: BExpression -> Bool
isStop (TorXakis.BExpr.BExpr.view -> Choice s) | Set.null s = True
isStop _                                                    = False

-- | Create an ActionPrefix behaviour expression.
-- TODO: how to handle EXIT >-> p <==> EXIT >-> STOP
--       should we give an error when containsEXIT (offers a) && not isStop(b)
--       or should we just rewrite the expression (currently give problems with LPE)
-- TODO: [VarDef] or VarsDecl?
-- TODO: check FreeVars of (ActOffer and BExpression) is subset of [VarDef] (i.e. vs)
mkActionPref :: BExprContext -> [VarDef] -> ActOffer -> BExpression -> Either MinError BExpression
mkActionPref ctx vs a b | null nonUniqueNames = unsafeActionPref ctx (Set.fromList vs) a b
                        | otherwise           = Left $ MinError (T.pack ("Non unique names : " ++ show nonUniqueNames))
    where
        nonUniqueNames :: [VarDef]
        nonUniqueNames = repeatedByName vs

unsafeActionPref :: BExprContext -> Set.Set VarDef -> ActOffer -> BExpression -> Either MinError BExpression
unsafeActionPref _ s a b = case TorXakis.ValExpr.view (constraint a) of
                             -- A?x [[ False ]] >-> p <==> stop
                             Vconst (Cbool False)    -> Right mkStop
                             _                       -> Right $ BExpression (ActionPref s a b)

-- | Create a guard behaviour expression.
mkGuard :: BExprContext -> ValExpression -> BExpression -> Either MinError BExpression
mkGuard _   c _   | getSort c /= SortBool  = Left $ MinError (T.pack ("Condition of Guard is not of expected sort Bool but " ++ show (getSort c)))
mkGuard ctx c b                            = unsafeGuard ctx c b

unsafeGuard :: BExprContext -> ValExpression -> BExpression -> Either MinError BExpression
-- [[ False ]] =>> p <==> stop
unsafeGuard _   (TorXakis.ValExpr.view -> Vconst (Cbool False)) _              = Right mkStop
-- [[ True ]] =>> p <==> p
unsafeGuard _   (TorXakis.ValExpr.view -> Vconst (Cbool True))  b              = Right b
-- [[ c ]] =>> stop <==> stop
unsafeGuard _   _ b                                                 | isStop b = Right mkStop
-- [[ c1 ]] =>> A?x [[ c2 ]] >-> p <==> A?x [[ IF c1 THEN c2 ELSE False FI ]] >-> p
-- Note: smart constructor ITE handles special case: [[ c1 ]] =>> A?x [[ True ]] >-> p <==> A?x [[ c1 ]] >-> p
unsafeGuard ctx v (TorXakis.BExpr.BExpr.view -> ActionPref vs (ActOffer o c) b) = mkConst ctx (Cbool False) >>= 
                                                                                  mkITE ctx v c >>= 
                                                                                  (\ite -> unsafeActionPref ctx vs (ActOffer o ite) b)
--  [[ c ]] =>> (p1 ## p2) <==> ( ([[ c ]] =>> p1) ## ([[ c ]] =>> p2) )
unsafeGuard ctx v (TorXakis.BExpr.BExpr.view -> Choice s)                      = case partitionEithers (map (unsafeGuard ctx v) (Set.toList s)) of
                                                                                    ([], l) -> unsafeChoice ctx (Set.fromList l)
                                                                                    (es, _) -> Left $ MinError (T.pack ("Errors in mkGuard - distribute over choice\n" ++ show es))
unsafeGuard _   v b                                                            = Right $ BExpression (Guard v b)

-- | Create a choice behaviour expression.
--  A choice combines zero or more behaviour expressions.
mkChoice :: BExprContext -> Set.Set BExpression -> Either MinError BExpression
mkChoice = unsafeChoice

-- TODO: should flatten happen only once?
unsafeChoice :: BExprContext -> Set.Set BExpression -> Either MinError BExpression
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
        flattenChoice :: Set.Set BExpression -> Set.Set BExpression
        flattenChoice = Set.unions . map fromBExpression . Set.toList

        fromBExpression :: BExpression -> Set.Set BExpression
        fromBExpression (TorXakis.BExpr.BExpr.view -> Choice s') = s'
        fromBExpression x                                        = Set.singleton x


-- | Create a parallel behaviour expression.
-- The behaviour expressions must synchronize on the given set of channels (and EXIT).
mkParallel :: a v -> Set.Set ChanDef -> [BExpression] -> Either MinError BExpression
mkParallel = unsafeParallel

-- TODO: should flatten happen only once?
unsafeParallel :: a v -> Set.Set ChanDef -> [BExpression] -> Either MinError BExpression
unsafeParallel _ cs bs = let fbs = flattenParallel bs
                            in Right $ BExpression (Parallel cs fbs)
    where
        -- nesting of parallels over the same channel sets are flatten
        --     (p |[ G ]| q) |[ G ]| r <==> p |[ G ]| q |[ G ]| r
        --    see https://wiki.haskell.org/Smart_constructors#Runtime_Optimisation_:_smart_constructors for inspiration for this implementation
        flattenParallel :: [BExpression] -> [BExpression]
        flattenParallel = concatMap fromBExpression

        fromBExpression :: BExpression -> [BExpression]
        fromBExpression (TorXakis.BExpr.BExpr.view -> Parallel pcs pbs) | cs == pcs  = pbs
        fromBExpression bexpr                                               = [bexpr]

-- | Create an enable behaviour expression.
mkEnable :: forall a v . VarDef v => a v -> BExpression -> [v] -> BExpression -> Either MinError BExpression
mkEnable ctx b1 cs b2 | null nonUniqueNames = case getExitKind b1 of
                                                Exit xs -> if xs == map getSort cs 
                                                            then unsafeEnable ctx b1 cs b2
                                                            else Left $ MinError (T.pack "Mismatch in sorts between ExitKind of initial process and ChanOffers")
                                                _       -> Left $ MinError (T.pack "ExitKind of initial process must be EXIT")
                      | otherwise           = Left $ MinError (T.pack ("Non unique names : " ++ show nonUniqueNames))
    where
        nonUniqueNames :: [v]
        nonUniqueNames = repeatedByName cs

unsafeEnable :: a v -> BExpression -> [v] -> BExpression -> Either MinError BExpression
-- stop >>> p <==> stop
unsafeEnable _ b _ _    | isStop b = Right mkStop
unsafeEnable _ b1 cs b2            = Right $ BExpression (Enable b1 cs b2)


-- | Create a disable behaviour expression.
mkDisable :: a v -> BExpression -> BExpression -> Either MinError BExpression
mkDisable = unsafeDisable

unsafeDisable :: a v -> BExpression -> BExpression -> Either MinError BExpression
-- stop [>> p <==> p
unsafeDisable _ b1 b2 | isStop b1 = Right b2
-- p [>> stop <==> p
unsafeDisable _ b1 b2 | isStop b2 = Right b1
unsafeDisable _ b1 b2             = Right $ BExpression (Disable b1 b2)


-- | Create an interrupt behaviour expression.
-- TODO: check functionality / EXitKind of b1 / b2
mkInterrupt :: a v -> BExpression -> BExpression -> Either MinError BExpression
mkInterrupt = unsafeInterrupt

unsafeInterrupt :: a v -> BExpression -> BExpression -> Either MinError BExpression
--  p [>< stop <==> p
unsafeInterrupt _ b1 b2 | isStop b2 = Right b1
unsafeInterrupt _ b1 b2             = Right $ BExpression (Interrupt b1 b2)

-- | Create a process instantiation behaviour expression.
mkProcInst :: a v -> ProcSignature -> [ChanDef] -> [ValExpression] -> Either MinError BExpression
mkProcInst = unsafeProcInst

unsafeProcInst :: a v -> ProcSignature -> [ChanDef] -> [ValExpression] -> Either MinError BExpression
-- TODO: when vs are all values, look up ProcInst and replace proc instance by body with substitution of parameters
unsafeProcInst _ p cs vs = Right $ BExpression (ProcInst p cs vs)

-- | Create a hide behaviour expression.
--   The given set of channels is hidden for its environment.
mkHide :: a v -> Set.Set ChanDef -> BExpression -> Either MinError BExpression
mkHide = unsafeHide

unsafeHide :: a v -> Set.Set ChanDef -> BExpression -> Either MinError BExpression
unsafeHide _ cs b = Right $ BExpression (Hide cs b)

------------------------------------------------------------------------------------------------------------------
-- Substitution
------------------------------------------------------------------------------------------------------------------

-- | find mismatches in sort in mapping
mismatchesSort :: (HasSort a, HasSort b) => HashMap.Map a b -> [ (a, b) ]
mismatchesSort = filter (\(a,b) -> getSort a /= getSort b) . HashMap.toList

-- | (Partial) Substitution: Substitute some variables by value expressions in a behaviour expression.
-- The Either is needed since substitution can cause an invalid ValExpr. 
-- For example, substitution of a variable by zero can cause a division by zero error
partSubst :: forall c v . BExprContext c v => c v -> HashMap.Map v ValExpression -> BExpression -> Either MinError BExpression
partSubst ctx mp be | null mismatches   = partSubstView mp (TorXakis.BExpr.BExpr.view be)
                    | otherwise         = Left $ MinError (T.pack ("Sort mismatches in map : " ++ show mismatches))
  where
    mismatches :: [ (v, ValExpression) ]
    mismatches = mismatchesSort mp

    deleteVars :: (Ord k, Hashable k) => HashMap.Map k w -> [k] -> HashMap.Map k w
    deleteVars = foldl (flip HashMap.delete)

    partSubstView :: HashMap.Map v ValExpression -> BExpressionView v -> Either MinError BExpression
    partSubstView mp' be' | HashMap.null mp'   = Right (BExpression be')
    partSubstView mp' be'                      = partSubstView' mp' be'

    partSubstView' :: HashMap.Map v ValExpression -> BExpressionView v -> Either MinError BExpression
    partSubstView' mp' (ActionPref vs a b)  = let newMp :: HashMap.Map v ValExpression
                                                  newMp = deleteVars mp' (Set.toList vs) in
                                                subst ctx newMp a >>= (\na ->
                                                partSubstView newMp (TorXakis.BExpr.BExpr.view b) >>=
                                                unsafeActionPref ctx vs na)
    partSubstView' mp' (Guard c b)          = TorXakis.ValExprContext.partSubst ctx mp' c >>= (\nc ->
                                              partSubstView' mp' (TorXakis.BExpr.BExpr.view b) >>=
                                              unsafeGuard ctx nc)
    partSubstView' mp' (Choice s)           = case partitionEithers (map (partSubstView' mp' . TorXakis.BExpr.BExpr.view) (Set.toList s)) of
                                                ([], nls)   -> unsafeChoice ctx (Set.fromList nls)
                                                (es,   _)   -> Left $ MinError (T.pack ("Subst partSubst 'Choice' failed\n" ++ show es))
    partSubstView' mp' (Parallel cs bs)     = case partitionEithers (map (partSubstView' mp' . TorXakis.BExpr.BExpr.view) bs) of
                                                ([], nbs)   -> unsafeParallel ctx cs nbs
                                                (es,   _)   -> Left $ MinError (T.pack ("Subst partSubst 'Parallel' failed\n" ++ show es))
    partSubstView' mp' (Enable b1 vs b2)    = partSubstView' mp' (TorXakis.BExpr.BExpr.view b1) >>= (\nb1 ->
                                              partSubstView (deleteVars mp' vs) (TorXakis.BExpr.BExpr.view b2) >>=
                                              unsafeEnable ctx nb1 vs)
    partSubstView' mp' (Disable b1 b2)      = partSubstView' mp' (TorXakis.BExpr.BExpr.view b1) >>= (\nb1 ->
                                              partSubstView' mp' (TorXakis.BExpr.BExpr.view b2) >>=
                                              unsafeDisable ctx nb1)
    partSubstView' mp' (Interrupt b1 b2)    = partSubstView' mp' (TorXakis.BExpr.BExpr.view b1) >>= (\nb1 ->
                                              partSubstView' mp' (TorXakis.BExpr.BExpr.view b2) >>=
                                              unsafeInterrupt ctx nb1)
    partSubstView' mp' (ProcInst p cs vs)   = case partitionEithers (map (TorXakis.ValExprContext.partSubst ctx mp') vs) of
                                                ([], nvs)   -> unsafeProcInst ctx p cs nvs
                                                (es,   _)   -> Left $ MinError (T.pack ("Subst partSubst 'ProcInst' failed\n" ++ show es))
    partSubstView' mp' (Hide cs b)          = partSubstView' mp' (TorXakis.BExpr.BExpr.view b) >>=
                                              unsafeHide ctx cs

subst :: forall c v . BExprContext c v => c v -> HashMap.Map v ValExpression -> ActOffer -> Either MinError ActOffer
subst ctx mp (ActOffer m c) = case TorXakis.ValExprContext.partSubst ctx mp c of
                                Left e -> Left $ MinError (T.pack ("Subst 'ActOffer' failed on constraint\n" ++ show e))
                                Right c' -> let l :: [Either MinError (ChanDef,[ValExpression])]
                                                l = map (\(a,b) -> case partitionEithers (map (TorXakis.ValExprContext.partSubst ctx mp) b) of
                                                                        ([], b') -> Right (a,b')
                                                                        (es, _)  -> Left $ MinError (T.pack ("Channel = " ++ show a ++ "\n" ++ show es)) 
                                                        )
                                                        (Map.toList m)
                                              in
                                                case partitionEithers l of
                                                    ([], n) -> Right $ ActOffer (Map.fromList n) c'
                                                    (es, _) -> Left $ MinError (T.pack ("Subst 'ActOffer' failed on Offers\n" ++ show es))