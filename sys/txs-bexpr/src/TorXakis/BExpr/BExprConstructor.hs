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
import           TorXakis.BExprContext
import           TorXakis.Chan
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.ProcSignature
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var

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
mkActionPref :: BExprContext c => c -> [VarDef] -> ActOffer -> BExpression -> Either Error BExpression
mkActionPref ctx vs a b | null nonUniqueNames = mkVarsDecl ctx vs >>= (\vd -> unsafeActionPref ctx vd a b)
                        | otherwise           = Left $ Error (T.pack ("Non unique names : " ++ show nonUniqueNames))
    where
        nonUniqueNames :: [VarDef]
        nonUniqueNames = repeatedByName vs

unsafeActionPref :: c -> VarsDecl -> ActOffer -> BExpression -> Either Error BExpression
unsafeActionPref _ s a b = case TorXakis.ValExpr.view (constraint a) of
                             -- A?x [[ False ]] >-> p <==> stop
                             Vconst (Cbool False)    -> Right mkStop
                             _                       -> Right $ BExpression (ActionPref s a b)

-- | Create a guard behaviour expression.
mkGuard :: BExprContext c => c -> ValExpression -> BExpression -> Either Error BExpression
mkGuard ctx c _   | getSort ctx c /= SortBool  = Left $ Error (T.pack ("Condition of Guard is not of expected sort Bool but " ++ show (getSort ctx c)))
mkGuard ctx c b                                = unsafeGuard ctx c b

unsafeGuard :: BExprContext c => c -> ValExpression -> BExpression -> Either Error BExpression
-- [[ False ]] =>> p <==> stop
unsafeGuard _   (TorXakis.ValExpr.view -> Vconst (Cbool False)) _              = Right mkStop
-- [[ True ]] =>> p <==> p
unsafeGuard _   (TorXakis.ValExpr.view -> Vconst (Cbool True))  b              = Right b
-- [[ c ]] =>> stop <==> stop
unsafeGuard _   _ b                                                 | isStop b = Right mkStop
-- [[ c1 ]] =>> A?x [[ c2 ]] >-> p <==> A?x [[ IF c1 THEN c2 ELSE False FI ]] >-> p
-- Note: smart constructor ITE handles special case: [[ c1 ]] =>> A?x [[ True ]] >-> p <==> A?x [[ c1 ]] >-> p
unsafeGuard ctx v (TorXakis.BExpr.BExpr.view -> ActionPref vs (ActOffer o c m) b) = mkConst ctx (Cbool False) >>= 
                                                                                      mkITE ctx v c >>= 
                                                                                     (\ite -> unsafeActionPref ctx vs (ActOffer o ite m) b)
--  [[ c ]] =>> (p1 ## p2) <==> ( ([[ c ]] =>> p1) ## ([[ c ]] =>> p2) )
unsafeGuard ctx v (TorXakis.BExpr.BExpr.view -> Choice s)                      = case partitionEithers (map (unsafeGuard ctx v) (Set.toList s)) of
                                                                                    ([], l) -> unsafeChoice ctx (Set.fromList l)
                                                                                    (es, _) -> Left $ Error (T.pack ("Errors in mkGuard - distribute over choice\n" ++ show es))
unsafeGuard _   v b                                                            = Right $ BExpression (Guard v b)

-- | Create a choice behaviour expression.
--  A choice combines zero or more behaviour expressions.
mkChoice :: c -> Set.Set BExpression -> Either Error BExpression
mkChoice = unsafeChoice

-- TODO: should flatten happen only once?
unsafeChoice :: c -> Set.Set BExpression -> Either Error BExpression
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
mkParallel :: c -> Set.Set ChanRef -> [BExpression] -> Either Error BExpression
mkParallel = unsafeParallel

-- TODO: should flatten happen only once?
unsafeParallel :: c -> Set.Set ChanRef -> [BExpression] -> Either Error BExpression
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
        fromBExpression bexpr                                                        = [bexpr]

-- | Create an enable behaviour expression.
mkEnable :: BExprContext c => c -> BExpression -> [VarDef] -> BExpression -> Either Error BExpression
mkEnable ctx b1 vs b2 = case getProcExit ctx b1 of
                             Exit xs -> if xs == map (getSort ctx) vs 
                                           then mkVarsDecl ctx vs >>= (\vd -> unsafeEnable ctx b1 vd b2)
                                           else Left $ Error "Mismatch in sorts between ExitKind of initial process and ChanOffers"
                             _       -> Left $ Error "ExitKind of initial process must be EXIT"

unsafeEnable :: c -> BExpression -> VarsDecl -> BExpression -> Either Error BExpression
-- stop >>> p <==> stop
unsafeEnable _ b _ _    | isStop b = Right mkStop
unsafeEnable _ b1 cs b2            = Right $ BExpression (Enable b1 cs b2)


-- | Create a disable behaviour expression.
mkDisable :: c -> BExpression -> BExpression -> Either Error BExpression
mkDisable = unsafeDisable

unsafeDisable :: c -> BExpression -> BExpression -> Either Error BExpression
-- stop [>> p <==> p
unsafeDisable _ b1 b2 | isStop b1 = Right b2
-- p [>> stop <==> p
unsafeDisable _ b1 b2 | isStop b2 = Right b1
unsafeDisable _ b1 b2             = Right $ BExpression (Disable b1 b2)


-- | Create an interrupt behaviour expression.
-- TODO: check functionality / EXitKind of b1 / b2
mkInterrupt :: c -> BExpression -> BExpression -> Either Error BExpression
mkInterrupt = unsafeInterrupt

unsafeInterrupt :: c -> BExpression -> BExpression -> Either Error BExpression
--  p [>< stop <==> p
unsafeInterrupt _ b1 b2 | isStop b2 = Right b1
unsafeInterrupt _ b1 b2             = Right $ BExpression (Interrupt b1 b2)

-- | Create a process instantiation behaviour expression.
mkProcInst :: c -> ProcSignature -> [ChanRef] -> [ValExpression] -> Either Error BExpression
mkProcInst = unsafeProcInst

unsafeProcInst :: c -> ProcSignature -> [ChanRef] -> [ValExpression] -> Either Error BExpression
-- TODO: when vs are all values, look up ProcInst and replace proc instance by body with substitution of parameters
unsafeProcInst _ p cs vs = Right $ BExpression (ProcInst p cs vs)

-- | Create a hide behaviour expression.
--   The given set of channels is hidden for its environment.
mkHide :: SortContext c => c -> [ChanDef] -> BExpression -> Either Error BExpression
mkHide ctx cs b = mkChansDecl ctx cs >>= (\d -> unsafeHide ctx (toMapByChanRef (TorXakis.Chan.toList d)) b)

unsafeHide :: c -> Map.Map ChanRef ChanDef -> BExpression -> Either Error BExpression
unsafeHide _ cs b = Right $ BExpression (Hide cs b)

------------------------------------------------------------------------------------------------------------------
-- Substitution
------------------------------------------------------------------------------------------------------------------

-- | find mismatches in sort in mapping
mismatchesSort :: VarContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> [ (RefByName VarDef, ValExpression) ]
mismatchesSort ctx = filter (\(a,b) -> case lookupVar (toName a) ctx of
                                            Nothing -> True         -- not expected for a valid mapping
                                            Just v  -> getSort ctx v /= getSort ctx b
                            ) . HashMap.toList

-- | (Partial) Substitution: Substitute some variables by value expressions in a behaviour expression.
-- The Either is needed since substitution can cause an invalid ValExpr. 
-- For example, substitution of a variable by zero can cause a division by zero error
partSubst :: BExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> BExpression -> Either Error BExpression
partSubst ctx mp be | null mismatches   = partSubstView mp (TorXakis.BExpr.BExpr.view be)
                    | otherwise         = Left $ Error ("Sort mismatches in map : " ++ show mismatches)
  where
    mismatches :: [ (RefByName VarDef, ValExpression) ]
    mismatches = mismatchesSort ctx mp

    deleteVars :: (Ord k, Hashable k) => HashMap.Map k w -> [k] -> HashMap.Map k w
    deleteVars = foldl (flip HashMap.delete)

    partSubstView :: HashMap.Map (RefByName VarDef) ValExpression -> BExpressionView -> Either Error BExpression
    partSubstView mp' be' | HashMap.null mp'   = Right (BExpression be')
    partSubstView mp' be'                      = partSubstView' mp' be'

    partSubstView' :: HashMap.Map (RefByName VarDef) ValExpression -> BExpressionView -> Either Error BExpression
    partSubstView' mp' (ActionPref vs a b)  = let newMp :: HashMap.Map (RefByName VarDef) ValExpression
                                                  newMp = deleteVars mp' (map (RefByName . name) (TorXakis.Var.toList vs)) in
                                                TorXakis.BExpr.BExprConstructor.subst ctx newMp a >>= (\na ->
                                                partSubstView newMp (TorXakis.BExpr.BExpr.view b) >>=
                                                unsafeActionPref ctx vs na)
    partSubstView' mp' (Guard c b)          = TorXakis.ValExpr.subst ctx mp' c >>= (\nc ->
                                              partSubstView' mp' (TorXakis.BExpr.BExpr.view b) >>=
                                              unsafeGuard ctx nc)
    partSubstView' mp' (Choice s)           = case partitionEithers (map (partSubstView' mp' . TorXakis.BExpr.BExpr.view) (Set.toList s)) of
                                                ([], nls)   -> unsafeChoice ctx (Set.fromList nls)
                                                (es,   _)   -> Left $ Error (T.pack ("Subst partSubst 'Choice' failed\n" ++ show es))
    partSubstView' mp' (Parallel cs bs)     = case partitionEithers (map (partSubstView' mp' . TorXakis.BExpr.BExpr.view) bs) of
                                                ([], nbs)   -> unsafeParallel ctx cs nbs
                                                (es,   _)   -> Left $ Error (T.pack ("Subst partSubst 'Parallel' failed\n" ++ show es))
    partSubstView' mp' (Enable b1 vs b2)    = partSubstView' mp' (TorXakis.BExpr.BExpr.view b1) >>= (\nb1 ->
                                              partSubstView (deleteVars mp' (map (RefByName . name) (TorXakis.Var.toList vs))) (TorXakis.BExpr.BExpr.view b2) >>=
                                              unsafeEnable ctx nb1 vs)
    partSubstView' mp' (Disable b1 b2)      = partSubstView' mp' (TorXakis.BExpr.BExpr.view b1) >>= (\nb1 ->
                                              partSubstView' mp' (TorXakis.BExpr.BExpr.view b2) >>=
                                              unsafeDisable ctx nb1)
    partSubstView' mp' (Interrupt b1 b2)    = partSubstView' mp' (TorXakis.BExpr.BExpr.view b1) >>= (\nb1 ->
                                              partSubstView' mp' (TorXakis.BExpr.BExpr.view b2) >>=
                                              unsafeInterrupt ctx nb1)
    partSubstView' mp' (ProcInst p cs vs)   = case partitionEithers (map (TorXakis.ValExpr.subst ctx mp') vs) of
                                                ([], nvs)   -> unsafeProcInst ctx p cs nvs
                                                (es,   _)   -> Left $ Error (T.pack ("Subst partSubst 'ProcInst' failed\n" ++ show es))
    partSubstView' mp' (Hide cs b)          = partSubstView' mp' (TorXakis.BExpr.BExpr.view b) >>=
                                              unsafeHide ctx cs

subst :: BExprContext c => c -> HashMap.Map (RefByName VarDef) ValExpression -> ActOffer -> Either Error ActOffer
subst _   _  QuiescenceStep   = Right QuiescenceStep
subst ctx mp (ActOffer o c m) = case TorXakis.ValExpr.subst ctx mp c of
                                     Left e   -> Left $ Error ("Subst 'ActOffer' failed on constraint\n" ++ show e)
                                     Right c' -> let l :: [Either Error (ChanRef,[ValExpression])]
                                                     l = map (\(a,b) -> case partitionEithers (map (TorXakis.ValExpr.subst ctx mp) b) of
                                                                             ([], b') -> Right (a,b')
                                                                             (es, _)  -> Left $ Error ("Subst 'ActOffer' failed on Channel = " ++ show a ++ "\n" ++ show es)
                                                             )
                                                             (Map.toList o)
                                                   in
                                                     case partitionEithers l of
                                                         ([], n) -> Right $ ActOffer (Map.fromList n) c' m
                                                         (es, _) -> Left $ Error ("Subst 'ActOffer' failed on Offers\n" ++ show es)