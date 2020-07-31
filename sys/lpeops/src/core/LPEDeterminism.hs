{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEDeterminism
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEDeterminism (
filterNonDeterministicSummands,
getNonDeterministicSummands,
getNonDeterministicSummandPair,
getNonDeterministicSummandGroup,
areSummandsNonDeterministic
-- getDeterminizableSummandPair,
-- areSummandsDeterminizable
) where

-- import qualified Data.List as List
-- import qualified Data.Map as Map
import qualified Control.Monad as Monad
import qualified Data.Set as Set
import qualified EnvCore as IOC
import qualified TxsDefs
-- import qualified ChanId
import qualified ValExpr
import qualified Satisfiability as Sat
import LPETypes
-- import BlindSubst
import UntilFixedPoint
-- import LPESuccessors
-- import VarFactory

-- From a given set of summands, returns all summands that are non-deterministic in combination with another summand in that set.
filterNonDeterministicSummands :: TxsDefs.VExpr -> LPESummands -> IOC.IOC LPESummands
filterNonDeterministicSummands invariant summands =
    Set.unions <$> Monad.mapM perSummand (Set.toList summands)
  where
    perSummand :: LPESummand -> IOC.IOC LPESummands
    perSummand summand = do
        nonDetSummands <- getNonDeterministicSummands invariant summands summand
        if nonDetSummands /= Set.empty
        then return (Set.insert summand nonDetSummands)
        else return Set.empty
-- filterNonDeterministicSummands

-- From a given set of summands, returns all summands that could be non-deterministic in combination with the given summand.
getNonDeterministicSummands :: TxsDefs.VExpr -> LPESummands -> LPESummand -> IOC.IOC LPESummands
getNonDeterministicSummands invariant summands summand =
    Set.fromList <$> Monad.filterM (areSummandsNonDeterministic invariant summand) (Set.toList (Set.delete summand summands))
-- getNonDeterministicSummands

-- Returns a pair of two different summands from a given list of summands such that
-- the two summands could be non-deterministic.
getNonDeterministicSummandPair :: TxsDefs.VExpr -> [LPESummand] -> IOC.IOC (Maybe (LPESummand, LPESummand))
getNonDeterministicSummandPair _ [] = return Nothing
getNonDeterministicSummandPair invariant (summand:summands) = pairSearch (summand:summands) summands
  where
    pairSearch :: [LPESummand] -> [LPESummand] -> IOC.IOC (Maybe (LPESummand, LPESummand))
    pairSearch [] _ = return Nothing
    pairSearch [_] [] = return Nothing
    pairSearch (_:x2:xs) [] = pairSearch (x2:xs) xs
    pairSearch (x:xs) (y:ys) = do
        nonDet <- areSummandsNonDeterministic invariant x y
        if nonDet
        then return (Just (x, y))
        else pairSearch (x:xs) ys
-- getNonDeterministicSummandPair

-- Returns a group of different summands from a given list of summands such that
-- the two summands could be non-deterministic.
getNonDeterministicSummandGroup :: TxsDefs.VExpr -> LPESummands -> IOC.IOC (Maybe LPESummands)
getNonDeterministicSummandGroup invariant summands = do
    maybePair <- getNonDeterministicSummandPair invariant (Set.toList summands)
    case maybePair of
      Just (summand1, summand2) -> Just <$> untilFixedPointM expandGroup (Set.fromList [summand1, summand2])
      Nothing -> return Nothing
  where
    expandGroup :: LPESummands -> IOC.IOC LPESummands
    expandGroup soFar = do
        newSummands <- Monad.mapM (getNonDeterministicSummands invariant (summands Set.\\ soFar)) (Set.toList soFar)
        return (Set.union soFar (Set.unions newSummands))
    -- expandGroup
-- getNonDeterministicSummandGroup

-- Checks if the two given summands could be non-deterministic.
areSummandsNonDeterministic :: TxsDefs.VExpr -> LPESummand -> LPESummand -> IOC.IOC Bool
areSummandsNonDeterministic invariant summand1 summand2 =
    -- Summands must use the same channel:
    if lpeSmdChan summand1 /= lpeSmdChan summand2
    then return False
    else do -- Both guards must be able to be true at the same time while their local variables have the same value:
            let guards = [lpeSmdGuard summand1, lpeSmdGuard summand2]
            let varEqs = zipWith (\cv1 cv2 -> ValExpr.cstrEqual (ValExpr.cstrVar cv2) (ValExpr.cstrVar cv1)) (lpeSmdVars summand1) (lpeSmdVars summand2)
            Sat.couldBeSatisfiable (ValExpr.cstrAnd (Set.fromList (guards ++ varEqs))) invariant
-- areSummandsNonDeterministic

-- -- Returns a pair of two different summands from a given list of summands such that
-- -- the two summands could be non-deterministic.
-- getDeterminizableSummandPair :: TxsDefs.VExpr -> [LPESummand] -> IOC.IOC (Maybe (LPESummand, LPESummand))
-- getDeterminizableSummandPair _ [] = return Nothing
-- getDeterminizableSummandPair invariant (summand:summands) = pairSearch (summand:summands) summands
  -- where
    -- pairSearch :: [LPESummand] -> [LPESummand] -> IOC.IOC (Maybe (LPESummand, LPESummand))
    -- pairSearch [] _ = return Nothing
    -- pairSearch [_] [] = return Nothing
    -- pairSearch (_:x2:xs) [] = pairSearch (x2:xs) xs
    -- pairSearch (x:xs) (y:ys) = do
        -- nonDet <- areSummandsDeterminizable invariant (summand:summands) x y
        -- if nonDet
        -- then return (Just (x, y))
        -- else pairSearch (x:xs) ys
-- -- getDeterminizableSummandPair

-- -- Checks if the two given summands could be non-deterministic.
-- areSummandsDeterminizable :: TxsDefs.VExpr -> [LPESummand] -> LPESummand -> LPESummand -> IOC.IOC Bool
-- areSummandsDeterminizable invariant summands summand1 summand2 = do
    -- -- Summands must use the same channel:
    -- if lpeSmdChan summand1 /= lpeSmdChan summand2
    -- then return False
    -- else do -- Both guards must be able to be true at the same time:
            -- let guards = ValExpr.cstrAnd (Set.fromList [lpeSmdGuard summand1, lpeSmdGuard summand2])
            -- notSat <- Sat.isNotSatisfiable guards invariant
            -- if notSat
            -- then return False
            -- else do -- All action arguments must be able to have the same value.
                    -- -- To check this, substitute the (by definition fresh) channel variables of one summand into the other:
                    -- let chanVars1 = concatMap snd sortedChans1
                    -- let chanVars2 = concatMap snd sortedChans2
                    -- let subst = Map.fromList (zipWith (\cv1 cv2 -> (cv2, ValExpr.cstrVar cv1)) chanVars1 chanVars2)
                    -- guard2' <- doBlindSubst subst (lpeSmdGuard summand2)
                    -- let guardEq = ValExpr.cstrEqual (lpeSmdGuard summand1) guard2'
                    -- sat <- Sat.isSatisfiable guardEq invariant
                    -- if sat
                    -- then do succs1 <- getPossibleSuccessors (Set.fromList summands) invariant summand1
                            -- succs2 <- getPossibleSuccessors (Set.fromList summands) invariant summand2
                            -- return (not ((summand1 `List.elem` succs1 || summand1 `List.elem` succs2) && (summand2 `List.elem` succs1 || summand2 `List.elem` succs2)))
                    -- else return False
-- -- areSummandsDeterminizable


