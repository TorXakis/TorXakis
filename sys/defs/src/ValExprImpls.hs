{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE ViewPatterns #-}
module ValExprImpls
( cstrFunc
, cstrCstr
, cstrIsCstr
, cstrAccess
, cstrNot
, cstrEqual
, cstrEnv
, cstrIte
, cstrVar
, cstrConst
, cstrModulo
, cstrMinus
, cstrSum
, cstrDivide
, cstrAnd
, cstrPredef
, cstrError
, cstrOr
, cstrImplies
)
where

import qualified Data.Set    as Set
import           Data.Text   (Text)
import           Debug.Trace as Trace

import           ConstDefs
import           CstrId
import           FuncId
import           Sum
import           ValExprDefs
import           Variable

cstrFunc :: (Variable v) => FuncId -> [ValExpr v] -> ValExpr v
cstrFunc f a = ValExpr (Vfunc f a)

-- | Apply ADT Constructor of constructor with CstrId and the provided arguments (the list of value expressions).
-- Preconditions are /not/ checked.
cstrCstr :: CstrId -> [ValExpr v] -> ValExpr v
cstrCstr c a = ValExpr (Vcstr c a)

-- | Is the provided value expression made by the ADT constructor with CstrId?
-- Preconditions are /not/ checked.
cstrIsCstr :: CstrId -> ValExpr v -> ValExpr v
cstrIsCstr c1 (view -> Vcstr c2 _)         = cstrConst (Cbool (c1 == c2) )
cstrIsCstr c1 (view -> Vconst (Cstr c2 _)) = cstrConst (Cbool (c1 == c2) )
cstrIsCstr c e                             = ValExpr (Viscstr c e)

-- | Apply ADT Accessor of constructor with CstrId on field with given position on the provided value expression.
-- Preconditions are /not/ checked.
cstrAccess :: CstrId -> Int -> ValExpr v -> ValExpr v
cstrAccess c1 p1 e@(view -> Vcstr c2 fields) =
    if c1 == c2 -- prevent crashes due to model errors
        then fields!!p1
        else Trace.trace ("Error in model: Accessing field with number " ++ show p1 ++ " of constructor " ++ show c1 ++ " on instance from constructor " ++ show c2) $
                ValExpr (Vaccess c1 p1 e)
cstrAccess c1 p1 e@(view -> Vconst (Cstr c2 fields)) =
    if c1 == c2 -- prevent crashes due to model errors
        then cstrConst (fields!!p1)
        else Trace.trace ("Error in model: Accessing field with number " ++ show p1 ++ " of constructor " ++ show c1 ++ " on value from constructor " ++ show c2) $
                ValExpr (Vaccess c1 p1 e)
cstrAccess c p e = ValExpr (Vaccess c p e)

-- | Is ValExpr a Constant/Value Expression?     
isConst :: ValExpr v -> Bool
isConst (view -> Vconst{}) = True
isConst _                  = False

cstrConst :: Const -> ValExpr v
cstrConst c = ValExpr (Vconst c)

cstrVar :: v -> ValExpr v
cstrVar v = ValExpr (Vvar v)

-- | Apply operator ITE (IF THEN ELSE) on the provided value expressions.
-- Preconditions are /not/ checked.
cstrIte :: ValExpr v -> ValExpr v -> ValExpr v -> ValExpr v
-- if (not b) then tb else fb == if b then fb else tb
cstrIte (view -> Vnot n) tb fb = ValExpr (Vite n fb tb)
cstrIte cs tb fb               = ValExpr (Vite cs tb fb)

cstrEnv :: VarEnv v v -> ValExpr v -> ValExpr v
cstrEnv ve e = ValExpr (Venv ve e)

-- | Apply operator Equal on the provided value expressions.
-- Preconditions are /not/ checked.
cstrEqual :: (Ord v) => ValExpr v -> ValExpr v -> ValExpr v
-- Simplification a == a <==> True
cstrEqual ve1 ve2 | ve1 == ve2                      = cstrConst (Cbool True)
-- Simplification Different Values <==> False : use Same Values are already detected in previous step
cstrEqual (view -> Vconst _) (view -> Vconst _)     = cstrConst (Cbool False)
-- Simplification True == e <==> e (twice)
cstrEqual (view -> Vconst (Cbool True)) e           = e
cstrEqual e (view -> Vconst (Cbool True))           = e

-- Simplification False == e <==> not e (twice)
cstrEqual (view -> Vconst (Cbool False)) e              = cstrNot e
cstrEqual e (view -> Vconst (Cbool False))              = cstrNot e
-- Not x == x <==> false (twice)
cstrEqual e (view -> Vnot n) | e == n                   = cstrConst (Cbool False)
cstrEqual (view -> Vnot n) e | e == n                   = cstrConst (Cbool False)
-- Not x == Not y <==> x == y   -- same representation
cstrEqual (view -> Vnot n1) (view -> Vnot n2)     = cstrEqual n1 n2
-- Not a == b <==> a == Not b -- same representation (twice)
cstrEqual x@(view -> Vnot n) e                = if n <= e
                                                        then ValExpr (Vequal x e)
                                                        else ValExpr (Vequal (cstrNot e) n)
cstrEqual e x@(view -> Vnot n)                = if n <= e
                                                        then ValExpr (Vequal x e)
                                                        else ValExpr (Vequal (cstrNot e) n)
-- a == b <==> b == a -- same representation
cstrEqual ve1 ve2                                   = if ve1 <= ve2
                                                        then ValExpr (Vequal ve1 ve2)
                                                        else ValExpr (Vequal ve2 ve1)

-- | Is ValExpr a Not Expression?
isNot :: ValExpr v -> Bool
isNot (view -> Vnot {}) = True
isNot _                 = False

-- | Apply operator Not on the provided value expression.
-- Preconditions are /not/ checked.
cstrNot :: ValExpr v -> ValExpr v
cstrNot (view -> Vconst (Cbool True))       = cstrConst (Cbool False)
cstrNot (view -> Vconst (Cbool False))      = cstrConst (Cbool True)
cstrNot (view -> Vnot ve)                   = ve
-- not (if cs then tb else fb) == if cs then not (tb) else not (fb)
cstrNot (view -> Vite cs tb fb)             = ValExpr (Vite cs (cstrNot tb) (cstrNot fb))
cstrNot ve                                  = ValExpr (Vnot ve)

-- | Is ValExpr an And Expression?
isAnd :: ValExpr v -> Bool
isAnd (view -> Vand {}) = True
isAnd _                 = False

-- | Apply operator And on the provided set of value expressions.
-- Preconditions are /not/ checked.
cstrAnd :: (Ord v) => Set.Set (ValExpr v) -> ValExpr v
cstrAnd ms =
        let (ands, nonands) = Set.partition isAnd ms in
            cstrAnd' $ foldl Set.union nonands (map (\(view -> Vand a) -> a) (Set.toList ands))

-- And doesn't contain elements of type Vand.
cstrAnd' :: (Ord v) => Set.Set (ValExpr v) -> ValExpr v
cstrAnd' s =
    if Set.member (cstrConst (Cbool False)) s
        then cstrConst (Cbool False)
        else let s' = Set.delete (cstrConst (Cbool True)) s in
                case Set.size s' of
                    0   -> cstrConst (Cbool True)
                    1   -> head (Set.toList s')
                    _   ->  -- not(x) and x == False
                            let (nots, _) = Set.partition isNot s' in
                                if any (contains s') (map (\(view -> Vnot n) -> n) (Set.toList nots))
                                    then cstrConst (Cbool False)
                                    else ValExpr (Vand s')
                                    -- todo? also check :
                                    -- 0 <= x and 0 <= -x <==> x == 0
                                    -- not (0 <= x) and not (0 <= -x) <==> False
    where
        contains :: (Ord v) => Set.Set (ValExpr v) -> ValExpr v -> Bool
        contains set (view -> Vand a) = all (`Set.member` set) (Set.toList a)
        contains set a                = Set.member a set

-- Divide

-- | Apply operator Divide on the provided value expressions.
-- Preconditions are /not/ checked.
cstrDivide :: ValExpr v -> ValExpr v -> ValExpr v
cstrDivide vet ven@(view -> Vconst (Cint n)) | n == 0 = Trace.trace "Error in model: Division by Zero in Divide" $ ValExpr (Vdivide vet ven)
cstrDivide (view ->  Vconst (Cint t)) (view ->  Vconst (Cint n)) = cstrConst (Cint (t `div` n) )
cstrDivide vet ven = ValExpr (Vdivide vet ven)
    
-- Modulo

-- | Apply operator Modulo on the provided value expressions.
-- Preconditions are /not/ checked.
cstrModulo :: ValExpr v -> ValExpr v -> ValExpr v
cstrModulo vet ven@(view -> Vconst (Cint n)) | n == 0 = Trace.trace "Error in model: Division by Zero in Modulo" $ ValExpr (Vmodulo vet ven) 
cstrModulo (view -> Vconst (Cint t)) (view -> Vconst (Cint n)) = cstrConst (Cint (t `mod` n) )
cstrModulo vet ven = ValExpr (Vmodulo vet ven)

-- | Apply operator Minus on the provided value expression.
-- Preconditions are /not/ checked.
cstrMinus :: Ord v => ValExpr v -> ValExpr v
cstrMinus (view -> Vconst (Cint x)) = cstrConst (Cint (-x))
cstrMinus (view -> Vsum s)          = cstrSum (Sum.multiply (-1) s)
cstrMinus v                         = ValExpr (Vsum (Sum.fromDistinctAscMultiplierList [(v,-1)]))

-- Sum

-- | Is ValExpr a Sum Expression?     
isSum :: ValExpr v -> Bool
isSum (view -> Vsum{}) = True
isSum _                = False

-- implementation details:
-- Properties incorporated
--    at most one value: the value is the sum of all values
--         special case if the sum is zero, no value is inserted since v == v+0
--    remove all nested sums, since (a+b) + (c+d) == (a+b+c+d)

         
-- | Apply operator sum on the provided sum of value expressions.
-- Preconditions are /not/ checked.
cstrSum :: Ord v => Sum (ValExpr v) -> ValExpr v
cstrSum ms = 
    let (adds, nonadds) = Sum.partition isSum ms in
            cstrSum' $ foldl Sum.sum nonadds (Sum.foldMultiplier toSumList [] adds)
    where                                                        
        toSumList :: ValExpr v -> Integer -> [Sum (ValExpr v)] -> [Sum (ValExpr v)]
        toSumList (view -> Vsum s) n  = (:) (Sum.multiply n s)
        toSumList _                _  = error ("ValExprImpls.hs - cstrSum - Unexpected ValExpr")
                    
-- Sum doesn't contain elements of type VExprSum
cstrSum' :: Ord v => Sum (ValExpr v) -> ValExpr v
cstrSum' ms = 
    let (vals, nonvals) = Sum.partition isConst ms in
        let sumVals = Sum.foldMultiplier addVal 0 vals in
            let retMS = case sumVals of
                        0   -> nonvals                                      -- 0 + x == x
                        _   -> Sum.add (cstrConst (Cint sumVals)) nonvals
                in
                    case Sum.toMultiplierList retMS of
                        []          -> cstrConst (Cint 0)                                -- sum of nothing equal zero
                        [(term,1)]  -> term
                        _           -> ValExpr (Vsum retMS)
    where                                                        
        addVal :: ValExpr v -> Integer -> Integer -> Integer
        addVal (view -> Vconst (Cint i)) n = (+) (i * n)
        addVal _ _                         = error ("ValExprImpls.hs - cstrSum' - Unexpected ValExpr")


cstrPredef :: PredefKind -> FuncId -> [ValExpr v] -> ValExpr v
cstrPredef p f a = ValExpr (Vpredef p f a)

cstrError :: Text -> ValExpr v
cstrError s = ValExpr (Verror s)

-- * Derived constructors

-- | Apply operator Or (\\\/) on the provided set of value expressions.
-- Preconditions are /not/ checked.
cstrOr :: (Ord v) => Set.Set (ValExpr v) -> ValExpr v
-- a \/ b == not (not a /\ not b)
cstrOr = cstrNot . cstrAnd . Set.map cstrNot

-- | Apply operator Implies (=>) on the provided value expressions.
-- Preconditions are /not/ checked.
cstrImplies :: (Ord v) => ValExpr v -> ValExpr v -> ValExpr v
-- a => b == not a \/ b == not (a /\ not b)
cstrImplies a b = (cstrNot . cstrAnd) (Set.insert a (Set.singleton (cstrNot b)))

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
