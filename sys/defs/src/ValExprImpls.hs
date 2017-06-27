{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE ViewPatterns #-}
module ValExprImpls
where
-- ----------------------------------------------------------------------------------------- --
import Debug.Trace as Trace

import CstrId
import FuncId
import ConstDefs
import SortOf
import ValExprDefs
import Variable

-- ----------------------------------------------------------------------------------------- --
cstrFunc :: (Variable v) => FuncId -> [ValExpr v] -> ValExpr v
cstrFunc f a                                                                                                        = ValExpr (Vfunc f a)

cstrCstr :: CstrId -> [ValExpr v] -> ValExpr v
cstrCstr c a = ValExpr (Vcstr c a)

cstrIsCstr :: CstrId -> ValExpr v -> ValExpr v
cstrIsCstr c1 (view -> Vcstr c2 _)          = cstrConst (Cbool (c1 == c2) )
cstrIsCstr c1 (view -> Vconst (Cstr c2 _))  = cstrConst (Cbool (c1 == c2) )
cstrIsCstr c e = ValExpr (Viscstr c e)

-- | Apply ADT Accessor of constructor with constructorName on field with fieldName on the provided value expression.
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

cstrConst :: Const -> ValExpr v
cstrConst c = ValExpr (Vconst c)

cstrVar :: v -> ValExpr v
cstrVar v = ValExpr (Vvar v)

cstrIte :: [ValExpr v] -> ValExpr v -> ValExpr v -> ValExpr v
cstrIte cs tb fb = ValExpr (Vite cs tb fb)

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
-- a == b <==> b == a -- same representation
cstrEqual ve1 ve2                                   = if ve1 <= ve2
                                                        then ValExpr (Vequal ve1 ve2)
                                                        else ValExpr (Vequal ve2 ve1)

cstrPredef :: PredefKind -> FuncId -> [ValExpr v] -> ValExpr v
cstrPredef p f a = ValExpr (Vpredef p f a)

cstrError :: String -> ValExpr v
cstrError s = ValExpr (Verror s)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --