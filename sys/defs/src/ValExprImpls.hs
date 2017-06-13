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
import CstrId
import FuncId
import ConstDefs
import SortOf
import ValExprDefs
import Variable

-- ----------------------------------------------------------------------------------------- --
cstrFunc :: (Variable v) => FuncId -> [ValExpr v] -> ValExpr v
cstrFunc (FuncId "==" _ [sl, sr] s) [l,r] | sl == sr && s == sortId_Bool && sortOf l == sortOf r && sl == sortOf l  = cstrEqual l r             -- TODO: what should I check?
cstrFunc f a                                                                                                        = ValExpr (Vfunc f a)

cstrCstr :: CstrId -> [ValExpr v] -> ValExpr v
cstrCstr c a = ValExpr (Vcstr c a)

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