{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module ValExprImpls
( -- * Constructors to create Value Expressions
  -- ** Constant value
  cstrConst
  -- ** VarRef
, cstrVar
  -- ** General Operators to create Value Expressions
  -- *** Equal
, cstrEqual
  -- *** If Then Else
, cstrITE
  -- *** Function Call
, cstrFunc
  -- ** Boolean Operators to create Value Expressions
  -- *** Not
, cstrNot
  -- *** And
, cstrAnd
  -- ** Integer Operators to create Value Expressions
  -- *** Sum
, cstrSum
  -- *** Product
, cstrProduct
  -- *** Divide
, cstrDivide
  -- *** Modulo
, cstrModulo
  -- *** Comparisons GEZ
, cstrGEZ
  -- ** String Operators to create Value Expressions
  -- *** Length operator
, cstrLength
  -- *** At operator
, cstrAt
  -- *** Concat operator
, cstrConcat
  -- ** Regular Expression Operators to create Value Expressions
  -- *** String in Regular Expression operator
, cstrStrInRe
  -- ** Algebraic Data Type Operators to create Value Expressions
  -- *** Algebraic Data Type constructor operator
, cstrCstr
  -- *** Algebraic Data Type IsConstructor function
, cstrIsCstr
  -- *** Algebraic Data Type Accessor
, cstrAccess

-- to be documented
, cstrPredef
, cstrAny
, cstrError
-- * Substitution of var by value
, subst
, compSubst         -- changes type
)
where

import           Control.Arrow   (first)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>))
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Debug.Trace     as Trace
import           Text.Regex.TDFA

import           ConstDefs
import           CstrId
import qualified FreeMonoidX     as FMX
import           FuncDef
import           FuncId
import           Product
import           RegexXSD2Posix
import           SortId
import           Sum
import           ValExprDefs
import           Variable


cstrFunc :: (Variable v, Variable w) => Map.Map FuncId (FuncDef v) -> FuncId -> [ValExpr w] -> ValExpr w
cstrFunc fis fi arguments =
    case Map.lookup fi fis of
        Nothing ->  ValExpr (Vfunc fi arguments) -- When implementing the body of a recursive function, a function call is made while the implementation is not (yet) finished and available.
        Just (FuncDef params body)-> case view body of
                                        Vconst x -> cstrConst x
                                        _        -> if all isConst arguments
                                                        then compSubst (Map.fromList (zip params arguments)) fis body
                                                        else ValExpr (Vfunc fi arguments)


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

-- | Get the integer value of a constant.
getIntVal :: ValExpr v -> Integer
getIntVal (view -> Vconst (Cint i)) = i
getIntVal _                         =
    error "ValExprImpls.hs - getIntVal - Unexpected ValExpr: "

cstrConst :: Const -> ValExpr v
cstrConst c = ValExpr (Vconst c)

cstrVar :: v -> ValExpr v
cstrVar v = ValExpr (Vvar v)

-- | Apply operator ITE (IF THEN ELSE) on the provided value expressions.
-- Preconditions are /not/ checked.
cstrITE :: ValExpr v -> ValExpr v -> ValExpr v -> ValExpr v
cstrITE (view -> Vconst (Cbool True))  tb _ = tb
cstrITE (view -> Vconst (Cbool False)) _ fb = fb
-- if (not b) then tb else fb == if b then fb else tb
cstrITE (view -> Vnot n) tb fb              = ValExpr (Vite n fb tb)
cstrITE cs tb fb                            = ValExpr (Vite cs tb fb)

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

-- * Sum

-- | Is ValExpr a Sum Expression?
isSum :: ValExpr v -> Bool
isSum (view -> Vsum{}) = True
isSum _                = False

getSum :: ValExpr v -> FreeSum (ValExpr v)
getSum (view -> Vsum s) = s
getSum _ = error "ValExprImpls.hs - getSum - Unexpected ValExpr "

-- | Apply operator sum on the provided sum of value expressions.
-- Preconditions are /not/ checked.
cstrSum :: forall v . (Ord v, Integral (ValExpr v)) => FreeSum (ValExpr v) -> ValExpr v
-- implementation details:
-- Properties incorporated
--    at most one value: the value is the sum of all values
--         special case if the sum is zero, no value is inserted since v == v+0
--    remove all nested sums, since (a+b) + (c+d) == (a+b+c+d)
cstrSum ms =
    cstrSum' $ nonadds <> FMX.flatten sumOfAdds
    where
      (adds, nonadds) = FMX.partitionT isSum ms
      sumOfAdds :: FMX.FreeMonoidX (FMX.FreeMonoidX (SumTerm (ValExpr v)))
      sumOfAdds = FMX.mapTerms (getSum . summand) adds

-- Sum doesn't contain elements of type VExprSum
cstrSum' :: Ord v => FreeSum (ValExpr v) -> ValExpr v
cstrSum' ms =
    let (vals, nonvals) = FMX.partitionT isConst ms
        sumVals = summand $ FMX.foldFMX (FMX.mapTerms (SumTerm . getIntVal . summand) vals)
        retMS = case sumVals of
                    0 -> nonvals                                      -- 0 + x == x
                    _ -> Sum.add (cstrConst (Cint sumVals)) nonvals
    in
        case FMX.toOccurList retMS of
            []         -> cstrConst (Cint 0)                                -- sum of nothing equal zero
            [(term,1)] -> summand term
            _          -> ValExpr (Vsum retMS)

-- Product

-- | Is ValExpr a Product Expression?
isProduct :: ValExpr v -> Bool
isProduct (view -> Vproduct{}) = True
isProduct _                    = False

getProduct :: ValExpr v -> FreeProduct (ValExpr v)
getProduct (view -> Vproduct p) = p
getProduct _ = error "ValExprImpls.hs - getProduct - Unexpected ValExpr "

-- | Apply operator product on the provided product of value expressions.
-- Be aware that division is not associative for Integer, so only use power >= 0.
-- Preconditions are /not/ checked.
cstrProduct :: forall v .(Ord v, Integral (ValExpr v)) => FreeProduct (ValExpr v) -> ValExpr v
-- implementation details:
-- Properties incorporated
--    at most one value: the value is the product of all values
--         special case if the product is one, no value is inserted since v == v*1
--    remove all nested products, since (a*b) * (c*d) == (a*b*c*d)
cstrProduct ms =
    cstrProduct' $ noprods <> FMX.flatten prodOfProds
    where
      (prods, noprods) = FMX.partitionT isProduct ms
      prodOfProds :: FMX.FreeMonoidX (FMX.FreeMonoidX (ProductTerm (ValExpr v)))
      prodOfProds = FMX.mapTerms (getProduct . factor) prods
        -- TODO: canonical form -- make one sum of products (so remove all sums within all products)

-- Product doesn't contain elements of type VExprProduct
cstrProduct' :: (Ord v, Integral (ValExpr v))
             => FreeProduct (ValExpr v) -> ValExpr v
cstrProduct' ms =
    let (vals, nonvals) = FMX.partitionT isConst ms
        (zeros, _) = FMX.partitionT isZero vals
    in
        case FMX.nrofDistinctTerms zeros of
            0   ->  -- let productVals = Product.foldPower timesVal 1 vals in
                    let intProducts = FMX.mapTerms (getIntVal <$>) vals
                        productVals = factor (FMX.foldFMX intProducts)
                    in
                        case FMX.toDistinctAscOccurListT nonvals of
                            []          ->  cstrConst (Cint productVals)
                            [(term, 1)] ->  cstrSum (FMX.fromOccurList [(SumTerm term, productVals)])                           -- term can be Sum -> rewrite needed
                            _           ->  cstrSum (FMX.fromOccurList [(SumTerm (ValExpr (Vproduct nonvals)), productVals)])  -- productVals can be 1 -> rewrite possible
            _   ->  let (_, n) = Product.fraction zeros in
                        case FMX.nrofDistinctTerms n of
                            0   ->  cstrConst (Cint 0)      -- 0 * x == 0
                            _   ->  Trace.trace "Error in model: Division by Zero in Product (via negative power)" $
                                    ValExpr (Vproduct ms)
    where
        isZero :: ValExpr v -> Bool
        isZero (view -> Vconst (Cint 0)) = True
        isZero _                         = False

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

-- | Apply operator GEZ (Greater Equal Zero) on the provided value expression.
-- Preconditions are /not/ checked.
cstrGEZ :: ValExpr v -> ValExpr v
-- Simplification Values
cstrGEZ (view -> Vconst (Cint v)) = cstrConst (Cbool (0 <= v))
cstrGEZ (view -> Vlength _)       = cstrConst (Cbool True)        -- length of string is always Greater or equal to zero
cstrGEZ ve                        = ValExpr (Vgez ve)


-- | Apply operator Length on the provided value expression.
-- Preconditions are /not/ checked.
cstrLength :: ValExpr v -> ValExpr v
cstrLength (view -> Vconst (Cstring s)) = cstrConst (Cint (Prelude.toInteger (T.length s)))
cstrLength v                            = ValExpr (Vlength v)

-- | Apply operator At on the provided value expressions.
-- Preconditions are /not/ checked.
cstrAt :: ValExpr v -> ValExpr v -> ValExpr v
cstrAt ves@(view -> Vconst (Cstring s)) vei@(view -> Vconst (Cint i)) =
    if i < 0 || i >= Prelude.toInteger (T.length s)
        then Trace.trace ("Error in model: Accessing string " ++ show s ++ " of length " ++ show (T.length s) ++ " with illegal index "++ show i) $
             ValExpr (Vat ves vei)
        else cstrConst (Cstring (T.take 1 (T.drop (fromInteger i) s)))
cstrAt ves vei = ValExpr (Vat ves vei)

-- | Apply operator Concat on the provided sequence of value expressions.
-- Preconditions are /not/ checked.
cstrConcat :: (Eq v) => [ValExpr v] -> ValExpr v
cstrConcat l =
    let n = (mergeVals . flatten . filter (cstrConst (Cstring "") /= ) ) l in
        case Prelude.length n of
           0 -> cstrConst (Cstring "")
           1 -> head n
           _ -> ValExpr (Vconcat n)

-- implementation details:
-- Properties incorporated
--    "" ++ x == x          - remove empty strings
--    "a" ++ "b" == "ab"    - concat consecutive string values
--   remove all nested Concats, since (a ++ b) ++ (c ++ d) == (a ++ b ++ c ++ d)

mergeVals :: [ValExpr v] -> [ValExpr v]
mergeVals []            = []
mergeVals [x]           = [x]
mergeVals ( (view -> Vconst (Cstring s1)) : (view -> Vconst (Cstring s2)) : xs) =
                          mergeVals (cstrConst (Cstring (s1 <> s2)): xs)
mergeVals (x1:x2:xs)    = x1 : mergeVals (x2:xs)

flatten :: [ValExpr v] -> [ValExpr v]
flatten []                       = []
flatten ((view -> Vconcat l):xs) = l ++ flatten xs
flatten (x:xs)                   = x : flatten xs

-- | Apply String In Regular Expression operator on the provided value expressions.
-- Preconditions are /not/ checked.
cstrStrInRe :: ValExpr v -> ValExpr v -> ValExpr v
cstrStrInRe (view -> Vconst (Cstring s)) (view -> Vconst (Cregex r)) = cstrConst (Cbool (T.unpack s =~ T.unpack (xsd2posix r) ) )
cstrStrInRe s r                                                      = ValExpr (Vstrinre s r)

cstrPredef :: PredefKind -> FuncId -> [ValExpr v] -> ValExpr v
cstrPredef p f a = ValExpr (Vpredef p f a)

cstrAny :: SortId -> ValExpr v
cstrAny srt = ValExpr (Vany srt)

cstrError :: Text -> ValExpr v
cstrError s = ValExpr (Verror s)

-- | Substitute variables by values in value expression.
-- Preconditions are /not/ checked.
subst :: (Variable v, Integral (ValExpr v), Variable w, Integral (ValExpr w))
      => Map.Map v (ValExpr v) -> Map.Map FuncId (FuncDef w) -> ValExpr v -> ValExpr v
subst ve _ x   | ve == Map.empty = x
subst ve fis x = subst' ve fis (view x)

subst' :: (Variable v, Integral (ValExpr v), Variable w, Integral (ValExpr w))
       => Map.Map v (ValExpr v) -> Map.Map FuncId (FuncDef w) -> ValExprView v -> ValExpr v
subst' _  _   (Vconst const')          = cstrConst const'
subst' ve _   (Vvar vid)               = Map.findWithDefault (cstrVar vid) vid ve
subst' ve fis (Vfunc fid vexps)        = cstrFunc fis fid (map (subst' ve fis . view) vexps)
subst' ve fis (Vcstr cid vexps)        = cstrCstr cid (map (subst' ve fis . view) vexps)
subst' ve fis (Viscstr cid vexp)       = cstrIsCstr cid ( (subst' ve fis . view) vexp)
subst' ve fis (Vaccess cid p vexp)     = cstrAccess cid p ( (subst' ve fis . view) vexp)
subst' ve fis (Vite cond vexp1 vexp2)  = cstrITE ( (subst' ve fis . view) cond) ( (subst' ve fis . view) vexp1) ( (subst' ve fis . view) vexp2)
subst' ve fis (Vdivide t n)            = cstrDivide ( (subst' ve fis . view) t) ( (subst' ve fis . view) n)
subst' ve fis (Vmodulo t n)            = cstrModulo ( (subst' ve fis . view) t) ( (subst' ve fis . view) n)
subst' ve fis (Vgez v)                 = cstrGEZ ( (subst' ve fis . view) v)
subst' ve fis (Vsum s)                 = cstrSum $ FMX.fromOccurListT $ map (first (subst' ve fis . view)) $ FMX.toDistinctAscOccurListT s
subst' ve fis (Vproduct p)             = cstrProduct $ FMX.fromOccurListT $ map (first (subst' ve fis . view)) $ FMX.toDistinctAscOccurListT p
subst' ve fis (Vequal vexp1 vexp2)     = cstrEqual ( (subst' ve fis . view) vexp1) ( (subst' ve fis . view) vexp2)
subst' ve fis (Vand vexps)             = cstrAnd $ Set.map (subst' ve fis . view) vexps
subst' ve fis (Vnot vexp)              = cstrNot ( (subst' ve fis . view) vexp)
subst' ve fis (Vlength vexp)           = cstrLength ( (subst' ve fis . view) vexp)
subst' ve fis (Vat s p)                = cstrAt ( (subst' ve fis . view) s) ( (subst' ve fis . view) p)
subst' ve fis (Vconcat vexps)          = cstrConcat $ map (subst' ve fis . view) vexps
subst' ve fis (Vstrinre s r)           = cstrStrInRe ( (subst' ve fis . view) s) ( (subst' ve fis . view) r)
subst' ve fis (Vpredef kd fid vexps)   = cstrPredef kd fid (map (subst' ve fis . view) vexps)
subst' _  _   (Vany sId)               = cstrAny sId
subst' _  _   (Verror str)             = cstrError str

-- | Substitute variables by values in value expression (change variable kind).
-- Preconditions are /not/ checked.
compSubst :: (Variable v, Integral (ValExpr v), Variable w, Integral (ValExpr w))
          => Map.Map v (ValExpr w) -> Map.Map FuncId (FuncDef v) -> ValExpr v -> ValExpr w
-- compSubst ve _ _ | ve == Map.empty = cstrError "TXS Subst compSubst: variables must be substitute, yet varenv empty\n"
compSubst ve fis x                 = compSubst' ve fis (view x)

compSubst' :: (Variable v, Integral (ValExpr v), Variable w, Integral (ValExpr w))
           => Map.Map v (ValExpr w) -> Map.Map FuncId (FuncDef v) -> ValExprView v -> ValExpr w
compSubst' _  _   (Vconst const')          = cstrConst const'
compSubst' ve _   (Vvar vid)               = fromMaybe
                                                    (cstrError "TXS Subst compSubst: incomplete\n")
                                                    (Map.lookup vid ve)
compSubst' ve fis (Vfunc fid vexps)        = cstrFunc fis fid (map (compSubst' ve fis . view) vexps)
compSubst' ve fis (Vcstr cid vexps)        = cstrCstr cid (map (compSubst' ve fis . view) vexps)
compSubst' ve fis (Viscstr cid vexp)       = cstrIsCstr cid ( (compSubst' ve fis . view) vexp)
compSubst' ve fis (Vaccess cid p vexp)     = cstrAccess cid p ( (compSubst' ve fis . view) vexp)
compSubst' ve fis (Vite cond vexp1 vexp2)  = cstrITE ( (compSubst' ve fis . view) cond) ( (compSubst' ve fis . view) vexp1) ( (compSubst' ve fis . view) vexp2)
compSubst' ve fis (Vdivide t n)            = cstrDivide ( (compSubst' ve fis . view) t) ( (compSubst' ve fis . view) n)
compSubst' ve fis (Vmodulo t n)            = cstrModulo ( (compSubst' ve fis . view) t) ( (compSubst' ve fis . view) n)
compSubst' ve fis (Vgez v)                 = cstrGEZ ( (compSubst' ve fis . view) v)
compSubst' ve fis (Vsum s)                 = cstrSum $ FMX.fromOccurListT $ map (first (compSubst' ve fis . view)) $ FMX.toDistinctAscOccurListT s
compSubst' ve fis (Vproduct p)             = cstrProduct $ FMX.fromOccurListT $ map (first (compSubst' ve fis . view)) $ FMX.toDistinctAscOccurListT p
compSubst' ve fis (Vequal vexp1 vexp2)     = cstrEqual ( (compSubst' ve fis . view) vexp1) ( (compSubst' ve fis . view) vexp2)
compSubst' ve fis (Vand vexps)             = cstrAnd $ Set.map (compSubst' ve fis . view) vexps
compSubst' ve fis (Vnot vexp)              = cstrNot ( (compSubst' ve fis . view) vexp)
compSubst' ve fis (Vlength vexp)           = cstrLength ( (compSubst' ve fis . view) vexp)
compSubst' ve fis (Vat s p)                = cstrAt ( (compSubst' ve fis . view) s) ( (compSubst' ve fis . view) p)
compSubst' ve fis (Vconcat vexps)          = cstrConcat $ map (compSubst' ve fis . view) vexps
compSubst' ve fis (Vstrinre s r)           = cstrStrInRe ( (compSubst' ve fis . view) s) ( (compSubst' ve fis . view) r)
compSubst' ve fis (Vpredef kd fid vexps)   = cstrPredef kd fid (map (compSubst' ve fis . view) vexps)
compSubst' _  _   (Vany sId)               = cstrAny sId
compSubst' _  _   (Verror str)             = cstrError str

-- ----------------------------------------------------------------------------------------- --

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
