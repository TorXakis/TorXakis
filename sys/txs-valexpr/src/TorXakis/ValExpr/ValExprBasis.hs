{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprBasis
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Smart constructors, Substitutions, and Context for Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
module TorXakis.ValExpr.ValExprBasis
( -- ** Constructors to create Value Expression
  -- *** Constant Value
  mkConst
  -- *** Variable
, mkVar
  -- *** General Operators to create Value Expressions
  -- **** Equal
, mkEqual
  -- **** If Then Else
, mkITE
  -- **** Function Call
, mkFunc
  -- **** PredefNonSolvableined Function Call
, mkPredefNonSolvable
  -- *** Boolean Operators to create Value Expressions
  -- **** Not
, mkNot
  -- **** And
, mkAnd
  -- *** Integer Operators to create Value Expressions
  -- **** Unary Minus
, mkUnaryMinus
  -- **** Divide
, mkDivide
  -- **** Modulo
, mkModulo
  -- **** Sum
, mkSum
  -- **** Product
, mkProduct
  -- **** Comparison
, mkGEZ
  -- *** String Operators to create Value Expressions
  -- **** Length operator
, mkLength
  -- **** At operator
, mkAt
  -- **** Concat operator
, mkConcat
  -- *** Regular Expression Operators to create Value Expressions
  -- **** String in Regular Expression operator
, mkStrInRe
  -- *** Algebraic Data Type Operators to create Value Expressions
  -- **** Algebraic Data Type constructor operator
, mkCstr
  -- **** Algebraic Data Type IsConstructor function
, mkIsCstr
  -- **** Algebraic Data Type Accessor
, mkAccess
  -- ** Substitution of variables
  -- *** Partial Substitution
, partSubst
  -- *** Complete Substitution
, compSubst
  -- ** Context
  -- *** ValExpr Context
, ValExprContext (..)
  -- *** Minimal ValExpr Context
, MinimalValExprContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import           Data.Either
import qualified Data.HashMap    as HashMap
import qualified Data.Map        as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           GHC.Generics           (Generic)
import           Text.Regex.TDFA

import           TorXakis.Error
import           TorXakis.FreeVars
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Value
import           TorXakis.ValExpr.RegexXSD2Posix
import           TorXakis.ValExpr.ValExpr
import           TorXakis.VarDef

------------------------------------------------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------------------------------------------------

trueValExpr :: ValExpr v
trueValExpr = ValExpr $ Vconst (Cbool True)

falseValExpr :: ValExpr v
falseValExpr = ValExpr $ Vconst (Cbool False)

stringEmptyValExpr :: ValExpr v
stringEmptyValExpr = ValExpr $ Vconst (Cstring (T.pack ""))

zeroValExpr :: ValExpr v
zeroValExpr = ValExpr $ Vconst (Cint 0)

-- | Create a constant value as a value expression.
mkConst :: ValExprContext c v => c v -> Value -> Either MinError (ValExpr v)
mkConst ctx v = if elemSort ctx (getSort v)
                    then unsafeConst v
                    else Left $  MinError (T.pack ("Sort " ++ show (getSort v) ++ " not defined in context"))

unsafeConst :: Value -> Either MinError (ValExpr v)
unsafeConst = Right . ValExpr . Vconst

-- | Create a variable as a value expression.
mkVar :: c v -> v -> Either MinError (ValExpr v)
mkVar _ = unsafeVar

unsafeVar :: v -> Either MinError (ValExpr v)
unsafeVar = Right . ValExpr . Vvar

-- | Apply operator Equal on the provided value expressions.
mkEqual :: ValExprContext c v => c v -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkEqual _   ve1 ve2 | getSort ve1 /= getSort ve2 = Left $ MinError (T.pack ("Sort of value expressions in equal differ " ++ show (getSort ve1) ++ " versus " ++ show (getSort ve2)))
mkEqual ctx ve1 ve2 | elemSort ctx (getSort ve1) = unsafeEqual ve1 ve2
mkEqual _   ve1 _                                = Left $  MinError (T.pack ("Sort " ++ show (getSort ve1) ++ " not defined in context"))

unsafeEqual :: Ord v => ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
-- Simplification: a == a <==> True
unsafeEqual ve1 ve2 | ve1 == ve2                    = Right trueValExpr
-- Simplification: Different Values <==> False : use Same Values are already detected in previous step
unsafeEqual (TorXakis.ValExpr.ValExpr.view -> Vconst {}) (TorXakis.ValExpr.ValExpr.view -> Vconst {}) = Right falseValExpr
-- Simplification: True == e <==> e (twice)
unsafeEqual b e | b == trueValExpr                = Right e
unsafeEqual e b | b == trueValExpr                = Right e
-- Simplification: False == e <==> not e (twice)
unsafeEqual b e | b == falseValExpr               = unsafeNot e
unsafeEqual e b | b == falseValExpr               = unsafeNot e
-- Simplification: Not x == x <==> false (twice)
unsafeEqual e (TorXakis.ValExpr.ValExpr.view -> Vnot n) | e == n             = Right falseValExpr
unsafeEqual (TorXakis.ValExpr.ValExpr.view -> Vnot n) e | e == n             = Right falseValExpr
-- Simplification: Not x == Not y <==> x == y
unsafeEqual (TorXakis.ValExpr.ValExpr.view -> Vnot n1) (TorXakis.ValExpr.ValExpr.view -> Vnot n2)     = unsafeEqual n1 n2
-- Same representation: Not a == b <==> a == Not b (twice)
unsafeEqual x@(TorXakis.ValExpr.ValExpr.view -> Vnot n) e = if n <= e
                                                            then Right $ ValExpr (Vequal x e)
                                                            else case unsafeNot e of
                                                                    Left m  -> error ("Unexpected error in Equal: " ++ show m)
                                                                    Right m -> Right $ ValExpr (Vequal m n)
unsafeEqual e x@(TorXakis.ValExpr.ValExpr.view -> Vnot n) = if n <= e
                                                            then Right $ ValExpr (Vequal x e)
                                                            else case unsafeNot e of
                                                                    Left m  -> error ("Unexpected error in Equal: " ++ show m)
                                                                    Right m -> Right $ ValExpr (Vequal m n)
-- Same representation: a == b <==> b == a
unsafeEqual ve1 ve2                                 = if ve1 <= ve2
                                                            then Right $ ValExpr (Vequal ve1 ve2)
                                                            else Right $ ValExpr (Vequal ve2 ve1)

-- | Apply operator ITE (IF THEN ELSE) on the provided value expressions.
mkITE :: ValExprContext c v => c v -> ValExpr v -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkITE _   b _  _  | getSort b  /= SortBool    = Left $ MinError (T.pack ("Condition of ITE is not of expected sort Bool but " ++ show (getSort b)))
mkITE _   _ tb fb | getSort tb /= getSort fb  = Left $ MinError (T.pack ("Sorts of branches differ " ++ show (getSort tb) ++ " versus " ++ show (getSort fb)))
mkITE ctx b tb fb | elemSort ctx (getSort tb) = unsafeITE b tb fb
mkITE _   _ tb _                              = Left $  MinError (T.pack ("Sort " ++ show (getSort tb) ++ " not defined in context"))

unsafeITE :: Eq v => ValExpr v -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
-- Simplification: if True then a else b <==> a
unsafeITE b tb _ | b == trueValExpr         = Right tb
-- Simplification: if False then a else b <==> b
unsafeITE b _ fb | b == falseValExpr        = Right fb
-- Simplification: if q then p else False fi <==> q /\ p : Note: p is boolean expression (otherwise different sorts in branches) 
-- Not implemented to enable conditional evaluation
-- Simplification: if c then a else a <==> a
unsafeITE _ tb fb | tb == fb                  = Right tb
-- Simplification: if (not c) then tb else fb <==> if c then fb else tb
unsafeITE (TorXakis.ValExpr.ValExpr.view -> Vnot n) tb fb              = Right $ ValExpr (Vite n fb tb)
-- Simplification: if c then True else False <==> c
unsafeITE b tb fb | tb == trueValExpr && fb == falseValExpr = Right b
-- Simplification: if c then False else True <==> not c
unsafeITE b tb fb | tb == falseValExpr && fb == trueValExpr = unsafeNot b
unsafeITE cs tb fb                            = Right $ ValExpr (Vite cs tb fb)

-- | Create a function call.
mkFunc :: ValExprContext c v => c v -> FuncSignature -> [ValExpr v] -> Either MinError (ValExpr v)
mkFunc ctx fs vs 
    | expected == actual = unsafeFunc ctx fs vs
    | otherwise          = Left $ MinError (T.pack ("Sorts of signature and arguments differ: " ++ show (zip expected actual) ) )
        where
            expected = args fs
            actual = map getSort vs

unsafeFunc :: (ValExprContext c v, VarDef w) => c v -> FuncSignature -> [ValExpr w] -> Either MinError (ValExpr w)
unsafeFunc ctx fs vs = case HashMap.lookup fs (funcDefs ctx) of
                            Nothing -> -- Allow creating a function call to a function that is being defined (e.g. a recursive function).
                                       -- When a new function is added to the context, all function calls will be checked to refer to existing functions.
                                       Right $ ValExpr (Vfunc fs vs)
                            Just fd -> let vw = TorXakis.FuncDef.view fd in
                                         case body vw of
                                            (TorXakis.ValExpr.ValExpr.view -> Vconst x)  -> unsafeConst x
                                            _                                            -> case toMaybeValues vs of
                                                                        Just _  -> compSubst ctx 
                                                                                             (HashMap.fromList (zip (paramDefs vw) vs))
                                                                                             (body vw)
                                                                        Nothing -> Right $ ValExpr (Vfunc fs vs)

-- | Make a call to some predefined functions
-- Only allowed in CNECTDEF
mkPredefNonSolvable :: ValExprContext c v => c v -> FuncSignature -> [ValExpr v] -> Either MinError (ValExpr v)
mkPredefNonSolvable ctx fs vs
    | not isPredefNonSolvableFunction  = Left $ MinError (T.pack ("Signature is not of a predefined function: " ++ show fs))
    | expected == actual    = unsafePredefNonSolvable ctx fs vs
    | otherwise             = Left $ MinError (T.pack ("Sorts of signature and arguments differ: " ++ show (zip expected actual) ) )
        where
            expected = args fs
            actual = map getSort vs
            isPredefNonSolvableFunction =
                case (T.unpack (TorXakis.Name.toText (TorXakis.FuncSignature.funcName fs)), returnSort fs, args fs) of
                    ("toString",     SortString, [v])                      -> elemSort ctx v
                    ("fromString",   v,          [SortString])             -> elemSort ctx v
                    ("toXML",        SortString, [v])                      -> elemSort ctx v
                    ("fromXML",      v,          [SortString])             -> elemSort ctx v
                    ("takeWhile",    SortString, [SortString, SortString]) -> True
                    ("takeWhileNot", SortString, [SortString, SortString]) -> True
                    ("dropWhile",    SortString, [SortString, SortString]) -> True
                    ("dropWhileNot", SortString, [SortString, SortString]) -> True
                    _                                                      -> False
    
unsafePredefNonSolvable :: SortContext c => c -> FuncSignature -> [ValExpr v] -> Either MinError (ValExpr v)
unsafePredefNonSolvable ctx fs vs = case toMaybeValues vs of
                                        Just values -> evalPredefNonSolvable ctx fs values
                                        Nothing     -> Right $ ValExpr (Vpredef fs vs)

evalPredefNonSolvable :: SortContext c => c -> FuncSignature -> [Value] -> Either MinError (ValExpr v)
evalPredefNonSolvable ctx fs vs =
    case (T.unpack (TorXakis.Name.toText (TorXakis.FuncSignature.funcName fs)), returnSort fs, vs) of
            ("toString",     SortString, [v])                      -> unsafeConst $ Cstring (valueToText ctx v)
            ("fromString",   s,          [Cstring t])              -> valueFromText ctx s t >>= unsafeConst
            ("toXML",        SortString, [v])                      -> unsafeConst $ Cstring (valueToXML ctx v)
            ("fromXML",      s,          [Cstring t])              -> valueFromXML ctx s t >>= unsafeConst
            ("takeWhile",    SortString, [Cstring v1, Cstring v2]) -> unsafeConst $ Cstring (T.takeWhile (`elemT` v1) v2)
            ("takeWhileNot", SortString, [Cstring v1, Cstring v2]) -> unsafeConst $ Cstring (T.takeWhile (`notElemT` v1) v2)
            ("dropWhile",    SortString, [Cstring v1, Cstring v2]) -> unsafeConst $ Cstring (T.dropWhile (`elemT` v1) v2)
            ("dropWhileNot", SortString, [Cstring v1, Cstring v2]) -> unsafeConst $ Cstring (T.dropWhile (`notElemT` v1) v2)
            _                                                      -> error ("Unknown predefined function: " ++ show fs)
    where
        elemT :: Char -> T.Text -> Bool
        elemT c = isJust . T.find (== c)

        notElemT :: Char -> T.Text -> Bool
        notElemT c = not . elemT c

-- | Apply operator Not on the provided value expression.
mkNot :: ValExprContext c v => c v -> ValExpr v -> Either MinError (ValExpr v)
mkNot _ n | getSort n == SortBool = unsafeNot n
mkNot _ n                         = Left $ MinError (T.pack ("Argument of Not is not of expected sort Bool but " ++ show (getSort n)))

unsafeNot :: Eq v => ValExpr v -> Either MinError (ValExpr v)
-- Simplification: not True <==> False
unsafeNot b | b == trueValExpr                              = Right falseValExpr
-- Simplification: not False <==> True
unsafeNot b | b == falseValExpr                             = Right trueValExpr
-- Simplification: not (not x) <==> x
unsafeNot (TorXakis.ValExpr.ValExpr.view -> Vnot ve)        = Right ve
-- Simplification: not (if cs then tb else fb) <==> if cs then not (tb) else not (fb)
unsafeNot (TorXakis.ValExpr.ValExpr.view -> Vite cs tb fb)  = case (unsafeNot tb, unsafeNot fb) of
                                                                (Right nt, Right nf) -> Right $ ValExpr (Vite cs nt nf)
                                                                _                    -> error "Unexpected error in NOT with ITE"
unsafeNot ve                                                = Right $ ValExpr (Vnot ve)

-- | Apply operator And on the provided list of value expressions.
-- TODO: generalize to Traversable instead of list?
mkAnd :: ValExprContext c v => c v -> [ValExpr v] -> Either MinError (ValExpr v)
mkAnd _ s | all (\e -> SortBool == getSort e) s = unsafeAnd (Set.fromList s)
mkAnd _ _                                       = Left $ MinError (T.pack "Not all value expressions in set are of expected sort Bool")

unsafeAnd :: Ord v => Set.Set (ValExpr v) -> Either MinError (ValExpr v)
unsafeAnd = unsafeAnd' . flattenAnd
    where
        flattenAnd :: Ord v => Set.Set (ValExpr v) -> Set.Set (ValExpr v)
        flattenAnd = Set.unions . map fromValExpr . Set.toList
        
        fromValExpr :: ValExpr v -> Set.Set (ValExpr v)
        fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vand a) = a
        fromValExpr x                                         = Set.singleton x

-- And doesn't contain elements of type Vand.
unsafeAnd' :: forall v . Ord v => Set.Set (ValExpr v) -> Either MinError (ValExpr v)
unsafeAnd' s =
    -- Canonical form (merge conditional IFs)
    -- IF a THEN b ELSE False FI /\ IF c THEN d ELSE False FI <==> IF a /\ c THEN b /\ d ELSE False FI
        mergeConditionals s >>= (\can ->
        let s' :: Set.Set (ValExpr v)
            s' = Set.delete trueValExpr can in
                if Set.member falseValExpr s'
                    then Right falseValExpr
                    else case Set.toList s' of
                            []  -> Right trueValExpr
                            [h] -> Right h
                            _   -> -- Simplification: not(x) and x <==> False
                                   let nots = filterNot (Set.toList s') in
                                        if any (contains s') nots
                                            then Right falseValExpr
                                            else Right $ ValExpr (Vand s')
        )
    where
        mergeConditionals :: Set.Set (ValExpr v) -> Either MinError (Set.Set (ValExpr v))
        mergeConditionals s' = Set.foldr mergeConditional (Right (Set.empty, Set.empty, Set.empty)) s' >>= 
                                (\(s'', cs, ds) -> case Set.toList cs of
                                                    (_:_:_) -> -- at least two items to merge
                                                                unsafeAnd cs >>=
                                                                (\acs -> unsafeAnd ds >>=
                                                                (\ads -> unsafeITE acs ads falseValExpr >>= 
                                                                (\ite -> Right $ Set.insert ite s'')))
                                                    _       -> -- nothing to merge
                                                                Right s'
                                )

        mergeConditional :: ValExpr v 
                         -> Either MinError (Set.Set (ValExpr v), Set.Set (ValExpr v), Set.Set (ValExpr v))
                         -> Either MinError (Set.Set (ValExpr v), Set.Set (ValExpr v), Set.Set (ValExpr v))
        mergeConditional _                                               (Left e)                                  = Left e
        mergeConditional (TorXakis.ValExpr.ValExpr.view -> Vite c tb fb) (Right (s', cs, ds)) | tb == falseValExpr = unsafeNot c >>= (\nc -> Right (s', Set.insert nc cs, Set.insert fb ds))
        mergeConditional (TorXakis.ValExpr.ValExpr.view -> Vite c tb fb) (Right (s', cs, ds)) | fb == falseValExpr = Right (s', Set.insert c cs, Set.insert tb ds)
        mergeConditional x                                               (Right (s', cs, ds))                      = Right (Set.insert x s', cs, ds)

        filterNot :: [ValExpr v] -> [ValExpr v]
        filterNot [] = []
        filterNot (x:xs) = case TorXakis.ValExpr.ValExpr.view x of
                            Vnot n -> n : filterNot xs
                            _      ->     filterNot xs

        contains :: Set.Set (ValExpr v) -> ValExpr v -> Bool
        contains set (TorXakis.ValExpr.ValExpr.view -> Vand a) = all (`Set.member` set) (Set.toList a)
        contains set a                                         = Set.member a set

-- | Apply unary operator Minus on the provided value expression.
mkUnaryMinus :: ValExprContext c v => c v -> ValExpr v -> Either MinError (ValExpr v)
mkUnaryMinus _ v | getSort v == SortInt = unsafeUnaryMinus v
mkUnaryMinus _ v                        = Left $ MinError (T.pack ("Unary Minus argument not of expected Sort Int but " ++ show (getSort v)))

unsafeUnaryMinus :: Ord v => ValExpr v -> Either MinError (ValExpr v)
unsafeUnaryMinus (TorXakis.ValExpr.ValExpr.view -> Vsum m) = unsafeSumFromMap (Map.map (* (-1)) m)
unsafeUnaryMinus x                                         = unsafeSumFromMap (Map.singleton x (-1))

-- | Apply operator Divide on the provided value expressions.
mkDivide :: ValExprContext c v => c v -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkDivide _ d _ | getSort d /= SortInt = Left $ MinError (T.pack ("Dividend not of expected Sort Int but " ++ show (getSort d)))
mkDivide _ _ d | getSort d /= SortInt = Left $ MinError (T.pack ("Divisor not of expected Sort Int but " ++ show (getSort d)))
mkDivide _ t n                        = unsafeDivide t n

unsafeDivide :: ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
unsafeDivide _                                                   (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint n)) | n == 0 = Left $ MinError (T.pack "Divisor equal to zero in Divide")
unsafeDivide (TorXakis.ValExpr.ValExpr.view ->  Vconst (Cint t)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint n))          = unsafeConst (Cint (t `div` n) )
unsafeDivide vet                                                 ven                                                         = Right $ ValExpr (Vdivide vet ven)

-- | Apply operator Modulo on the provided value expressions.
mkModulo :: ValExprContext c v => c v -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkModulo _ d _ | getSort d /= SortInt = Left $ MinError (T.pack ("Dividend not of expected Sort Int but " ++ show (getSort d)))
mkModulo _ _ d | getSort d /= SortInt = Left $ MinError (T.pack ("Divisor not of expected Sort Int but " ++ show (getSort d)))
mkModulo _ t n                        = unsafeModulo t n

unsafeModulo :: ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
unsafeModulo _                                                   (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint n)) | n == 0 = Left $ MinError (T.pack "Divisor equal to zero in Modulo")
unsafeModulo (TorXakis.ValExpr.ValExpr.view ->  Vconst (Cint t)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint n))          = unsafeConst (Cint (t `mod` n) )
unsafeModulo vet                                                 ven                                                         = Right $ ValExpr (Vmodulo vet ven)

-- is key a constant?
isKeyConst :: ValExpr v -> Integer -> Bool
isKeyConst (TorXakis.ValExpr.ValExpr.view -> Vconst{}) _ = True
isKeyConst _                                           _ = False

-- | Apply operator sum on the provided list of value expressions.
mkSum :: ValExprContext c v => c v -> [ValExpr v] -> Either MinError (ValExpr v)
mkSum _ l | all (\e -> SortInt == getSort e) l = unsafeSum l
mkSum _ _                                      = Left $ MinError (T.pack "Not all value expressions in list are of expected sort Int")
-- Note: inverse of addition (subtraction) is communicative -> occurrence can also be negativeonly
-- Simplification incorporated:
-- 1. remove all nested sums, since (a+b) + (c+d) == (a+b+c+d)
--    remove duplicates (by using occurrence) -- we use communicative property of sum
-- 2. at most one value: the value is the sum of all values
--    special case if the sum is zero, no value is inserted since v == v+0
-- 3. When map is empty return 0
--    When map contains a single element exactly once, return that element
unsafeSum :: Ord v => [ValExpr v] -> Either MinError (ValExpr v)
unsafeSum = unsafeSumFromMap . flattenSum
    where
        flattenSum :: Ord v => [ValExpr v] -> Map.Map (ValExpr v) Integer
        flattenSum = Map.filter (0 ==) . Map.unionsWith (+) . map fromValExpr       -- combine maps (duplicates should be counted) and remove elements with occurrence 0
        
        fromValExpr :: ValExpr v -> Map.Map (ValExpr v) Integer
        fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vsum m) = m
        fromValExpr x                                         = Map.singleton x 1

-- unsafeSumFromMap doesn't contain elements of type Vsum.
unsafeSumFromMap :: forall v . Ord v => Map.Map (ValExpr v) Integer -> Either MinError (ValExpr v)
unsafeSumFromMap m = 
    let (vals, nonvals) = Map.partitionWithKey isKeyConst m
        retVal :: Map.Map (ValExpr v) Integer
        retVal = case sum (map toValue (Map.toList vals)) of
                    0   -> nonvals
                    val -> case unsafeConst (Cint val) of
                                Right x -> Map.insert x 1 nonvals
                                Left _  -> error "Unexpected failure in unsafeConst in unsafeSum"
      in
        case Map.toList retVal of
            []          -> Right zeroValExpr     -- sum of nothing equal to zero
            [(term, 1)] -> Right term
            _           -> Right (ValExpr (Vsum m))
    where
        toValue :: (ValExpr v, Integer) -> Integer
        toValue (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i), o) = i * o
        toValue (_                                               , _) = error "Unexpected value expression (expecting const of integer type) in toValue of unsafeSum"

-- | Apply operator product on the provided list of value expressions.
mkProduct :: ValExprContext c v => c v -> [ValExpr v] -> Either MinError (ValExpr v)
mkProduct _ l | all (\e -> SortInt == getSort e) l = unsafeProduct l
mkProduct _ _                                      = Left $ MinError (T.pack "Not all value expressions in list are of expected sort Int")
-- Note: inverse of product (division) is not communicative -> occurrence can only be positive
-- Simplification incorporated:
-- 1. remove all nested products, since (a*b) * (c*d) == (a*b*c*d)
--    remove duplicates (by using occurrence) -- we use communicative property of product
-- 2. at most one value: the value is the product of all values
--    special case if the product is zero, value is zero
-- 3. When non values are empty return value
--    When non-values are not empty, return sum which multiplies value with non-values
unsafeProduct :: Ord v => [ValExpr v] -> Either MinError (ValExpr v)
unsafeProduct = unsafeProductFromMap  . flattenProduct
    where
        flattenProduct :: Ord v => [ValExpr v] -> Map.Map (ValExpr v) Integer
        flattenProduct = Map.unionsWith (+) . map fromValExpr       -- combine maps (duplicates should be counted)
        
        fromValExpr :: ValExpr v -> Map.Map (ValExpr v) Integer
        fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vproduct m) = m
        fromValExpr x                                             = Map.singleton x 1

-- Flatten Product doesn't contain elements of type Vproduct.
unsafeProductFromMap  :: forall v . Ord v => Map.Map (ValExpr v) Integer -> Either MinError (ValExpr v)
-- Simplification: 0 * x = 0
unsafeProductFromMap  m | Map.member zeroValExpr m = Right zeroValExpr
unsafeProductFromMap  m =
    let (vals, nonvals) = Map.partitionWithKey isKeyConst m
        value = product (map toValue (Map.toList vals))
      in
        case Map.toList nonvals of
            []         -> unsafeConst (Cint value)
            [(term,1)] -> unsafeSumFromMap $ Map.singleton term value
            _          -> unsafeSumFromMap $ Map.singleton (ValExpr (Vproduct nonvals)) value
    where
        toValue :: (ValExpr v, Integer) -> Integer
        toValue (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i), o) = i ^ o
        toValue (_                                               , _) = error "Unexpected value expression (expecting const of integer type) in toValue of unsafeProduct"

-- | Apply operator GEZ (Greater Equal Zero) on the provided value expression.
mkGEZ :: ValExprContext c v => c v -> ValExpr v -> Either MinError (ValExpr v)
mkGEZ _ d | getSort d /= SortInt = Left $ MinError (T.pack ("Argument of GEZ not of expected Sort Int but " ++ show (getSort d)))
mkGEZ _ d                        = unsafeGEZ d

unsafeGEZ :: ValExpr v -> Either MinError (ValExpr v)
-- Simplification: Integer Value to Boolean
unsafeGEZ (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint v)) = unsafeConst (Cbool (0 <= v))
-- Simplification: length of string is always Greater or equal to zero
unsafeGEZ (TorXakis.ValExpr.ValExpr.view -> Vlength {})      = Right trueValExpr
unsafeGEZ ve                                                 = Right $ ValExpr (Vgez ve)

-- | Apply operator Length on the provided value expression.
mkLength :: ValExprContext c v => c v -> ValExpr v -> Either MinError (ValExpr v)
mkLength _ s | getSort s /= SortString = Left $ MinError (T.pack ("Argument of Length not of expected Sort String but " ++ show (getSort s)))
mkLength _ s                           = unsafeLength s

unsafeLength :: ValExpr v -> Either MinError (ValExpr v)
unsafeLength (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) = unsafeConst (Cint (Prelude.toInteger (T.length s)))
unsafeLength v                                                     = Right $ ValExpr (Vlength v)

-- | Apply operator At on the provided value expressions.
mkAt :: ValExprContext c v => c v -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkAt _ s i | getSort s /= SortString = Left $ MinError (T.pack ("First argument of At not of expected Sort String but " ++ show (getSort s)))
           | getSort i /= SortInt    = Left $ MinError (T.pack ("Second argument of At not of expected Sort Int but " ++ show (getSort i)))
           | otherwise               = unsafeAt s i

unsafeAt :: (Eq v) => ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
unsafeAt _                                                     (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i)) | i < 0                                 = Right stringEmptyValExpr
unsafeAt (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i)) | i >= Prelude.toInteger (T.length s)   = Right stringEmptyValExpr
unsafeAt (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i))                                         = unsafeConst (Cstring (T.take 1 (T.drop (fromInteger i) s)))    -- s !! i for Text
unsafeAt (TorXakis.ValExpr.ValExpr.view -> Vconcat ((TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)):xs)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i)) =
    let lengthS = Prelude.toInteger (T.length s) in
        if i < lengthS 
            then unsafeConst (Cstring (T.take 1 (T.drop (fromInteger i) s)))
            else unsafeConcat xs >>= (\nc -> 
                    unsafeConst (Cint (i - lengthS)) >>=
                        unsafeAt nc)
unsafeAt ves vei = Right $ ValExpr (Vat ves vei)

-- | Apply operator Concat on the provided sequence of value expressions.
mkConcat :: ValExprContext c v => c v -> [ValExpr v] -> Either MinError (ValExpr v)
mkConcat _ l | all (\e -> SortString == getSort e) l = unsafeConcat l
mkConcat _ _                                         = Left $ MinError (T.pack "Not all value expressions in list are of expected sort String")

unsafeConcat :: (Eq v) => [ValExpr v] -> Either MinError (ValExpr v)
unsafeConcat l =
    case (mergeVals . flatten . filter (stringEmptyValExpr /= ) ) l of
        []  -> Right stringEmptyValExpr
        [e] -> Right e
        cs  -> Right $ ValExpr (Vconcat cs)
  where
    -- implementation details:
    -- Properties incorporated
    --    "" ++ x == x          - remove empty strings
    --    "a" ++ "b" == "ab"    - concat consecutive string values
    --   remove all nested Concats, since (a ++ b) ++ (c ++ d) == (a ++ b ++ c ++ d)

    mergeVals :: [ValExpr v] -> [ValExpr v]
    mergeVals []            = []
    mergeVals [x]           = [x]
    mergeVals ( (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s1)) 
              : (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s2)) 
              : xs)         = case unsafeConst (Cstring (T.append s1 s2)) of
                                    Right x -> mergeVals (x:xs)
                                    Left e  -> error ("Unexpected error in mergeVals of Concat" ++ show e)
    mergeVals (x1:x2:xs)    = x1 : mergeVals (x2:xs)

    flatten :: [ValExpr v] -> [ValExpr v]
    flatten = concatMap fromValExpr

    fromValExpr :: ValExpr v -> [ValExpr v]
    fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vconcat cs) = cs
    fromValExpr x                                             = [x]

-- | Apply String In Regular Expression operator on the provided value expressions.
mkStrInRe :: ValExprContext c v => c v -> ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
mkStrInRe _ s _ | getSort s /= SortString = Left $ MinError (T.pack ("First argument of At not of expected Sort String but " ++ show (getSort s)))
mkStrInRe _ _ r | getSort r /= SortRegex  = Left $ MinError (T.pack ("Second argument of At not of expected Sort Regex but " ++ show (getSort r)))
mkStrInRe _ s r                           = unsafeStrInRe s r

unsafeStrInRe :: ValExpr v -> ValExpr v -> Either MinError (ValExpr v)
unsafeStrInRe (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) 
              (TorXakis.ValExpr.ValExpr.view -> Vconst (Cregex r))  = unsafeConst (Cbool (T.unpack s =~ T.unpack (xsd2posix r) ) )
unsafeStrInRe s r                                                   = Right $ ValExpr (Vstrinre s r)

-- get ConstructorDef when possible
getCstr :: SortContext c => c -> RefByName ADTDef -> RefByName ConstructorDef -> Either MinError (ADTDef, ConstructorDef)
getCstr ctx aName cName = case HashMap.lookup aName (adtDefs ctx) of
                                Nothing   -> Left $ MinError (T.pack ("ADTDefinition " ++ show aName ++ " not defined in context"))
                                Just aDef -> case HashMap.lookup cName ( (constructors . viewADTDef) aDef) of
                                                Nothing   -> Left $ MinError (T.pack ("Constructor " ++ show cName ++ " not defined for ADTDefinition " ++ show aName))
                                                Just cDef -> Right (aDef, cDef)

-- | When all value expressions are constant values, return Just them otherwise return Nothing.
toMaybeValues :: [ValExpr v] -> Maybe [Value]
toMaybeValues = foldl toMaybeValue (Just [])
    where
        toMaybeValue :: Maybe [Value] -> ValExpr v -> Maybe [Value]
        toMaybeValue Nothing   _                                           = Nothing
        toMaybeValue (Just vs) (TorXakis.ValExpr.ValExpr.view -> Vconst v) = Just (v:vs)
        toMaybeValue _         _                                           = Nothing

-- | Apply ADT Constructor of the given ADT Name and Constructor Name on the provided arguments (the list of value expressions).
mkCstr :: ValExprContext c v => c v -> RefByName ADTDef -> RefByName ConstructorDef -> [ValExpr v] -> Either MinError (ValExpr v)
mkCstr ctx aName cName as = getCstr ctx aName cName >>= const (unsafeCstr aName cName as)
                                
unsafeCstr :: RefByName ADTDef -> RefByName ConstructorDef -> [ValExpr v] -> Either MinError (ValExpr v)
unsafeCstr aName cName as = case toMaybeValues as of
                                Just vs -> unsafeConst (Ccstr aName cName vs)
                                Nothing -> Right $ ValExpr (Vcstr aName cName as)

-- | Is the provided value expression made by the ADT constructor with the given ADT Name and Constructor Name?
mkIsCstr :: ValExprContext c v => c v -> RefByName ADTDef -> RefByName ConstructorDef -> ValExpr v -> Either MinError (ValExpr v)
mkIsCstr ctx aName cName v = getCstr ctx aName cName >>= structuralIsCstr aName cName v

-- One time only check - will never change (since structural)
-- After type checking holds:
-- IsX(t::T) with T having only one constructor (X) <==> true
structuralIsCstr :: RefByName ADTDef -> RefByName ConstructorDef -> ValExpr v -> (ADTDef, ConstructorDef) -> Either MinError (ValExpr v)
structuralIsCstr aName cName v (aDef,_) = case HashMap.toList ( (constructors . viewADTDef) aDef) of
                                                    [_] -> Right trueValExpr
                                                    _   -> unsafeIsCstr aName cName v

unsafeIsCstr :: RefByName ADTDef -> RefByName ConstructorDef -> ValExpr v -> Either MinError (ValExpr v)
unsafeIsCstr aName cName (TorXakis.ValExpr.ValExpr.view -> Vcstr a c _)          = unsafeConst (Cbool (aName == a && cName == c))
unsafeIsCstr aName cName (TorXakis.ValExpr.ValExpr.view -> Vconst (Ccstr a c _)) = unsafeConst (Cbool (aName == a && cName == c))
unsafeIsCstr aName cName v                                                       = Right $ ValExpr (Viscstr aName cName v)

-- | Access field made by ADT Constructor of the given ADT Name and Constructor Name on the provided argument.
mkAccess :: ValExprContext c v => c v -> RefByName ADTDef -> RefByName ConstructorDef -> RefByName FieldDef -> ValExpr v -> Either MinError (ValExpr v)
mkAccess ctx aName cName fName v = getCstr ctx aName cName >>= getFieldInfo . snd >>= (\(s,p) -> unsafeAccess aName cName s p v)
    where
        getFieldInfo :: ConstructorDef -> Either MinError (Sort, Int)
        getFieldInfo cDef = case lookupField (zip (fields (viewConstructorDef cDef)) [0..]) of
                                Nothing       -> Left $ MinError (T.pack ("FieldName " ++ show fName ++ " not contained in constructor " ++ show cName ++ " of ADTDefinition " ++ show aName))
                                Just (fd,pos) -> Right (getSort fd, pos)
            where                    
                lookupField :: [(FieldDef, Int)] -> Maybe (FieldDef, Int)
                lookupField []            = Nothing
                lookupField ((f,p):xs)    = if fieldName f == toName fName
                                            then Just (f,p)
                                            else lookupField xs

unsafeAccess :: RefByName ADTDef -> RefByName ConstructorDef -> Sort -> Int -> ValExpr v -> Either MinError (ValExpr v)
-- Note: different sort is impossible so aName for both the same
unsafeAccess _ cName _ pos (TorXakis.ValExpr.ValExpr.view -> Vcstr _ c fs) = 
    if cName == c
        then Right $ fs !! pos
        else Left $ MinError (T.pack ("Error in model: Accessing field with number " ++ show pos ++ " of constructor " ++ show cName ++ " on instance from constructor " ++ show c
                                      ++ "\nFor more info, see https://github.com/TorXakis/TorXakis/wiki/Function#implicitly-defined-typedef-functions") )
unsafeAccess _ cName _ pos (TorXakis.ValExpr.ValExpr.view -> Vconst (Ccstr _ c fs)) = 
    if cName == c
        then unsafeConst $ fs !! pos
        else Left $ MinError (T.pack ("Error in model: Accessing field with number " ++ show pos ++ " of constructor " ++ show cName ++ " on value from constructor " ++ show c
                                      ++ "\nFor more info, see https://github.com/TorXakis/TorXakis/wiki/Function#implicitly-defined-typedef-functions") )
unsafeAccess aName cName srt pos v = Right $ ValExpr (Vaccess aName cName srt pos v)

------------------------------------------------------------------------------------------------------------------
-- Substitution
------------------------------------------------------------------------------------------------------------------

-- | find mismatches in sort in mapping
mismatchesSort :: (HasSort a, HasSort b) => HashMap.Map a b -> [ (a, b) ]
mismatchesSort = filter (\(a,b) -> getSort a /= getSort b) . HashMap.toList

-- | Partial Substitution: Substitute some variables by value expressions in a value expression.
-- The Either is needed since substitution can cause an invalid ValExpr. 
-- For example, substitution of a variable by zero can cause a division by zero error
partSubst :: forall c v . ValExprContext c v => c v -> HashMap.Map v (ValExpr v) -> ValExpr v -> Either MinError (ValExpr v)
partSubst ctx mp ve | HashMap.null mp          = Right ve
                    | not (null mismatches)    = Left $ MinError (T.pack ("Sort mismatches in map : " ++ show mismatches))
                    | otherwise                = partSubstView (TorXakis.ValExpr.ValExpr.view ve)
  where
    mismatches :: [ (v, ValExpr v) ]
    mismatches = mismatchesSort mp
    
    partSubstView :: ValExprView v -> Either MinError (ValExpr v)
    partSubstView (Vconst c)                = unsafeConst c
    partSubstView (Vvar v)                  = case HashMap.lookup v mp of
                                                    Nothing -> unsafeVar v
                                                    Just x  -> Right x
    partSubstView (Vequal ve1 ve2)          = partSubstView (TorXakis.ValExpr.ValExpr.view ve1) >>= (\ne1 ->
                                              partSubstView (TorXakis.ValExpr.ValExpr.view ve2) >>= 
                                              unsafeEqual ne1)
    partSubstView (Vite c tb fb)            = partSubstView (TorXakis.ValExpr.ValExpr.view tb) >>= (\ntb ->
                                              partSubstView (TorXakis.ValExpr.ValExpr.view fb) >>= (\nfb ->
                                              partSubstView (TorXakis.ValExpr.ValExpr.view c) >>= (\nc ->
                                              unsafeITE nc ntb nfb)))
    partSubstView (Vfunc fs vs)             = case partitionEithers (map (partSubstView . TorXakis.ValExpr.ValExpr.view) vs) of
                                                ([], nvs) -> unsafeFunc ctx fs nvs
                                                (es, _)   -> Left $ MinError (T.pack ("Subst partSubst 'func' failed\n" ++ show es))
    partSubstView (Vpredef fs vs)           = case partitionEithers (map (partSubstView . TorXakis.ValExpr.ValExpr.view) vs) of
                                                ([], nvs) -> unsafePredefNonSolvable ctx fs nvs
                                                (es, _)   -> Left $ MinError (T.pack ("Subst partSubst 'predef' failed\n" ++ show es))
    partSubstView (Vnot v)                  = partSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeNot
    partSubstView (Vand s)                  = case partitionEithers (map (partSubstView . TorXakis.ValExpr.ValExpr.view) (Set.toList s)) of
                                                ([], ns) -> unsafeAnd (Set.fromList ns)
                                                (es, _)  -> Left $ MinError (T.pack ("Subst partSubst 'and' failed\n" ++ show es))
    partSubstView (Vdivide t n)             = partSubstView (TorXakis.ValExpr.ValExpr.view t) >>= (\nt ->
                                              partSubstView (TorXakis.ValExpr.ValExpr.view n) >>=
                                              unsafeDivide nt)
    partSubstView (Vmodulo t n)             = partSubstView (TorXakis.ValExpr.ValExpr.view t) >>= (\nt ->
                                              partSubstView (TorXakis.ValExpr.ValExpr.view n) >>=
                                              unsafeModulo nt)
    partSubstView (Vsum m)                  = case partitionEithers (map (\(x,i) -> partSubstView (TorXakis.ValExpr.ValExpr.view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                ([], l) -> unsafeSumFromMap (Map.fromListWith (+) l)
                                                (es, _) -> Left $ MinError (T.pack ("Subst partSubst 'sum' failed\n" ++ show es))
    partSubstView (Vproduct m)              = case partitionEithers (map (\(x,i) -> partSubstView (TorXakis.ValExpr.ValExpr.view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                ([], l) -> unsafeProductFromMap (Map.fromListWith (+) l)
                                                (es, _) -> Left $ MinError (T.pack ("Subst partSubst 'product' failed\n" ++ show es))
    partSubstView (Vgez v)                  = partSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeGEZ
    partSubstView (Vlength s)               = partSubstView (TorXakis.ValExpr.ValExpr.view s) >>= unsafeLength
    partSubstView (Vat s i)                 = partSubstView (TorXakis.ValExpr.ValExpr.view s) >>= (\ns ->
                                              partSubstView (TorXakis.ValExpr.ValExpr.view i) >>= 
                                              unsafeAt ns)
    partSubstView (Vconcat s)               = case partitionEithers (map (partSubstView . TorXakis.ValExpr.ValExpr.view) s) of
                                                ([], ns) -> unsafeConcat ns
                                                (es, _)  -> Left $ MinError (T.pack ("Subst partSubst 'concat' failed\n" ++ show es))
    partSubstView (Vstrinre s r)            = partSubstView (TorXakis.ValExpr.ValExpr.view s) >>= (\ns ->
                                              partSubstView (TorXakis.ValExpr.ValExpr.view r) >>= 
                                              unsafeStrInRe ns)
    partSubstView (Vcstr a c l)             = case partitionEithers (map (partSubstView . TorXakis.ValExpr.ValExpr.view) l) of
                                                ([], nl) -> unsafeCstr a c nl
                                                (es, _)  -> Left $ MinError (T.pack ("Subst partSubst 'cstr' failed\n" ++ show es))
    partSubstView (Viscstr a c v)           = partSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeIsCstr a c
    partSubstView (Vaccess a c s p v)       = partSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeAccess a c s p


-- | Complete Substitution: Substitute all variables by value expressions in a value expression.
-- Since all variables are changed, one can change the kind of variables.
compSubst :: forall c v w . (ValExprContext c v, VarDef w) => c v -> HashMap.Map v (ValExpr w) -> ValExpr v -> Either MinError (ValExpr w)
compSubst ctx mp ve | not (null mismatches)    = Left $ MinError (T.pack ("Sort mismatches in map : " ++ show mismatches))
                    | otherwise                = compSubstView (TorXakis.ValExpr.ValExpr.view ve)
  where
    mismatches :: [ (v, ValExpr w) ]
    mismatches = mismatchesSort mp

    compSubstView :: ValExprView v -> Either MinError (ValExpr w)
    compSubstView (Vconst c)                = unsafeConst c
    compSubstView (Vvar v)                  = case HashMap.lookup v mp of
                                                    Nothing -> Left $ MinError (T.pack ("Subst compSubst: incomplete. Missing " ++ show v))
                                                    Just w  -> Right w
    compSubstView (Vequal ve1 ve2)          = compSubstView (TorXakis.ValExpr.ValExpr.view ve1) >>= (\ne1 ->
                                              compSubstView (TorXakis.ValExpr.ValExpr.view ve2) >>= 
                                              unsafeEqual ne1)
    compSubstView (Vite c tb fb)            = compSubstView (TorXakis.ValExpr.ValExpr.view tb) >>= (\ntb ->
                                              compSubstView (TorXakis.ValExpr.ValExpr.view fb) >>= (\nfb ->
                                              compSubstView (TorXakis.ValExpr.ValExpr.view c) >>= (\nc ->
                                              unsafeITE nc ntb nfb)))
    compSubstView (Vfunc fs vs)             = case partitionEithers (map (compSubstView . TorXakis.ValExpr.ValExpr.view) vs) of
                                                    ([], nvs) -> unsafeFunc ctx fs nvs
                                                    (es, _)   -> Left $ MinError (T.pack ("Subst compSubst 'func' failed\n" ++ show es))
    compSubstView (Vpredef fs vs)           = case partitionEithers (map (compSubstView . TorXakis.ValExpr.ValExpr.view) vs) of
                                                    ([], nvs) -> unsafePredefNonSolvable ctx fs nvs
                                                    (es, _)   -> Left $ MinError (T.pack ("Subst compSubst 'predef' failed\n" ++ show es))
    compSubstView (Vnot v)                  = compSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeNot
    compSubstView (Vand s)                  = case partitionEithers (map (compSubstView . TorXakis.ValExpr.ValExpr.view) (Set.toList s)) of
                                                    ([], ns) -> unsafeAnd (Set.fromList ns)
                                                    (es, _)  -> Left $ MinError (T.pack ("Subst compSubst 'and' failed\n" ++ show es))
    compSubstView (Vdivide t n)             = compSubstView (TorXakis.ValExpr.ValExpr.view t) >>= (\nt ->
                                              compSubstView (TorXakis.ValExpr.ValExpr.view n) >>=
                                              unsafeDivide nt)
    compSubstView (Vmodulo t n)             = compSubstView (TorXakis.ValExpr.ValExpr.view t) >>= (\nt ->
                                              compSubstView (TorXakis.ValExpr.ValExpr.view n) >>=
                                              unsafeModulo nt)
    compSubstView (Vsum m)                  = case partitionEithers (map (\(x,i) -> compSubstView (TorXakis.ValExpr.ValExpr.view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                    ([], l) -> unsafeSumFromMap (Map.fromListWith (+) l)
                                                    (es, _) -> Left $ MinError (T.pack ("Subst compSubst 'sum' failed\n" ++ show es))
    compSubstView (Vproduct m)              = case partitionEithers (map (\(x,i) -> compSubstView (TorXakis.ValExpr.ValExpr.view x) >>= (\nx -> Right (nx,i)))
                                                                         (Map.toList m)) of
                                                    ([], l) -> unsafeProductFromMap (Map.fromListWith (+) l)
                                                    (es, _) -> Left $ MinError (T.pack ("Subst compSubst 'product' failed\n" ++ show es))
    compSubstView (Vgez v)                  = compSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeGEZ
    compSubstView (Vlength s)               = compSubstView (TorXakis.ValExpr.ValExpr.view s) >>= unsafeLength
    compSubstView (Vat s i)                 = compSubstView (TorXakis.ValExpr.ValExpr.view s) >>= (\ns ->
                                              compSubstView (TorXakis.ValExpr.ValExpr.view i) >>= 
                                              unsafeAt ns)
    compSubstView (Vconcat s)               = case partitionEithers (map (compSubstView . TorXakis.ValExpr.ValExpr.view) s) of
                                                    ([], ns) -> unsafeConcat ns
                                                    (es, _)  -> Left $ MinError (T.pack ("Subst compSubst 'concat' failed\n" ++ show es))
    compSubstView (Vstrinre s r)            = compSubstView (TorXakis.ValExpr.ValExpr.view s) >>= (\ns ->
                                              compSubstView (TorXakis.ValExpr.ValExpr.view r) >>= 
                                              unsafeStrInRe ns)
    compSubstView (Vcstr a c l)             = case partitionEithers (map (compSubstView . TorXakis.ValExpr.ValExpr.view) l) of
                                                    ([], nl) -> unsafeCstr a c nl
                                                    (es, _)  -> Left $ MinError (T.pack ("Subst compSubst 'cstr' failed\n" ++ show es))
    compSubstView (Viscstr a c v)           = compSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeIsCstr a c
    compSubstView (Vaccess a c s p v)       = compSubstView (TorXakis.ValExpr.ValExpr.view v) >>= unsafeAccess a c s p

------------------------------------------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------------------------------------------

-- | A ValExprContext instance contains all definitions to work with value expressions and references thereof
class (SortContext (a v), VarDef v) => ValExprContext a v where
    -- | Accessor for Function Definitions
    funcDefs :: a v -> HashMap.Map FuncSignature (FuncDef v)

    -- | Add function definitions to value expression context.
    --   A value expression context is returned when the following constraints are satisfied:
    --
    --   * The signatures of the function definitions are unique.
    --
    --   * The variables used are known
    --
    --   * All references (both Sort and FunctionDefinition) are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncDefs :: a v -> [FuncDef v] -> Either MinError (a v)


-- | A minimal instance of 'ValExprContext'.
data MinimalValExprContext v = MinimalValExprContext { sortContext :: MinimalSortContext
                                                         -- function definitions
                                                     , _funcDefs :: HashMap.Map FuncSignature (FuncDef v)
                                                     } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext (MinimalValExprContext MinimalVarDef) where
    empty = MinimalValExprContext (TorXakis.Sort.empty::MinimalSortContext) HashMap.empty
    adtDefs ctx    = adtDefs (sortContext ctx)
    addAdtDefs ctx as = case addAdtDefs (sortContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {sortContext = sctx} 

instance ValExprContext MinimalValExprContext MinimalVarDef where
    funcDefs = _funcDefs
    addFuncDefs ctx fds
        | not $ null nuFuncDefs              = Left $ MinError (T.pack ("Non unique function signatures: " ++ show nuFuncDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of function signatures with undefined sorts: " ++ show undefinedSorts))
        | not $ null undefinedVariables      = Left $ MinError (T.pack ("List of function signatures with undefined variables in their bodies: " ++ show undefinedVariables))
        | not $ null undefinedFuncSignatures = Left $ MinError (T.pack ("List of function signatures with undefined function signatures in their bodies: " ++ show undefinedFuncSignatures))
        | otherwise                          = Right $ newCtx ctx (toMapByFuncSignature fds)
      where
        nuFuncDefs :: [FuncDef MinimalVarDef]
        nuFuncDefs = repeatedByFuncSignatureIncremental (HashMap.elems (funcDefs ctx)) fds

        undefinedSorts :: [(FuncSignature, Set.Set Sort)]
        undefinedSorts = mapMaybe undefinedSort fds

        undefinedSort :: HasFuncSignature a => a -> Maybe (FuncSignature, Set.Set Sort)
        undefinedSort fd = let fs@(FuncSignature _ as rs) = getFuncSignature fd in
                            case filter (not . elemSort ctx) (rs:as) of
                                [] -> Nothing
                                xs -> Just (fs, Set.fromList xs)

        undefinedVariables :: [(FuncSignature, Set.Set MinimalVarDef)]
        undefinedVariables = mapMaybe undefinedVariable fds

        undefinedVariable :: FuncDef MinimalVarDef -> Maybe (FuncSignature, Set.Set MinimalVarDef)
        undefinedVariable fd = let vw            = TorXakis.FuncDef.view fd
                                   definedVars   = Set.fromList (paramDefs vw)
                                   usedVars      = freeVars (body vw)
                                   undefinedVars = Set.difference usedVars definedVars
                                in
                                    if Set.null undefinedVars
                                        then Nothing
                                        else Just (getFuncSignature fd, undefinedVars)

        undefinedFuncSignatures :: [(FuncSignature, Set.Set FuncSignature)]
        undefinedFuncSignatures = mapMaybe undefinedFuncSignature fds

        undefinedFuncSignature :: FuncDef MinimalVarDef -> Maybe (FuncSignature, Set.Set FuncSignature)
        undefinedFuncSignature fd = let definedFuncSignatures = HashMap.union (toMapByFuncSignature fds) (_funcDefs ctx) in
                                        case findUndefinedFuncSignature definedFuncSignatures (body (TorXakis.FuncDef.view fd)) of
                                            [] -> Nothing
                                            xs -> Just (getFuncSignature fd, Set.fromList xs)

        newCtx :: MinimalValExprContext MinimalVarDef -> HashMap.Map FuncSignature (FuncDef MinimalVarDef) -> MinimalValExprContext MinimalVarDef
        newCtx ctx' mfs = let updateCtx = ctx'{ _funcDefs = HashMap.union (funcDefs ctx') mfs }
                              (lm, rm)  = HashMap.mapEither (newFuncDef updateCtx . TorXakis.FuncDef.view) mfs in
                                if HashMap.null lm 
                                    then let (lc, rc) = HashMap.partition isConstBody rm in
                                            if HashMap.null lc 
                                                then ctx'{ _funcDefs = HashMap.union (funcDefs ctx') rc }
                                                else newCtx (ctx'{ _funcDefs = HashMap.union (funcDefs ctx') lc }) rc
                                    else error ("All check passed, yet errors occurred\n" ++ show (HashMap.elems lm))

        newFuncDef :: MinimalValExprContext MinimalVarDef -> FuncDefView MinimalVarDef -> Either MinError (FuncDef MinimalVarDef)
        newFuncDef updateCtx (FuncDefView nm ps bd) = compSubst updateCtx (HashMap.fromList (zip ps (map (ValExpr . Vvar) ps))) bd >>= mkFuncDef nm ps
        
        isConstBody :: FuncDef v -> Bool
        isConstBody fd = case TorXakis.ValExpr.ValExpr.view (body (TorXakis.FuncDef.view fd)) of
                                Vconst {} -> True
                                _         -> False

-- | Find Undefined Function Signatures in given Value Expression (given the defined Function Signatures)
findUndefinedFuncSignature :: HashMap.Map FuncSignature (FuncDef v) -> ValExpr v -> [FuncSignature]
findUndefinedFuncSignature definedFuncSignatures = findUndefinedFuncSignature'
    where
        findUndefinedFuncSignature' :: ValExpr v -> [FuncSignature]
        findUndefinedFuncSignature' = findUndefinedFuncSignatureView . TorXakis.ValExpr.ValExpr.view
        
        findUndefinedFuncSignatureView :: ValExprView v -> [FuncSignature]
        findUndefinedFuncSignatureView Vconst{}                            = []
        findUndefinedFuncSignatureView Vvar{}                              = []
        findUndefinedFuncSignatureView (Vequal v1 v2)                      = findUndefinedFuncSignature' v1 ++ findUndefinedFuncSignature' v2
        findUndefinedFuncSignatureView (Vite c t f)                        = findUndefinedFuncSignature' c ++ findUndefinedFuncSignature' t ++ findUndefinedFuncSignature' f
        findUndefinedFuncSignatureView (Vfunc f as)                        = (if HashMap.member f definedFuncSignatures
                                                                                    then []
                                                                                    else [f]
                                                                              )
                                                                              ++ concatMap findUndefinedFuncSignature' as
        findUndefinedFuncSignatureView (Vpredef _ as)                      = concatMap findUndefinedFuncSignature' as
        findUndefinedFuncSignatureView (Vnot v)                            = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vand vs)                           = concatMap findUndefinedFuncSignature' (Set.toList vs)
        findUndefinedFuncSignatureView (Vdivide t n)                       = findUndefinedFuncSignature' t ++ findUndefinedFuncSignature' n
        findUndefinedFuncSignatureView (Vmodulo t n)                       = findUndefinedFuncSignature' t ++ findUndefinedFuncSignature' n
        findUndefinedFuncSignatureView (Vsum mp)                           = concatMap findUndefinedFuncSignature' (Map.keys mp)
        findUndefinedFuncSignatureView (Vproduct mp)                       = concatMap findUndefinedFuncSignature' (Map.keys mp)
        findUndefinedFuncSignatureView (Vgez v)                            = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vlength v)                         = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vat s p)                           = findUndefinedFuncSignature' s ++ findUndefinedFuncSignature' p
        findUndefinedFuncSignatureView (Vconcat vs)                        = concatMap findUndefinedFuncSignature' vs
        findUndefinedFuncSignatureView (Vstrinre s r)                      = findUndefinedFuncSignature' s ++ findUndefinedFuncSignature' r
        findUndefinedFuncSignatureView (Vcstr _ _ as)                      = concatMap findUndefinedFuncSignature' as
        findUndefinedFuncSignatureView (Viscstr _ _ v)                     = findUndefinedFuncSignature' v
        findUndefinedFuncSignatureView (Vaccess _ _ _ _ v)                 = findUndefinedFuncSignature' v