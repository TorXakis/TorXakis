{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Unsafe
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Unsafe constructors for Value Expressions (except function instantiation)
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
module TorXakis.ValExpr.Unsafe
( unsafeConst
, unsafeVar
, unsafeEqual
, unsafeITE
, unsafePredefNonSolvable
, unsafeNot
, unsafeAnd
, unsafeUnaryMinus
, unsafeDivide
, unsafeModulo
, unsafeSum
, unsafeSumFactor
, unsafeProduct
, unsafeProductFactor
, unsafeGEZ
, unsafeLength
, unsafeAt
, unsafeConcat
, unsafeStrInRe
, unsafeCstr
, unsafeIsCstr
, unsafeAccess
, trueValExpr
, toMaybeValues
)
where
import           Data.Either     (partitionEithers)
import           Data.List       (intercalate)
import qualified Data.Map        as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Text       as T
import           Text.Regex.TDFA

import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.RefByIndex
import           TorXakis.Sort
import           TorXakis.SortContext
import           TorXakis.ValExpr.RegexXSD2Posix
import           TorXakis.ValExpr.ValExpr
import           TorXakis.Value
import           TorXakis.Var

trueValExpr :: ValExpression
trueValExpr = ValExpression $ Vconst (Cbool True)

falseValExpr :: ValExpression
falseValExpr = ValExpression $ Vconst (Cbool False)

stringEmptyValExpr :: ValExpression
stringEmptyValExpr = ValExpression $ Vconst (Cstring (T.pack ""))

zeroValExpr :: ValExpression
zeroValExpr = ValExpression $ Vconst (Cint 0)

unsafeConst :: Value -> Either Error ValExpression
unsafeConst = Right . ValExpression . Vconst

unsafeVar :: RefByName VarDef -> Either Error ValExpression
unsafeVar = Right . ValExpression . Vvar

unsafeEqual :: Either Error ValExpression -> Either Error ValExpression -> Either Error ValExpression
unsafeEqual (Left e1)  (Left e2 ) = Left $ Error ("Equal error on both sides\nleft: " ++ show e1 ++ "\nright: " ++ show e2)
unsafeEqual (Left e1)  _          = Left $ Error ("Equal error on left side: " ++ show e1)
unsafeEqual _          (Left e2)  = Left $ Error ("Equal error on right side: " ++ show e2)
unsafeEqual (Right v1) (Right v2) = unsafeEqual' v1 v2
  where
    unsafeEqual' :: ValExpression -> ValExpression -> Either Error ValExpression
    -- Simplification: a == a <==> True
    -- Note: also (1 % v) == (1 % v) resolves to True, and later substitution of v by 0 still yields True
    unsafeEqual' ve1 ve2 | ve1 == ve2                    = Right trueValExpr
    -- Simplification: Different Values <==> False : use Same Values are already detected in previous step
    unsafeEqual' (TorXakis.ValExpr.ValExpr.view -> Vconst {}) (TorXakis.ValExpr.ValExpr.view -> Vconst {}) = Right falseValExpr
    -- Simplification: True == e <==> e (twice)
    unsafeEqual' b e | b == trueValExpr                = Right e
    unsafeEqual' e b | b == trueValExpr                = Right e
    -- Simplification: False == e <==> not e (twice)
    unsafeEqual' b e | b == falseValExpr               = unsafeNot' e
    unsafeEqual' e b | b == falseValExpr               = unsafeNot' e
    -- Simplification: Not x == x <==> false (twice)
    unsafeEqual' e (TorXakis.ValExpr.ValExpr.view -> Vnot n) | e == n             = Right falseValExpr
    unsafeEqual' (TorXakis.ValExpr.ValExpr.view -> Vnot n) e | e == n             = Right falseValExpr
    -- Simplification: Not x == Not y <==> x == y
    unsafeEqual' (TorXakis.ValExpr.ValExpr.view -> Vnot n1) (TorXakis.ValExpr.ValExpr.view -> Vnot n2)     = unsafeEqual' n1 n2
    -- Same representation: Not a == b <==> a == Not b (twice)
    unsafeEqual' x@(TorXakis.ValExpr.ValExpr.view -> Vnot n) e = if n <= e
                                                                then Right $ ValExpression (Vequal x e)
                                                                else case unsafeNot' e of
                                                                        Left m  -> error ("Unexpected error in Equal: " ++ show m)
                                                                        Right m -> Right $ ValExpression (Vequal m n)
    unsafeEqual' e x@(TorXakis.ValExpr.ValExpr.view -> Vnot n) = if n <= e
                                                                then Right $ ValExpression (Vequal x e)
                                                                else case unsafeNot' e of
                                                                        Left m  -> error ("Unexpected error in Equal: " ++ show m)
                                                                        Right m -> Right $ ValExpression (Vequal m n)
    -- Same representation: a == b <==> b == a
    unsafeEqual' ve1 ve2                                 = if ve1 <= ve2
                                                                then Right $ ValExpression (Vequal ve1 ve2)
                                                                else Right $ ValExpression (Vequal ve2 ve1)

-- Lazy evaluation (e.g to allow subst n -> 0 in IF n==0 THEN t ESLE t%n FI to succeed [although t%0 fails])
unsafeITE :: Either Error ValExpression -> Either Error ValExpression -> Either Error ValExpression -> Either Error ValExpression
-- Simplification: if True then a else b <==> a
unsafeITE (Right b) tb _ | b == trueValExpr         = tb
-- Simplification: if False then a else b <==> b
unsafeITE (Right b) _ fb | b == falseValExpr        = fb
-- Simplification: if (not c) then tb else fb <==> if c then fb else tb
unsafeITE (Right (TorXakis.ValExpr.ValExpr.view -> Vnot n)) tb fb = unsafeITE (Right n) fb tb
-- Simplification: if c then a else a <==> a
unsafeITE _ (Right tb) (Right fb) | tb == fb        = Right tb
unsafeITE (Left eb) (Left et) (Left ef) = Left $ Error ("ITE Error 3" ++ "\ncondition: " ++ show eb ++ "\ntrue branch: " ++ show et ++ "\nfalse branch: " ++ show ef)
unsafeITE _         (Left et) (Left ef) = Left $ Error ("ITE Error 2" ++                               "\ntrue branch: " ++ show et ++ "\nfalse branch: " ++ show ef)
unsafeITE (Left eb) _         (Left ef) = Left $ Error ("ITE Error 2" ++ "\ncondition: " ++ show eb ++                                 "\nfalse branch: " ++ show ef)
unsafeITE (Left eb) (Left et) _         = Left $ Error ("ITE Error 2" ++ "\ncondition: " ++ show eb ++ "\ntrue branch: " ++ show et                                 )
unsafeITE (Left eb) _         _         = Left $ Error ("ITE Error 1" ++ "\ncondition: " ++ show eb                                                                 )
unsafeITE _         (Left et) _         = Left $ Error ("ITE Error 1" ++                               "\ntrue branch: " ++ show et                                 )
unsafeITE _         _         (Left ef) = Left $ Error ("ITE Error 1" ++                                                               "\nfalse branch: " ++ show ef)
-- Simplification: if c then True else False <==> c
unsafeITE b (Right tb) (Right fb) | tb == trueValExpr && fb == falseValExpr = b
-- Simplification: if c then False else True <==> not c
unsafeITE b (Right tb) (Right fb) | tb == falseValExpr && fb == trueValExpr = unsafeNot b
-- Simplification: if q then p else False fi <==> q /\ p : Note: p is boolean expression (otherwise different sorts in branches) 
-- Not implemented to enable conditional evaluation
unsafeITE (Right b) (Right tb) (Right fb)                                   = Right $ ValExpression (Vite b tb fb)

unsafePredefNonSolvable :: SortContext c => c -> RefByFuncSignature -> [Either Error ValExpression] -> Either Error ValExpression
unsafePredefNonSolvable ctx r vs = case partitionEithers vs of
                                         ([], xs)   -> case toMaybeValues xs of
                                                            Just values -> evalPredefNonSolvable values
                                                            Nothing     -> Right $ ValExpression (Vpredef r xs)
                                         (es, _)    -> Left $ Error ("PredefNonSolvable Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))
    where
        fs :: FuncSignature
        fs = toFuncSignature r

        evalPredefNonSolvable :: [Value] -> Either Error ValExpression
        evalPredefNonSolvable values =
            case (T.unpack (TorXakis.Name.toText (TorXakis.FuncSignature.funcName fs)), returnSort fs, values) of
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

unsafeNot :: Either Error ValExpression -> Either Error ValExpression
unsafeNot (Left e) = Left $ Error ("Not Error " ++ show e)
unsafeNot (Right v) = unsafeNot' v

unsafeNot' :: ValExpression -> Either Error ValExpression
-- Simplification: not True <==> False
unsafeNot' b | b == trueValExpr                              = Right falseValExpr
-- Simplification: not False <==> True
unsafeNot' b | b == falseValExpr                             = Right trueValExpr
-- Simplification: not (not x) <==> x
unsafeNot' (TorXakis.ValExpr.ValExpr.view -> Vnot ve)          = Right ve
-- Simplification: not (if cs then tb else fb) <==> if cs then not (tb) else not (fb)
-- also needs if a then x else y /\ if a then u else w <==> if a then x /\ u else y /\ w
-- otherwise TorXakis can't maintain (a \/ not a) <==> True
-- unsafeNot' (TorXakis.ValExpr.ValExpr.view -> Vite cs tb fb)  = case (unsafeNot' tb, unsafeNot' fb) of
--                                                                     (Right nt, Right nf) -> Right $ ValExpression (Vite cs nt nf)
--                                                                     _                    -> error "Unexpected error in NOT with ITE"
unsafeNot' ve                                                = Right $ ValExpression (Vnot ve)

-- TODO? More laziness?
-- If a False value is present, the combination is False (even when errors are present)
unsafeAnd :: Set.Set (Either Error ValExpression) -> Either Error ValExpression
unsafeAnd ps = case partitionEithers (Set.toList ps) of
                    ([], xs)   -> unsafeAnd' (flattenAnd (Set.fromList xs))
                    (es, _)    -> Left $ Error ("And Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))
    where
        flattenAnd :: Set.Set ValExpression -> Set.Set ValExpression
        flattenAnd = Set.unions . map fromValExpression . Set.toList
        
        fromValExpression :: ValExpression -> Set.Set ValExpression
        fromValExpression (TorXakis.ValExpr.ValExpr.view -> Vand a) = a
        fromValExpression x                                         = Set.singleton x

        -- And doesn't contain elements of type Vand.
        unsafeAnd' :: Set.Set ValExpression -> Either Error ValExpression
        unsafeAnd' s =
            -- Canonical form (merge conditional IFs)
            -- IF a THEN b ELSE False FI /\ IF c THEN d ELSE False FI <==> IF a /\ c THEN b /\ d ELSE False FI
                mergeConditionals s >>= (\can ->
                let s' :: Set.Set ValExpression
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
                                                    else Right $ ValExpression (Vand s')
                )
            where
                mergeConditionals :: Set.Set ValExpression -> Either Error (Set.Set ValExpression)
                mergeConditionals s' = Set.foldr mergeConditional (Right (Set.empty, Set.empty, Set.empty)) s' >>= 
                                        (\(s'', cs, ds) -> case Set.toList cs of
                                                            (_:_:_) -> -- at least two items to merge
                                                                        case unsafeITE (unsafeAnd' cs) (unsafeAnd' ds) (Right falseValExpr) of
                                                                            Right ite -> Right $ Set.insert ite s''
                                                                            Left e    -> Left $ Error ("And Error - mergeConditionals " ++ show e)
                                                            _       -> -- nothing to merge
                                                                        Right s'
                                        )

                mergeConditional :: ValExpression 
                                 -> Either Error (Set.Set ValExpression, Set.Set ValExpression, Set.Set ValExpression)
                                 -> Either Error (Set.Set ValExpression, Set.Set ValExpression, Set.Set ValExpression)
                mergeConditional _                                               (Left e)                                  = Left e
                mergeConditional (TorXakis.ValExpr.ValExpr.view -> Vite c tb fb) (Right (s', cs, ds)) | tb == falseValExpr = unsafeNot' c >>= (\nc -> Right (s', Set.insert nc cs, Set.insert fb ds))
                mergeConditional (TorXakis.ValExpr.ValExpr.view -> Vite c tb fb) (Right (s', cs, ds)) | fb == falseValExpr = Right (s', Set.insert c cs, Set.insert tb ds)
                mergeConditional x                                               (Right (s', cs, ds))                      = Right (Set.insert x s', cs, ds)

                filterNot :: [ValExpression] -> [ValExpression]
                filterNot [] = []
                filterNot (x:xs) = case TorXakis.ValExpr.ValExpr.view x of
                                    Vnot n -> n : filterNot xs
                                    _      ->     filterNot xs

                contains :: Set.Set ValExpression -> ValExpression -> Bool
                contains set (TorXakis.ValExpr.ValExpr.view -> Vand a) = all (`Set.member` set) (Set.toList a)
                contains set a                                         = Set.member a set

unsafeUnaryMinus :: Either Error ValExpression -> Either Error ValExpression
unsafeUnaryMinus (Left e)                                          = Left $ Error ("Unary Minus Error " ++ show e)
unsafeUnaryMinus (Right (TorXakis.ValExpr.ValExpr.view -> Vsum m)) = unsafeSumFromMap (Map.map (* (-1)) m)
unsafeUnaryMinus (Right x)                                         = unsafeSumFromMap (Map.singleton x (-1))

unsafeDivide :: Either Error ValExpression -> Either Error ValExpression -> Either Error ValExpression
unsafeDivide (Left e1)                                                   (Left e2)                                                   = Left $ Error ("Divide Error 2" ++ "\nDividend: " ++ show e1 ++ "\nDivisor:" ++ show e2)
unsafeDivide (Left e1)                                                   _                                                           = Left $ Error ("Divide Error 1" ++ "\nDividend: " ++ show e1                     )
unsafeDivide _                                                           (Left e2)                                                   = Left $ Error ("Divide Error 1" ++                              "\nDivisor:" ++ show e2)
unsafeDivide _                                                           (Right (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint 0)))  = Left $ Error  "Divide Error: Divisor equal to zero"
unsafeDivide (Right (TorXakis.ValExpr.ValExpr.view ->  Vconst (Cint t))) (Right (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint n)))  = unsafeConst (Cint (t `div` n) )
unsafeDivide (Right vet)                                                 (Right ven)                                                 = Right $ ValExpression (Vdivide vet ven)

unsafeModulo :: Either Error ValExpression -> Either Error ValExpression -> Either Error ValExpression
unsafeModulo (Left e1)                                                   (Left e2)                                                   = Left $ Error ("Modulo Error 2" ++ "\nDividend: " ++ show e1 ++ "\nDivisor:" ++ show e2)
unsafeModulo (Left e1)                                                   _                                                           = Left $ Error ("Modulo Error 1" ++ "\nDividend: " ++ show e1                     )
unsafeModulo _                                                           (Left e2)                                                   = Left $ Error ("Modulo Error 1" ++                              "\nDivisor:" ++ show e2)
unsafeModulo _                                                           (Right (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint 0)))  = Left $ Error  "Modulo Error: Divisor equal to zero"
unsafeModulo (Right (TorXakis.ValExpr.ValExpr.view ->  Vconst (Cint t))) (Right (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint n)))  = unsafeConst (Cint (t `mod` n) )
unsafeModulo (Right vet)                                                 (Right ven)                                                 = Right $ ValExpression (Vmodulo vet ven)

-- is key a constant?
isKeyConst :: ValExpression -> Integer -> Bool
isKeyConst (TorXakis.ValExpr.ValExpr.view -> Vconst{}) _ = True
isKeyConst _                                           _ = False

-- Note: inverse of addition (subtraction) is communicative -> occurrence can also be negativeonly
-- Simplification incorporated:
-- 1. remove all nested sums, since (a+b) + (c+d) == (a+b+c+d)
--    remove duplicates (by using occurrence) -- we use communicative property of sum
-- 2. at most one value: the value is the sum of all values
--    special case if the sum is zero, no value is inserted since v == v+0
-- 3. When map is empty return 0
--    When map contains a single element exactly once, return that element
unsafeSum :: [Either Error ValExpression] -> Either Error ValExpression
unsafeSum ps = case partitionEithers ps of
                    ([], xs)   -> unsafeSumFromMap (flattenSum xs)
                    (es, _)    -> Left $ Error ("Sum Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))
    where
        flattenSum :: [ValExpression] -> Map.Map ValExpression Integer
        flattenSum = Map.filter (0 ==) . Map.unionsWith (+) . map fromValExpr       -- combine maps (duplicates should be counted) and remove elements with occurrence 0

        fromValExpr :: ValExpression -> Map.Map ValExpression Integer
        fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vsum m) = m
        fromValExpr x                                         = Map.singleton x 1

partitionEitherTuples :: [(Either Error ValExpression, Integer)] -> ([Error], [(ValExpression, Integer)])
partitionEitherTuples = foldl partitionEitherTuple ([],[])
    where
        partitionEitherTuple :: ([Error], [(ValExpression, Integer)]) -> (Either Error ValExpression, Integer) -> ([Error], [(ValExpression, Integer)])
        partitionEitherTuple (es, xs) (Left  e, _) = (e:es,xs)
        partitionEitherTuple (es, xs) (Right x, i) = (es,(x,i):xs)

unsafeSumFactor :: [(Either Error ValExpression, Integer)] -> Either Error ValExpression
unsafeSumFactor ps = case partitionEitherTuples ps of
                            ([], xs) -> unsafeSumFromMap (flattenSum xs)
                            (es, _)  -> Left $ Error ("Sum Factor Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))
    where
        flattenSum :: [(ValExpression,Integer)] -> Map.Map ValExpression Integer
        flattenSum = Map.filter (0 ==) . Map.unionsWith (+) . map fromValExpr       -- combine maps (duplicates should be counted) and remove elements with occurrence 0
        
        fromValExpr :: (ValExpression, Integer) -> Map.Map ValExpression Integer
        fromValExpr (_                                      , i) | i == 0 = error "Factor of Sum is equal to zero: violates invariant"
        fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vsum m, i)          = Map.map (*i) m
        fromValExpr (x                                      , i)          = Map.singleton x i

-- unsafeSumFromMap doesn't contain elements of type Vsum.
unsafeSumFromMap :: Map.Map ValExpression Integer -> Either Error ValExpression
unsafeSumFromMap m = 
    let (vals, nonvals) = Map.partitionWithKey isKeyConst m
        retVal :: Map.Map ValExpression Integer
        retVal = case sum (map toValue (Map.toList vals)) of
                    0   -> nonvals
                    val -> case unsafeConst (Cint val) of
                                Right x -> Map.insert x 1 nonvals
                                Left _  -> error "Unexpected failure in unsafeConst in unsafeSum"
      in
        case Map.toList retVal of
            []          -> Right zeroValExpr     -- sum of nothing equal to zero
            [(term, 1)] -> Right term
            _           -> Right $ ValExpression (Vsum m)
    where
        toValue :: (ValExpression, Integer) -> Integer
        toValue (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i), o) = i * o
        toValue (_                                               , _) = error "Unexpected value expression (expecting const of integer type) in toValue of unsafeSum"

-- Note: inverse of product (division) is not communicative -> occurrence can only be positive
-- Simplification incorporated:
-- 1. remove all nested products, since (a*b) * (c*d) == (a*b*c*d)
--    remove duplicates (by using occurrence) -- we use communicative property of product
-- 2. at most one value: the value is the product of all values
--    special case if the product is zero, value is zero
-- 3. When non values are empty return value
--    When non-values are not empty, return sum which multiplies value with non-values
unsafeProduct :: [Either Error ValExpression] -> Either Error ValExpression
unsafeProduct ps = case partitionEithers ps of
                        ([], xs)   -> unsafeProductFromMap (flattenProduct xs)
                        (es, _)    -> Left $ Error ("Product Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))
    where
        flattenProduct :: [ValExpression] -> Map.Map ValExpression Integer
        flattenProduct = Map.unionsWith (+) . map fromValExpr       -- combine maps (duplicates should be counted)

        fromValExpr :: ValExpression -> Map.Map ValExpression Integer
        fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vproduct m) = m
        fromValExpr x                                             = Map.singleton x 1

-- TODO: add more laziness (if wanted)
-- 0 * x = 0
-- yet what about 0 * 1/0 ?
unsafeProductFactor :: [(Either Error ValExpression, Integer)] -> Either Error ValExpression
unsafeProductFactor ps = case partitionEitherTuples ps of
                            ([], xs) -> unsafeProductFromMap (flattenProduct xs)
                            (es, _)  -> Left $ Error ("Product Factor Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))
    where
        flattenProduct :: [(ValExpression,Integer)] -> Map.Map ValExpression Integer
        flattenProduct = Map.unionsWith (+) . map fromValExpr       -- combine maps (duplicates should be counted)

        fromValExpr :: (ValExpression, Integer) -> Map.Map ValExpression Integer
        fromValExpr (_                                      , i) | i <= 0 = error "Factor of Product is not positive: violates invariant"
        fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vsum m, i)          = Map.map (*i) m
        fromValExpr (x                                      , i)          = Map.singleton x i

-- Flatten Product doesn't contain elements of type Vproduct.
unsafeProductFromMap  :: Map.Map ValExpression Integer -> Either Error ValExpression
-- Simplification: 0 * x = 0
unsafeProductFromMap  m | Map.member zeroValExpr m = Right zeroValExpr
unsafeProductFromMap  m =
    let (vals, nonvals) = Map.partitionWithKey isKeyConst m
        value = product (map toValue (Map.toList vals))
      in
        case Map.toList nonvals of
            []         -> unsafeConst (Cint value)
            [(term,1)] -> unsafeSumFromMap $ Map.singleton term value
            _          -> unsafeSumFromMap $ Map.singleton (ValExpression (Vproduct nonvals)) value
    where
        toValue :: (ValExpression, Integer) -> Integer
        toValue (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i), o) = i ^ o
        toValue (_                                               , _) = error "Unexpected value expression (expecting const of integer type) in toValue of unsafeProduct"

unsafeGEZ :: Either Error ValExpression -> Either Error ValExpression
unsafeGEZ (Left e) = Left $ Error ("GEZ Error " ++ show e)
unsafeGEZ (Right v) = unsafeGEZ' v

unsafeGEZ' :: ValExpression -> Either Error ValExpression
-- Simplification: Integer Value to Boolean
unsafeGEZ' (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint v)) = unsafeConst (Cbool (0 <= v))
-- Simplification: length of string is always Greater or equal to zero
unsafeGEZ' (TorXakis.ValExpr.ValExpr.view -> Vlength {})      = Right trueValExpr
unsafeGEZ' ve                                                 = Right $ ValExpression (Vgez ve)

unsafeLength :: Either Error ValExpression -> Either Error ValExpression
unsafeLength (Left e) = Left $ Error ("Length Error " ++ show e)
unsafeLength (Right v) = unsafeLength' v

unsafeLength' :: ValExpression -> Either Error ValExpression
unsafeLength' (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) = unsafeConst (Cint (Prelude.toInteger (T.length s)))
unsafeLength' v                                                     = Right $ ValExpression (Vlength v)

unsafeAt :: Either Error ValExpression -> Either Error ValExpression -> Either Error ValExpression
unsafeAt (Left e1) (Left e2) = Left $ Error ("At Error 2" ++ "\nString :" ++ show e1 ++ "\nPosition :" ++ show e2)
unsafeAt (Left e1) _         = Left $ Error ("At Error 1" ++ "\nString :" ++ show e1                             )
unsafeAt _         (Left e2) = Left $ Error ("At Error 1" ++                            "\nPosition :" ++ show e2)
unsafeAt (Right s) (Right p) = unsafeAt' s p

unsafeAt' :: ValExpression -> ValExpression -> Either Error ValExpression
unsafeAt' _                                                     (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i)) | i < 0                                 = Right stringEmptyValExpr
unsafeAt' (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i)) | i >= Prelude.toInteger (T.length s)   = Right stringEmptyValExpr
unsafeAt' (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i))                                         = unsafeConst (Cstring (T.take 1 (T.drop (fromInteger i) s)))    -- s !! i for Text
unsafeAt' (TorXakis.ValExpr.ValExpr.view -> Vconcat ((TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)):xs)) (TorXakis.ValExpr.ValExpr.view -> Vconst (Cint i)) =
    let lengthS = Prelude.toInteger (T.length s) in
        if i < lengthS 
            then unsafeConst (Cstring (T.take 1 (T.drop (fromInteger i) s)))
            else unsafeConcat' xs >>= (\nc -> 
                    unsafeConst (Cint (i - lengthS)) >>=
                        unsafeAt' nc)
unsafeAt' ves vei = Right $ ValExpression (Vat ves vei)

-- TODO? More laziness?
-- E.g. At might only need the first string in Concat, so accept Errors later on
unsafeConcat :: [Either Error ValExpression] -> Either Error ValExpression
unsafeConcat ps = case partitionEithers ps of
                       ([], l) -> unsafeConcat' l
                       (es, _) -> Left $ Error ("Product Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))

unsafeConcat' :: [ValExpression] -> Either Error ValExpression
unsafeConcat' l = case (mergeVals . flatten . filter (stringEmptyValExpr /= ) ) l of
                                         []  -> Right stringEmptyValExpr
                                         [e] -> Right e
                                         cs  -> Right $ ValExpression (Vconcat cs)
  where
    -- implementation details:
    -- Properties incorporated
    --    "" ++ x == x          - remove empty strings
    --    "a" ++ "b" == "ab"    - concat consecutive string values
    --   remove all nested Concats, since (a ++ b) ++ (c ++ d) == (a ++ b ++ c ++ d)

    mergeVals :: [ValExpression] -> [ValExpression]
    mergeVals []            = []
    mergeVals [x]           = [x]
    mergeVals ( (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s1)) 
              : (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s2)) 
              : xs)         = case unsafeConst (Cstring (T.append s1 s2)) of
                                    Right x -> mergeVals (x:xs)
                                    Left e  -> error ("Unexpected error in mergeVals of Concat" ++ show e)
    mergeVals (x1:x2:xs)    = x1 : mergeVals (x2:xs)

    flatten :: [ValExpression] -> [ValExpression]
    flatten = concatMap fromValExpr

    fromValExpr :: ValExpression -> [ValExpression]
    fromValExpr (TorXakis.ValExpr.ValExpr.view -> Vconcat cs) = cs
    fromValExpr x                                             = [x]

unsafeStrInRe :: Either Error ValExpression -> Either Error ValExpression -> Either Error ValExpression
unsafeStrInRe (Left e1) (Left e2) = Left $ Error ("StrInRe Error 2" ++ "\nString :" ++ show e1 ++ "\nRegex :" ++ show e2)
unsafeStrInRe (Left e1) _         = Left $ Error ("StrInRe Error 1" ++ "\nString :" ++ show e1                             )
unsafeStrInRe _         (Left e2) = Left $ Error ("StrInRe Error 1" ++                            "\nRegex :" ++ show e2)
unsafeStrInRe (Right s) (Right p) = unsafeStrInRe' s p

unsafeStrInRe' :: ValExpression -> ValExpression -> Either Error ValExpression
unsafeStrInRe' (TorXakis.ValExpr.ValExpr.view -> Vconst (Cstring s)) 
               (TorXakis.ValExpr.ValExpr.view -> Vconst (Cregex r))  = unsafeConst (Cbool (T.unpack s =~ T.unpack (xsd2posix r) ) )
unsafeStrInRe' s r                                                   = Right $ ValExpression (Vstrinre s r)

-- | When all value expressions are constant values, return Just them otherwise return Nothing.
toMaybeValues :: [ValExpression] -> Maybe [Value]
toMaybeValues = foldl toMaybeValue (Just [])
    where
        toMaybeValue :: Maybe [Value] -> ValExpression -> Maybe [Value]
        toMaybeValue Nothing   _                                           = Nothing
        toMaybeValue (Just vs) (TorXakis.ValExpr.ValExpr.view -> Vconst v) = Just (v:vs)
        toMaybeValue _         _                                           = Nothing

-- TODO? More laziness?
-- e.g. when used in an accessor, only one field is relevant
unsafeCstr :: RefByName ADTDef -> RefByName ConstructorDef -> [Either Error ValExpression] -> Either Error ValExpression
unsafeCstr aName cName ps = case partitionEithers ps of
                                 ([], as)   -> unsafeCstr' aName cName as
                                 (es, _)    -> Left $ Error ("Cstr " ++ show cName ++ " of " ++ show aName ++ " Error " ++ show (length es) ++ "\n" ++ intercalate "\n" (map show es))

unsafeCstr' :: RefByName ADTDef -> RefByName ConstructorDef -> [ValExpression] -> Either Error ValExpression
unsafeCstr' aName cName as = case toMaybeValues as of
                                  Just vs -> unsafeConst (Ccstr aName cName vs)
                                  Nothing -> Right $ ValExpression (Vcstr aName cName as)

unsafeIsCstr :: RefByName ADTDef -> RefByName ConstructorDef -> Either Error ValExpression -> Either Error ValExpression
unsafeIsCstr aName cName (Left e) = Left $ Error ("Is Cstr " ++ show cName ++ " of " ++ show aName ++ " Error: " ++ show e)
unsafeIsCstr aName cName (Right v) = unsafeIsCstr' aName cName v

unsafeIsCstr' :: RefByName ADTDef -> RefByName ConstructorDef -> ValExpression -> Either Error ValExpression
unsafeIsCstr' aName cName (TorXakis.ValExpr.ValExpr.view -> Vcstr a c _)          = unsafeConst (Cbool (aName == a && cName == c))
unsafeIsCstr' aName cName (TorXakis.ValExpr.ValExpr.view -> Vconst (Ccstr a c _)) = unsafeConst (Cbool (aName == a && cName == c))
unsafeIsCstr' aName cName v                                                       = Right $ ValExpression (Viscstr aName cName v)

unsafeAccess :: RefByName ADTDef -> RefByName ConstructorDef -> RefByIndex FieldDef -> Either Error ValExpression -> Either Error ValExpression
unsafeAccess _ _ _ (Left e) = Left $ Error ("Access Error " ++ show e)
unsafeAccess aName cName pos (Right v) = unsafeAccess' aName cName pos v

unsafeAccess' :: RefByName ADTDef -> RefByName ConstructorDef -> RefByIndex FieldDef -> ValExpression -> Either Error ValExpression
-- Note: different sort is impossible so aName for both the same
unsafeAccess' _ cName pos (TorXakis.ValExpr.ValExpr.view -> Vcstr _ c fs) = 
    if cName == c
        then Right $ fs !! toIndex pos
        else Left $ Error (T.pack ("Error in model: Accessing field with number " ++ show pos ++ " of constructor " ++ show cName ++ " on instance from constructor " ++ show c
                                      ++ "\nFor more info, see https://github.com/TorXakis/TorXakis/wiki/Function#implicitly-defined-typedef-functions") )
unsafeAccess' _ cName pos (TorXakis.ValExpr.ValExpr.view -> Vconst (Ccstr _ c fs)) = 
    if cName == c
        then unsafeConst $ fs !! toIndex pos
        else Left $ Error (T.pack ("Error in model: Accessing field with number " ++ show pos ++ " of constructor " ++ show cName ++ " on value from constructor " ++ show c
                                      ++ "\nFor more info, see https://github.com/TorXakis/TorXakis/wiki/Function#implicitly-defined-typedef-functions") )
unsafeAccess' aName cName pos v = Right $ ValExpression (Vaccess aName cName pos v)
