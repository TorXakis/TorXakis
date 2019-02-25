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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
)
where
import qualified Data.Set        as Set

import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.RefByIndex
import           TorXakis.Sort
import           TorXakis.ValExpr.Unsafe
import           TorXakis.ValExpr.ValExpr
import           TorXakis.Value
import           TorXakis.ValExprConstructionContext

------------------------------------------------------------------------------------------------------------------
-- Constructors
------------------------------------------------------------------------------------------------------------------

-- | Create a constant value as a value expression.
mkConst :: ValExprConstructionContext c => c -> Value -> Either Error ValExpression
mkConst ctx v = if memberSort (getSort ctx v) ctx
                    then unsafeConst v
                    else Left $  Error ("Sort " ++ show (getSort ctx v) ++ " not defined in context")

-- | Create a variable as a value expression.
mkVar :: ValExprConstructionContext c => c -> RefByName VarDef -> Either Error ValExpression
mkVar ctx r = case lookupVar (toName r) ctx of
                Nothing -> Left $ Error ("Variable " ++ show r ++ " not defined in context")
                Just _  -> unsafeVar r

-- | Apply operator Equal on the provided value expressions.
mkEqual :: ValExprConstructionContext c => c -> ValExpression -> ValExpression -> Either Error ValExpression
mkEqual ctx ve1 ve2 | getSort ctx ve1 /= getSort ctx ve2    = Left $ Error ("Sort of value expressions in equal differ " ++ show (getSort ctx ve1) ++ " versus " ++ show (getSort ctx ve2))
mkEqual ctx ve1 ve2 | memberSort (getSort ctx ve1) ctx      = unsafeEqual (Right ve1) (Right ve2)
mkEqual ctx ve1 _                                           = Left $  Error ("Sort " ++ show (getSort ctx ve1) ++ " not defined in context")

-- | Apply operator ITE (IF THEN ELSE) on the provided value expressions.
mkITE :: ValExprConstructionContext c => c -> ValExpression -> ValExpression -> ValExpression -> Either Error ValExpression
mkITE ctx b _  _  | getSort ctx b  /= SortBool        = Left $ Error ("Condition of ITE is not of expected sort Bool but " ++ show (getSort ctx b))
mkITE ctx _ tb fb | getSort ctx tb /= getSort ctx fb  = Left $ Error ("Sorts of branches differ " ++ show (getSort ctx tb) ++ " versus " ++ show (getSort ctx fb))
mkITE ctx b tb fb | memberSort (getSort ctx tb) ctx   = unsafeITE (Right b) (Right tb) (Right fb)
mkITE ctx _ tb _                                      = Left $  Error ("Sort " ++ show (getSort ctx tb) ++ " not defined in context")

-- | Create a function call.
-- TODO: check for undefined entities in arguments (vs)
-- TODO: when func signature exists, all sorts are defined. When func signature doesn't exist, we already report an error... so useless to check sorts?
mkFunc :: ValExprConstructionContext c => c -> RefByFuncSignature -> [ValExpression] -> Either Error ValExpression
mkFunc ctx r@(RefByFuncSignature fs) vs 
    | expected /= actual                        = Left $ Error ("Sorts of signature and arguments differ: " ++ show (zip expected actual))
    | not (null undefinedSorts)                 = Left $ Error ("Undefined sorts : " ++ show undefinedSorts)
    | isPredefinedNonSolvableFuncSignature fs   = Left $ Error ("Signature of predefined function : " ++ show fs) -- to avoid confusion and enable round tripping
    | not (memberFunc fs ctx)                   = Left $ Error ("Undefined FuncSignature : " ++ show fs)
    | otherwise                                 = Right $ ValExpression (Vfunc r vs)
        where
            expected = args fs
            actual = map (getSort ctx) vs
            undefinedSorts = filter (not . flip memberSort ctx)  expected


-- | Make a call to some predefined functions
-- Only allowed in CNECTDEF
mkPredefNonSolvable :: ValExprConstructionContext c => c -> RefByFuncSignature -> [ValExpression] -> Either Error ValExpression
mkPredefNonSolvable ctx r@(RefByFuncSignature fs) vs
    | not (isPredefinedNonSolvableFuncSignature fs) = Left $ Error ("Signature is not of a predefined function: " ++ show fs)
    | expected /= actual                            = Left $ Error ("Sorts of signature and arguments differ: " ++ show (zip expected actual) )
    | otherwise                                     = unsafePredefNonSolvable ctx r (map Right vs)
        where
            expected = args fs
            actual = map (getSort ctx) vs
            
-- | Apply operator Not on the provided value expression.
mkNot :: ValExprConstructionContext c => c -> ValExpression -> Either Error ValExpression
mkNot ctx n | getSort ctx n == SortBool = unsafeNot (Right n)
            | otherwise                 = Left $ Error ("Argument of Not is not of expected sort Bool but " ++ show (getSort ctx n))

-- | Apply operator And on the provided list of value expressions.
-- TODO: generalize to Traversable instead of list?
mkAnd :: ValExprConstructionContext c => c -> [ValExpression] -> Either Error ValExpression
mkAnd ctx s | all (\e -> SortBool == getSort ctx e) s = unsafeAnd (Set.fromList (map Right s))
mkAnd _   _                                           = Left $ Error "Not all value expressions in set are of expected sort Bool"

-- | Apply unary operator Minus on the provided value expression.
mkUnaryMinus :: ValExprConstructionContext c => c -> ValExpression -> Either Error ValExpression
mkUnaryMinus ctx v | getSort ctx v == SortInt = unsafeUnaryMinus (Right v)
                   | otherwise                = Left $ Error ("Unary Minus argument not of expected Sort Int but " ++ show (getSort ctx v))

-- | Apply operator Divide on the provided value expressions.
mkDivide :: ValExprConstructionContext c => c -> ValExpression -> ValExpression -> Either Error ValExpression
mkDivide ctx d _ | getSort ctx d /= SortInt = Left $ Error ("Dividend not of expected Sort Int but " ++ show (getSort ctx d))
mkDivide ctx _ d | getSort ctx d /= SortInt = Left $ Error ("Divisor not of expected Sort Int but " ++ show (getSort ctx d))
mkDivide _   t n                        = unsafeDivide (Right t) (Right n)

-- | Apply operator Modulo on the provided value expressions.
mkModulo :: ValExprConstructionContext c => c -> ValExpression -> ValExpression -> Either Error ValExpression
mkModulo ctx d _ | getSort ctx d /= SortInt = Left $ Error ("Dividend not of expected Sort Int but " ++ show (getSort ctx d))
mkModulo ctx _ d | getSort ctx d /= SortInt = Left $ Error ("Divisor not of expected Sort Int but " ++ show (getSort ctx d))
mkModulo _   t n                            = unsafeModulo (Right t) (Right n)

-- | Apply operator sum on the provided list of value expressions.
mkSum :: ValExprConstructionContext c => c -> [ValExpression] -> Either Error ValExpression
mkSum ctx l | all (\e -> SortInt == getSort ctx e) l = unsafeSum (map Right l)
mkSum _   _                                          = Left $ Error "Not all value expressions in list are of expected sort Int"

-- | Apply operator product on the provided list of value expressions.
mkProduct :: ValExprConstructionContext c => c -> [ValExpression] -> Either Error ValExpression
mkProduct ctx l | all (\e -> SortInt == getSort ctx e) l = unsafeProduct (map Right l)
mkProduct _   _                                          = Left $ Error "Not all value expressions in list are of expected sort Int"

-- | Apply operator GEZ (Greater Equal Zero) on the provided value expression.
mkGEZ :: ValExprConstructionContext c => c -> ValExpression -> Either Error ValExpression
mkGEZ ctx d | getSort ctx d /= SortInt = Left $ Error ("Argument of GEZ not of expected Sort Int but " ++ show (getSort ctx d))
            | otherwise                = unsafeGEZ (Right d)

-- | Apply operator Length on the provided value expression.
mkLength :: ValExprConstructionContext c => c -> ValExpression -> Either Error ValExpression
mkLength ctx s | getSort ctx s /= SortString = Left $ Error ("Argument of Length not of expected Sort String but " ++ show (getSort ctx s))
               | otherwise                   = unsafeLength (Right s)

-- | Apply operator At on the provided value expressions.
mkAt :: ValExprConstructionContext c => c -> ValExpression -> ValExpression -> Either Error ValExpression
mkAt ctx s i | getSort ctx s /= SortString = Left $ Error ("First argument of At not of expected Sort String but " ++ show (getSort ctx s))
             | getSort ctx i /= SortInt    = Left $ Error ("Second argument of At not of expected Sort Int but " ++ show (getSort ctx i))
             | otherwise                   = unsafeAt (Right s) (Right i)

-- | Apply operator Concat on the provided sequence of value expressions.
mkConcat :: ValExprConstructionContext c => c -> [ValExpression] -> Either Error ValExpression
mkConcat ctx l | all (\e -> SortString == getSort ctx e) l = unsafeConcat (map Right l)
mkConcat _   _                                             = Left $ Error "Not all value expressions in list are of expected sort String"

-- | Apply String In Regular Expression operator on the provided value expressions.
mkStrInRe :: ValExprConstructionContext c => c -> ValExpression -> ValExpression -> Either Error ValExpression
mkStrInRe ctx s _ | getSort ctx s /= SortString = Left $ Error ("First argument of At not of expected Sort String but " ++ show (getSort ctx s))
mkStrInRe ctx _ r | getSort ctx r /= SortRegex  = Left $ Error ("Second argument of At not of expected Sort Regex but " ++ show (getSort ctx r))
mkStrInRe _   s r                               = unsafeStrInRe (Right s) (Right r)

-- get ConstructorDef when possible
getCstr :: SortContext c => c -> RefByName ADTDef -> RefByName ConstructorDef -> Either Error (ADTDef, ConstructorDef)
getCstr ctx aRef cRef = case lookupADT (toName aRef) ctx of
                                Nothing   -> Left $ Error ("ADTDefinition " ++ show aRef ++ " not defined in context")
                                Just aDef -> case lookupConstructor (toName cRef) aDef of
                                                Nothing   -> Left $ Error ("Constructor " ++ show cRef ++ " not defined for ADTDefinition " ++ show aRef)
                                                Just cDef -> Right (aDef, cDef)
-- | Apply ADT Constructor of the given ADT Name and Constructor Name on the provided arguments (the list of value expressions).
mkCstr :: ValExprConstructionContext c => c -> RefByName ADTDef -> RefByName ConstructorDef -> [ValExpression] -> Either Error ValExpression
mkCstr ctx aRef cRef as = getCstr ctx aRef cRef >>= const (unsafeCstr aRef cRef (map Right as))

-- | Is the provided value expression made by the ADT constructor with the given ADT Name and Constructor Name?
mkIsCstr :: ValExprConstructionContext c => c -> RefByName ADTDef -> RefByName ConstructorDef -> ValExpression -> Either Error ValExpression
mkIsCstr ctx aRef cRef v = getCstr ctx aRef cRef >>= structuralIsCstr aRef cRef v

-- One time only check - will never change (since structural)
-- After type checking holds:
-- IsX(t::T) with T having only one constructor (X) <==> true
structuralIsCstr :: RefByName ADTDef -> RefByName ConstructorDef -> ValExpression -> (ADTDef, ConstructorDef) -> Either Error ValExpression
structuralIsCstr aRef cRef v (aDef,_) = case elemsConstructor aDef of
                                                    [_] -> Right trueValExpr
                                                    _   -> unsafeIsCstr aRef cRef (Right v)

-- | Access field made by ADT Constructor of the given ADT Name and Constructor Name on the provided argument.
mkAccess :: ValExprConstructionContext c => c -> RefByName ADTDef -> RefByName ConstructorDef -> RefByName FieldDef -> ValExpression -> Either Error ValExpression
mkAccess ctx aRef cRef fRef v = getCstr ctx aRef cRef >>= (\(_,cDef) -> case positionField (toName fRef) cDef of
                                                                            Just p  -> unsafeAccess aRef cRef (RefByIndex p) (Right v)
                                                                            Nothing -> Left $ Error ("FieldName " ++ show (toName fRef) ++ " not contained in constructor " ++ show (toName cRef) ++ " of ADTDefinition " ++ show (toName aRef))
                                                           )