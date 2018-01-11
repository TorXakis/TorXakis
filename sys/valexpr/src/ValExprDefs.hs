{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExprDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Data definitions file for Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ViewPatterns #-}
module ValExprDefs
( ValExprView(..)
, ValExpr(..)       -- for local usage only!
, eval
, PredefKind(..)
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.Set        (Set)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

-- import           ADTDef
import           ConstructorDef
import           ConstDefs
-- import           FieldDef
import           FuncId
import           Id
import           Product
import           Ref
import           Sum
import           StandardSortRefs
import           SortDef
import           SortOf
import           Variable

-- | ValExprView: the public view of value expression 'ValExpr'
data  ValExprView v = Vconst  Const
                    | Vvar    v
                    -- Boolean
                    | Vequal    (ValExpr v) (ValExpr v)
                    | Vite      {   condition   :: ValExpr v
                                ,   trueBranch  :: ValExpr v
                                ,   falseBranch :: ValExpr v
                                }
                    | Vnot      (ValExpr v)
                    | Vand      (Set (ValExpr v))
                    -- Int
                    | Vdivide   {   dividend :: ValExpr v
                                ,   divisor  :: ValExpr v
                                }
                    | Vmodulo   {   dividend :: ValExpr v
                                ,   divisor  :: ValExpr v
                                }
                    | Vsum      (FreeSum (ValExpr v))
                    | Vproduct  (FreeProduct (ValExpr v))
                    | Vgez      (ValExpr v)
                    -- String
                    | Vlength   (ValExpr v)
                    | Vat       {   string   :: ValExpr v
                                ,   position :: ValExpr v
                                }
                    | Vconcat   [ValExpr v]
                    -- Regex
                    | Vstrinre {    string :: ValExpr v
                               ,    regex  :: ValExpr v
                               }
                    -- ADT
                    | Vcstr   (TRef SortDef) (TRef ConstructorDef) [ValExpr v]
                    | Viscstr (TRef SortDef) (TRef ConstructorDef) (ValExpr v)
                    | Vaccess (TRef SortDef) (TRef ConstructorDef) Int (TRef SortDef) (ValExpr v)

                    | Vfunc   FuncId [ValExpr v]
                    | Vpredef PredefKind FuncId [ValExpr v]
                    | Verror  Text
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance (Ord v, Resettable v) => Resettable (ValExprView v)

-- These instances are needed to use the symbolic representation of sums and
-- products of val expressions. These instances have no implementation, which
-- means that if an attempt is made to compute the value of a sum or product of
-- `ValExpr` then a runtime error will occur. When GADT's are used it will be
-- possible to define instances for `ValExpr` of numeric types.
instance Num (ValExpr v)
    where
        (+)         = error "Symbolic ValExpr Num: +"
        (*)         = error "Symbolic ValExpr Num: *"
        abs         = error "Symbolic ValExpr Num: abs"
        signum      = error "Symbolic ValExpr Num: signum"
        fromInteger = error "Symbolic ValExpr Num: fromInteger"
        (-)         = error "Symbolic ValExpr Num: -"
instance Enum (ValExpr v)
    where
        toEnum   = error "Symbolic ValExpr Enum: toEnum"
        fromEnum = error "Symbolic ValExpr Enum: fromEnum"
instance Ord v => Real (ValExpr v)
    where
        toRational = error "Symbolic ValExpr Real: toRational"
instance Ord v => Integral (ValExpr v)
    where
        quotRem   = error "Symbolic ValExpr Integral: quotRem"
        toInteger = error "Symbolic ValExpr Integral: toInteger"

-- | ValExpr: value expression
--
-- 1. User can't directly construct ValExpr (such that invariants will always hold)
--
-- 2. User can still pattern match on ValExpr using 'ValExprView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype ValExpr v = ValExpr {
                        -- | View on value expression.
                        view :: ValExprView v }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance (Ord v, Resettable v) => Resettable (ValExpr v)

-- | Evaluate the provided value expression.
-- Either the Right Constant Value is returned or a (Left) error message.
eval :: Show v => ValExpr v -> Either String Const
eval = evalView . view

evalView :: Show v => ValExprView v -> Either String Const
evalView (Vconst v) = Right v
evalView x          = Left $ "Value Expression is not a constant value " ++ show x

-- | SortOf instance
instance (Variable v) => SortOf (ValExpr v) where
  sortOf vexp = let s = sortOf' vexp in
    if s == sortRefError
    then sortRefString
    else s

sortOf' :: (Variable v) => ValExpr v -> TRef SortDef
sortOf' (view -> Vfunc (FuncId _nm _uid _fa fs) _vexps)       = fs
sortOf' (view -> Vcstr adtRf _cstrRef _vexps)                 = adtRf
sortOf' (view -> Viscstr { })                                 = sortRefBool
sortOf' (view -> Vaccess _adtRf _cstrRf _pos sortRf _cstrVexp)= sortRf
sortOf' (view -> Vconst con)                                  = sortOf con
sortOf' (view -> Vvar v)                                      = vsort v
sortOf' (view -> Vite _cond vexp1 vexp2)                      = -- if the LHS is an error (Verror), we want to yield the type of the RHS which might be no error
                                                                  let sort' = sortOf' vexp1 in
                                                                  if sort' == sortRefError
                                                                    then sortOf' vexp2
                                                                    else sort'
sortOf' (view -> Vequal { })                                  = sortRefBool
sortOf' (view -> Vnot { })                                    = sortRefBool
sortOf' (view -> Vand { })                                    = sortRefBool
sortOf' (view -> Vsum { })                                    = sortRefInt
sortOf' (view -> Vproduct { })                                = sortRefInt
sortOf' (view -> Vmodulo { })                                 = sortRefInt
sortOf' (view -> Vdivide { })                                 = sortRefInt
sortOf' (view -> Vgez { })                                    = sortRefBool
sortOf' (view -> Vlength { })                                 = sortRefInt
sortOf' (view -> Vat { })                                     = sortRefString
sortOf' (view -> Vconcat { })                                 = sortRefString
sortOf' (view -> Vstrinre { })                                = sortRefBool
sortOf' (view -> Vpredef _kd (FuncId _nm _uid _fa fs) _vexps) = fs
sortOf' (view -> Vpredef{})                                   = error "sortOf': Unexpected Ident with Vpredef"
sortOf' (view -> Verror _str)                                 = sortRefError
sortOf' _                                                     = error "sortOf': All items must be in view"

-- | only needed for CNECTDEF
data PredefKind     = AST     -- Algebraic To String
                    | ASF     -- Algebraic From String
                    | AXT     -- Algebraic To Xml
                    | AXF     -- Algebraic From Xml
                    | SSB     -- Standard Sort Bool
                    | SSI     -- Standard Sort Int
                    | SSS     -- Standard Sort String
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Resettable PredefKind
