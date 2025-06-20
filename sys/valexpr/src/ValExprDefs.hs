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

import           Constant (Constant)
import           CstrId
import           FuncId
import           Id
import           Product
import           Sum
import           SortId
import           SortOf
import           Variable


-- ----------------------------------------------------------------------------------------- --
-- value expression

-- | ValExprView: the public view of value expression 'ValExpr'
data  ValExprView v = Vconst  Constant
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
                    | Vcstr   CstrId [ValExpr v]
                    | Viscstr CstrId (ValExpr v)
                    | Vaccess CstrId Text Int (ValExpr v)

                    | Vfunc   FuncId [ValExpr v]
                    | Vpredef PredefKind FuncId [ValExpr v]
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
eval :: Show v => ValExpr v -> Either String Constant
eval = evalView . view

evalView :: Show v => ValExprView v -> Either String Constant
evalView (Vconst v) = Right v
evalView x          = Left $ "Value Expression is not a constant value " ++ show x

-- | SortOf instance
instance (Variable v) => SortOf (ValExpr v) where
  sortOf = sortOf'

sortOf' :: (Variable v) => ValExpr v -> SortId
sortOf' (view -> Vfunc (FuncId _nm _uid _fa fs) _vexps)         = fs
sortOf' (view -> Vcstr (CstrId _nm _uid _ca cs) _vexps)         = cs
sortOf' (view -> Viscstr { })                                   = sortIdBool
sortOf' (view -> Vaccess (CstrId _nm _uid ca _cs) _n p _vexps)  = ca!!p
sortOf' (view -> Vconst con)                                    = sortOf con
sortOf' (view -> Vvar v)                                        = vsort v
sortOf' (view -> Vite _cond vexp1 _vexp2)                       = sortOf' vexp1
sortOf' (view -> Vequal { })                                    = sortIdBool
sortOf' (view -> Vnot { })                                      = sortIdBool
sortOf' (view -> Vand { })                                      = sortIdBool
sortOf' (view -> Vsum { })                                      = sortIdInt
sortOf' (view -> Vproduct { })                                  = sortIdInt
sortOf' (view -> Vmodulo { })                                   = sortIdInt
sortOf' (view -> Vdivide { })                                   = sortIdInt
sortOf' (view -> Vgez { })                                      = sortIdBool
sortOf' (view -> Vlength { })                                   = sortIdInt
sortOf' (view -> Vat { })                                       = sortIdString
sortOf' (view -> Vconcat { })                                   = sortIdString
sortOf' (view -> Vstrinre { })                                  = sortIdBool
sortOf' (view -> Vpredef _kd (FuncId _nm _uid _fa fs) _vexps)   = fs



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

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
