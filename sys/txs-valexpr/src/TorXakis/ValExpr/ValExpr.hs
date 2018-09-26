{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass     #-}
module TorXakis.ValExpr.ValExpr
( -- * Value Expression
  ValExprView (..)
, ValExpr (..)
  -- * Evaluate
, eval
, PredefKind(..)
)
where
import           Control.DeepSeq (NFData)
import           Data.Data           (Data)
import           Data.Set
import           GHC.Generics    (Generic)


import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Value
import           TorXakis.VarDef

-- | ValExprView: the public view of value expression 'ValExpr'
data ValExprView v = Vconst    Value
                   | Vvar      v                                                                        -- Sort is stored to prevent lookup in context
                   -- Boolean
                   | Vequal    (ValExpr v)
                               (ValExpr v)
                   | Vite      (ValExpr v)
                               (ValExpr v)
                               (ValExpr v)
                   | Vnot      (ValExpr v)
                   | Vand      (Set (ValExpr v))
                   -- Int
                   | Vdivide   (ValExpr v)
                               (ValExpr v)
                   | Vmodulo   (ValExpr v)
                               (ValExpr v)
                   -- | Vsum      (FreeSum (ValExpr v))
                   -- | Vproduct  (FreeProduct (ValExpr v))
                   | Vgez      (ValExpr v)
                   -- String
                   | Vlength   (ValExpr v)
                   | Vat       (ValExpr v)
                               (ValExpr v)
                   | Vconcat   [ValExpr v]
                   -- Regex
                   | Vstrinre  (ValExpr v)
                               (ValExpr v)
                   -- ADT
                   | Vcstr     (RefByName ADTDef) (RefByName ConstructorDef) [ValExpr v]
                   | Viscstr   (RefByName ADTDef) (RefByName ConstructorDef) (ValExpr v)
                   | Vaccess   (RefByName ADTDef) (RefByName ConstructorDef) Sort Int (ValExpr v)       -- Sort is stored to prevent lookup in context
                
                   | Vfunc     FuncSignature [ValExpr v]
                   | Vpredef   PredefKind FuncSignature [ValExpr v]
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | ValExpr: value expression
--
-- 1. User can't directly construct ValExpr (such that invariants will always hold)
--
-- 2. User can still pattern match on ValExpr using 'ValExprView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype ValExpr v = ValExpr { -- | View on value expression.
                              view :: ValExprView v
                            }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Evaluate the provided value expression.
-- Either the Right Constant Value is returned or a (Left) error message.
eval :: Show v => ValExpr v -> Either String Value
eval = evalView . view

evalView :: Show v => ValExprView v -> Either String Value
evalView (Vconst v) = Right v
evalView x          = Left $ "Value Expression is not a constant value " ++ show x


-- | SortOf instance
instance (VarDef v) => HasSort (ValExpr v) where
  getSort = sortOf . view

sortOf :: (VarDef v) => ValExprView v -> Sort
sortOf (Vconst val)                                   = getSort val
sortOf (Vvar v)                                       = getSort v
sortOf  Vequal { }                                    = SortBool
sortOf (Vite _cond vexp1 _vexp2)                      = getSort vexp1
sortOf  Vnot { }                                      = SortBool
sortOf  Vand { }                                      = SortBool
sortOf  Vdivide { }                                   = SortInt
sortOf  Vmodulo { }                                   = SortInt
--sortOf  Vsum { }                                      = SortInt
--sortOf  Vproduct { }                                  = SortInt
sortOf  Vgez { }                                      = SortBool
sortOf  Vlength { }                                   = SortInt
sortOf  Vat { }                                       = SortString
sortOf  Vconcat { }                                   = SortString
sortOf  Vstrinre { }                                  = SortBool
sortOf (Vcstr a _c _vexps)                            = SortADT a
sortOf  Viscstr { }                                   = SortBool
sortOf (Vaccess _a _c s _p _vexps)                    = s
sortOf (Vfunc fs _vexps)                              = returnSort fs
sortOf (Vpredef _kd fs _vexps)                        = returnSort fs

-- | only needed for CNECTDEF 
-- Function from Values to Value
data PredefKind     = AST     -- Algebraic To String
                    | ASF     -- Algebraic From String
                    | AXT     -- Algebraic To Xml
                    | AXF     -- Algebraic From Xml
                    | SSB     -- Standard Sort Bool
                    | SSI     -- Standard Sort Int
                    | SSS     -- Standard Sort String
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)