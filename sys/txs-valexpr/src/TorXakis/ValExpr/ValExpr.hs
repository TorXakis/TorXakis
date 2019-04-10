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
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ValExpr.ValExpr
( -- * Value Expression
  ValExpressionView (..)
, ValExpression (..)
  -- * Evaluate
, eval
  -- dependencies, yet part of interface
, FreeVars (..)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.RefByIndex
import           TorXakis.Sort
import           TorXakis.Value
import           TorXakis.Var
import           TorXakis.VarContext

-- | ValExpressionView: the public view of value expression 'ValExpression'
-- Should we change to Either Error ValExpression to increase laziness (e.g. in ITE, And, concat, func, and cstr)?
data ValExpressionView = Vconst    Value
                       | Vvar      (RefByName VarDef)
                       -- generic
                       | Vequal    ValExpression
                                   ValExpression
                       | Vite      ValExpression
                                   ValExpression
                                   ValExpression
                       | Vfunc     RefByFuncSignature [ValExpression]
                       | Vpredef   RefByFuncSignature [ValExpression]
                       -- Boolean
                       | Vnot      ValExpression
                       | Vand      (Set.Set ValExpression)
                       -- Int
                       | Vdivide   ValExpression
                                   ValExpression
                       | Vmodulo   ValExpression
                                   ValExpression
                       | Vsum      (Map.Map ValExpression Integer)
                       | Vproduct  (Map.Map ValExpression Integer)
                       | Vgez      ValExpression
                       -- String
                       | Vlength   ValExpression
                       | Vat       ValExpression
                                   ValExpression
                       | Vconcat   [ValExpression]
                       -- Regex
                       | Vstrinre  ValExpression
                                   ValExpression
                       -- ADT
                       | Vcstr     (RefByName ADTDef) (RefByName ConstructorDef) [ValExpression]
                       | Viscstr   (RefByName ADTDef) (RefByName ConstructorDef) ValExpression
                       | Vaccess   (RefByName ADTDef) (RefByName ConstructorDef) (RefByIndex FieldDef) ValExpression
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | ValExpression: value expression
--
-- 1. User can't directly construct ValExpression (such that invariants will always hold)
--
-- 2. User can still pattern match on ValExpression using 'ValExpressionView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype ValExpression = ValExpression { -- | View on value expression.
                                          view :: ValExpressionView
                                        }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Evaluate the provided value expression.
-- Either the Right Constant Value is returned or a (Left) error message.
eval :: ValExpression -> Either Error Value
eval = evalView . view
  where
    evalView :: ValExpressionView -> Either Error Value
    evalView (Vconst v) = Right v
    evalView x          = Left $ Error ("Value Expression is not a constant value " ++ show x)

-- | SortOf instance
instance VarContext c => HasSort c ValExpression where
  getSort c = getSort c . TorXakis.ValExpr.ValExpr.view

instance VarContext c => HasSort c ValExpressionView where
    getSort ctx (Vconst val)              = getSort ctx val
    getSort ctx (Vvar r)                  = case lookupVar (toName r) ctx of
                                               Nothing -> error ("getSort: VarDef not found in context " ++ show r)
                                               Just v  -> getSort ctx v
    getSort _    Vequal { }               = SortBool
    getSort ctx (Vite _cond vexp1 _vexp2) = getSort ctx vexp1
    getSort _   (Vfunc r _vexps)          = returnSort (toFuncSignature r)
    getSort _   (Vpredef r _vexps)        = returnSort (toFuncSignature r)
    getSort _    Vnot { }                 = SortBool
    getSort _    Vand { }                 = SortBool
    getSort _    Vdivide { }              = SortInt
    getSort _    Vmodulo { }              = SortInt
    getSort _    Vsum { }                 = SortInt
    getSort _    Vproduct { }             = SortInt
    getSort _    Vgez { }                 = SortBool
    getSort _    Vlength { }              = SortInt
    getSort _    Vat { }                  = SortString
    getSort _    Vconcat { }              = SortString
    getSort _    Vstrinre { }             = SortBool
    getSort _   (Vcstr a _c _vexps)       = SortADT a
    getSort _    Viscstr { }              = SortBool
    getSort ctx (Vaccess a c p _vexps)    = case lookupADT (toName a) ctx of
                                               Nothing   -> error ("getSort: ADTDef not found in context " ++ show a)
                                               Just aDef -> case lookupConstructor (toName c) aDef of
                                                               Nothing   -> error ("getSort: Constructor not found in ADTDef " ++ show c)
                                                               Just cDef -> getSort ctx ( elemsField cDef !! toIndex p )

instance VarContext c => UsedSorts c ValExpression where
    usedSorts ctx = usedSorts ctx . view

instance VarContext c => UsedSorts c ValExpressionView where
    -- we add return types that are not necessarily in the arguments
    usedSorts ctx (Vconst c)          = usedSorts ctx c
    usedSorts ctx (Vvar r)            = case lookupVar (toName r) ctx of
                                               Nothing -> error ("usedSorts: VarDef not found in context " ++ show r)
                                               Just v  -> usedSorts ctx v
    usedSorts ctx (Vequal v1 v2)      = Set.insert SortBool $ Set.unions (map (usedSorts ctx) [v1, v2])
    usedSorts ctx (Vite c t f)        = Set.unions $ map (usedSorts ctx) [c, t, f]
    usedSorts ctx (Vfunc fs as)       = Set.unions (usedSorts ctx (toFuncSignature fs) : map (usedSorts ctx) as)
    usedSorts ctx (Vpredef fs as)     = Set.unions (usedSorts ctx (toFuncSignature fs) : map (usedSorts ctx) as)
    usedSorts ctx (Vnot v)            = usedSorts ctx v
    usedSorts ctx (Vand s)            = Set.unions $ map (usedSorts ctx) (Set.toList s)
    usedSorts ctx (Vdivide t n)       = Set.unions $ map (usedSorts ctx) [t, n]
    usedSorts ctx (Vmodulo t n)       = Set.unions $ map (usedSorts ctx) [t, n]
    usedSorts ctx (Vsum m)            = Set.unions $ map (usedSorts ctx) (Map.keys m)
    usedSorts ctx (Vproduct m)        = Set.unions $ map (usedSorts ctx) (Map.keys m)
    usedSorts ctx (Vgez v)            = Set.insert SortBool $ usedSorts ctx v
    usedSorts ctx (Vlength v)         = Set.insert SortInt $ usedSorts ctx v
    usedSorts ctx (Vat s p)           = Set.unions $ map (usedSorts ctx) [s, p]
    usedSorts ctx (Vconcat vs)        = Set.unions $ map (usedSorts ctx) vs
    usedSorts ctx (Vstrinre s r)      = Set.insert SortBool $ Set.unions (map (usedSorts ctx) [s, r])
    usedSorts ctx (Vcstr a _ vs)      = Set.insert (SortADT a) $ Set.unions (map (usedSorts ctx) vs)
    usedSorts ctx (Viscstr _ _ v)     = Set.insert SortBool $ usedSorts ctx v
    usedSorts ctx a@(Vaccess _ _ _ v) = Set.insert (getSort ctx a) $ usedSorts ctx v

instance FreeVars ValExpression where
    freeVars = freeVars . view

instance FreeVars ValExpressionView where
    freeVars  Vconst{}         = Set.empty
    freeVars (Vvar v)          = Set.singleton v
    freeVars (Vequal v1 v2)    = Set.unions $ map freeVars [v1, v2]
    freeVars (Vite c t f)      = Set.unions $ map freeVars [c, t, f]
    freeVars (Vfunc _ as)      = Set.unions $ map freeVars as
    freeVars (Vpredef _ as)    = Set.unions $ map freeVars as
    freeVars (Vnot v)          = freeVars v
    freeVars (Vand s)          = Set.unions $ map freeVars (Set.toList s)
    freeVars (Vdivide t n)     = Set.unions $ map freeVars [t, n]
    freeVars (Vmodulo t n)     = Set.unions $ map freeVars [t, n]
    freeVars (Vsum m)          = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vproduct m)      = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vgez v)          = freeVars v
    freeVars (Vlength v)       = freeVars v
    freeVars (Vat s p)         = Set.unions $ map freeVars [s, p]
    freeVars (Vconcat vs)      = Set.unions $ map freeVars vs
    freeVars (Vstrinre s r)    = Set.unions $ map freeVars [s, r]
    freeVars (Vcstr _ _ vs)    = Set.unions $ map freeVars vs
    freeVars (Viscstr _ _ v)   = freeVars v
    freeVars (Vaccess _ _ _ v) = freeVars v

instance UsedFuncSignatures ValExpression where
    usedFuncSignatures = usedFuncSignatures . view

instance UsedFuncSignatures ValExpressionView where
    usedFuncSignatures  Vconst{}         = Set.empty
    usedFuncSignatures  Vvar{}           = Set.empty
    usedFuncSignatures (Vequal v1 v2)    = Set.unions $ map usedFuncSignatures [v1, v2]
    usedFuncSignatures (Vite c t f)      = Set.unions $ map usedFuncSignatures [c, t, f]
    usedFuncSignatures (Vfunc fs as)     = Set.insert (toFuncSignature fs) $ Set.unions (map usedFuncSignatures as)
    usedFuncSignatures (Vpredef fs as)   = Set.insert (toFuncSignature fs) $ Set.unions (map usedFuncSignatures as)
    usedFuncSignatures (Vnot v)          = usedFuncSignatures v
    usedFuncSignatures (Vand s)          = Set.unions $ map usedFuncSignatures (Set.toList s)
    usedFuncSignatures (Vdivide t n)     = Set.unions $ map usedFuncSignatures [t, n]
    usedFuncSignatures (Vmodulo t n)     = Set.unions $ map usedFuncSignatures [t, n]
    usedFuncSignatures (Vsum m)          = Set.unions $ map usedFuncSignatures (Map.keys m)
    usedFuncSignatures (Vproduct m)      = Set.unions $ map usedFuncSignatures (Map.keys m)
    usedFuncSignatures (Vgez v)          = usedFuncSignatures v
    usedFuncSignatures (Vlength v)       = usedFuncSignatures v
    usedFuncSignatures (Vat s p)         = Set.unions $ map usedFuncSignatures [s, p]
    usedFuncSignatures (Vconcat vs)      = Set.unions $ map usedFuncSignatures vs
    usedFuncSignatures (Vstrinre s r)    = Set.unions $ map usedFuncSignatures [s, r]
    usedFuncSignatures (Vcstr _ _ vs)    = Set.unions $ map usedFuncSignatures vs
    usedFuncSignatures (Viscstr _ _ v)   = usedFuncSignatures v
    usedFuncSignatures (Vaccess _ _ _ v) = usedFuncSignatures v

-- TODO: add UsedNames / UsedFuncNames to find usage of keywords / reserved words
--       Only defined names are used -> so unnecessary to check in ValExpr?