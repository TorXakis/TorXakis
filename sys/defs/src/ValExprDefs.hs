{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ValExprDefs
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import GHC.Generics (Generic)
import Control.DeepSeq

import ConstDefs
import CstrId
import FuncId
import VarId


-- ----------------------------------------------------------------------------------------- --
-- value expression

-- | ValExprView: the public view of value expression 'ValExpr'
data  ValExprView v = Vfunc   FuncId [ValExpr v]
                    | Vcstr   CstrId [ValExpr v]
                    | Viscstr CstrId (ValExpr v)
                    | Vaccess CstrId Int (ValExpr v)
                    | Vconst  Const
                    | Vvar    v
                    | Vite    [ValExpr v] (ValExpr v) (ValExpr v)
                    | Venv    (VarEnv v v) (ValExpr v)
                    | Vequal  (ValExpr v) (ValExpr v)
                    | Vnot    (ValExpr v)
                    | Vand    (Set (ValExpr v))
                    | Vpredef PredefKind FuncId [ValExpr v]
                    | Verror  String
     deriving (Eq,Ord,Read,Show, Generic, NFData)
     
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
  deriving (Eq, Ord, Read, Show, Generic, NFData)

data PredefKind     = AFS     -- Algebraic Field Selector
                    | ACC     -- Algebraic Constructor Check
                    | ANE     -- Algebraic Non-Equality
                    | AST     -- Algebraic To String
                    | ASF     -- Algebraic From String
                    | AXT     -- Algebraic To Xml
                    | AXF     -- Algebraic From Xml
                    | SSB     -- Standard Sort Bool
                    | SSI     -- Standard Sort Int
                    | SSS     -- Standard Sort String
                    | SSR     -- Standard Sort Regex
     deriving (Eq,Ord,Read,Show, Generic, NFData)


type  VarEnv v w    =  Map.Map v (ValExpr w)     -- simultaneous substitution
                                                 -- all variables different
                                                 -- non-recursive


type  VExpr         =  ValExpr VarId


type  VEnv          =  VarEnv VarId VarId

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --