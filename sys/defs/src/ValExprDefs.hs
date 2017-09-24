{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ValExprDefs
where

import qualified Data.Map        as Map
import           Data.Set        (Set)
import           Data.Text       (Text)

import           Control.DeepSeq
import           GHC.Generics    (Generic)

import           ConstDefs
import           CstrId
import           FreeMonoidX
import           FuncId
import           Product
import           Sum
import           VarId



-- ----------------------------------------------------------------------------------------- --
-- value expression

-- | ValExprView: the public view of value expression 'ValExpr'
data  ValExprView v = Vconst  Const
                    | Vvar    v
                    -- Boolean
                    | Vequal  (ValExpr v) (ValExpr v)
                    | Vite    {   condition   :: ValExpr v
                              ,   trueBranch  :: ValExpr v
                              ,   falseBranch :: ValExpr v
                              }
                    | Vnot    (ValExpr v)
                    | Vand    (Set (ValExpr v))
                    -- Int
                    | Vdivide     {   dividend :: ValExpr v
                                  ,   divisor  :: ValExpr v
                                  }
                    | Vmodulo     {   dividend :: ValExpr v
                                  ,   divisor  :: ValExpr v
                                  }
                    | Vsum      (FreeSum (ValExpr v))
                    | Vproduct  (Product (ValExpr v))
                    -- ADT
                    | Vcstr   CstrId [ValExpr v]
                    | Viscstr CstrId (ValExpr v)
                    | Vaccess CstrId Int (ValExpr v)

                    | Venv    (VarEnv v v) (ValExpr v)
                    | Vfunc   FuncId [ValExpr v]
                    | Vpredef PredefKind FuncId [ValExpr v]
                    | Verror  Text
     deriving (Eq, Ord, Read, Show, Generic, NFData)

instance Num (ValExpr v)
instance Enum (ValExpr v)
instance Ord v => Integral (ValExpr v)
instance Ord v => Real (ValExpr v)


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

data PredefKind     = AST     -- Algebraic To String
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
