{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for ValExpr: all defined sorts, variables, and functions
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ValExpr.ValExprContext
( -- * ValExpr Context
  ValExprContext (..)
, MinimalValExprContext(MinimalValExprContext)
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap           as Map
import           GHC.Generics           (Generic)

import           TorXakis.Error         ( MinError )
import           TorXakis.Name          ( RefByName, toMapByName )
import           TorXakis.Sort          ( SortContext (..) , ADTDef, violationsAddAdtDefs )
import           TorXakis.VarDef        ( VarDef, MinimalVarDef )
import           TorXakis.FuncDef       ( FuncDef )
import           TorXakis.FuncSignature ( FuncSignature )


-- | A ValExprContext instance contains all definitions to work with sorts and references thereof
class (SortContext a, VarDef v) => ValExprContext a v where
    -- | Accessor for Variable Definitions
    varDefs :: a -> Map.Map (RefByName v) v

    -- | Add variable definitions to value expression context.
    --   A value expression context is returned when the following constraints are satisfied:
    --
    --   * The 'Name's of added variable definitions are unique
    --
    --   * All sorts of the added variables are known (within this context)
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    --
    -- Note that variables in the context are hidden when variables with the same names are added.
    addVarDefs :: a -> [v] -> Either MinError a

    -- | Accessor for Function Definitions
    funcDefs :: a -> Map.Map FuncSignature (FuncDef v)

    -- | Add function definitions to value expression context.
    --   A value expression context is returned when the following constraints are satisfied:
    --
    --   * The signatures of the function definitions are unique.
    --
    --   * All references (both Sort and FunctionDefinition) are known
    --
    --   * The sort of all bodys of the added Function Definitions is in agreement with the function signature (its return type).
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncDefs :: a -> [FuncDef v] -> Either MinError a


-- | A minimal instance of 'ValExprContext'.
data MinimalValExprContext = MinimalValExprContext { -- adt definitions
                                                     adtDefsToMap :: Map.Map (RefByName ADTDef) ADTDef 
                                                     -- var definitions
                                                   , varDefsToMap :: Map.Map (RefByName MinimalVarDef) MinimalVarDef
                                                     -- function definitions
                                                   , funcDefsToMap :: Map.Map FuncSignature (FuncDef MinimalVarDef)
                                                } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext MinimalValExprContext where
    empty = MinimalValExprContext Map.empty Map.empty Map.empty
    adtDefs = adtDefsToMap
    addAdtDefs context as = case violationsAddAdtDefs context as of
                                Just e  -> Left e
                                Nothing -> Right $ context { adtDefsToMap = Map.union (adtDefsToMap context) (toMapByName as) }

    
instance ValExprContext MinimalValExprContext MinimalVarDef where
    varDefs = varDefsToMap
    addVarDefs = undefined
    funcDefs = funcDefsToMap
    addFuncDefs = undefined
