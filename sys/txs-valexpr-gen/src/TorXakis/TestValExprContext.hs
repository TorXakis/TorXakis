{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestValExprContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Sort Context for Test: 
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.TestValExprContext
(-- * Test Sort Context
  TestValExprContext
, MinimalTestValExprContext(..)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as Map
import           GHC.Generics        (Generic)

import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.TestSortContext
import           TorXakis.ValExpr
import           TorXakis.VarDef

-- | A TestValExprContext instance contains all definitions to work with value expressions (of course including sort)
--  and reference thereof for test purposes
class (ValExprContext a v, TestSortContext a) => TestValExprContext a v

-- | A minimal instance of 'TestValExprContext'.
data MinimalTestValExprContext = MinimalTestValExprContext 
                                    { testSortContext :: MinimalTestSortContext
                                    , _varDefs :: Map.Map (RefByName MinimalVarDef) MinimalVarDef
                                      -- TODO add sort to var mapping?
                                    , _funcDefs :: Map.Map FuncSignature (FuncDef MinimalVarDef)
                                    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext MinimalTestValExprContext where
    empty             = MinimalTestValExprContext empty Map.empty Map.empty
    adtDefs ctx       = adtDefs (testSortContext ctx)
    addAdtDefs ctx as = case addAdtDefs (testSortContext ctx) as of
                            Left e     -> Left e
                            Right tctx -> Right $ ctx { testSortContext = tctx }

instance TestSortContext MinimalTestValExprContext where
    mapSortSize ctx              = mapSortSize (testSortContext ctx)
    mapAdtMapConstructorSize ctx = mapAdtMapConstructorSize (testSortContext ctx)

instance ValExprContext MinimalTestValExprContext MinimalVarDef where
    varDefs = _varDefs
    addVarDefs = undefined
    funcDefs = _funcDefs
    addFuncDefs = undefined

instance TestValExprContext MinimalTestValExprContext MinimalVarDef