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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.TestValExprContext
(-- * Test Sort Context
  TestFuncContext
, MinimalTestFuncContext(..)
, TestValExprContext
, MinimalTestValExprContext(..)
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as HashMap
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort (SortContext (..), elemSort)
import           TorXakis.TestSortContext
import           TorXakis.ValExprContext
import           TorXakis.VarContext
import           TorXakis.VarDef (VarDef, sort)

-------------------------------------------------------------------------------------------------------------
-- Test Func Context
-------------------------------------------------------------------------------------------------------------

-- | TestFuncContext
class TestSortContext a => TestFuncContext a

-- | A minimal instance of 'TestFuncContext'.
data MinimalTestFuncContext a = MinimalTestFuncContext 
                                    { testSortContext :: a
                                    , _funcDefs :: HashMap.Map FuncSignature FuncDef
                                    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext a => SortContext (MinimalTestFuncContext a) where
    empty             = MinimalTestFuncContext empty HashMap.empty
    adtDefs ctx       = adtDefs (testSortContext ctx)
    addAdtDefs ctx as = case addAdtDefs (testSortContext ctx) as of
                            Left e     -> Left e
                            Right tctx -> Right $ ctx { testSortContext = tctx }

instance TestSortContext a => TestSortContext (MinimalTestFuncContext a) where
    mapSortSize ctx              = mapSortSize (testSortContext ctx)
    mapAdtMapConstructorSize ctx = mapAdtMapConstructorSize (testSortContext ctx)

instance SortContext a => FuncContext (MinimalTestFuncContext a) where
    funcDefs = _funcDefs
    addFuncDefs = undefined -- TODO: add look functionality for generation of ValExpr

-- TODO what is needed additional in a Test generator to ensure termination while generating (recursive) functions?

instance TestSortContext a => TestFuncContext (MinimalTestFuncContext a)
-------------------------------------------------------------------------------------------------------------
-- Test Val Expr Context
-------------------------------------------------------------------------------------------------------------

-- | A TestValExprContext instance contains all definitions to work with value expressions (of course including sort)
--  and reference thereof for test purposes
class (VarContext a, TestFuncContext a) => TestValExprContext a

-- | A minimal instance of 'TestValExprContext'.
data MinimalTestValExprContext a = MinimalTestValExprContext 
                                    { testFuncContext :: a
                                    , _varDefs :: HashMap.Map (RefByName VarDef) VarDef
                                    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext a => SortContext (MinimalTestValExprContext a) where
    empty             = MinimalTestValExprContext empty HashMap.empty
    adtDefs ctx       = adtDefs (testFuncContext ctx)
    addAdtDefs ctx as = case addAdtDefs (testFuncContext ctx) as of
                            Left e     -> Left e
                            Right tctx -> Right $ ctx { testFuncContext = tctx }

instance TestSortContext a => TestSortContext (MinimalTestValExprContext a) where
    mapSortSize ctx              = mapSortSize (testFuncContext ctx)
    mapAdtMapConstructorSize ctx = mapAdtMapConstructorSize (testFuncContext ctx)

instance FuncContext a => FuncContext (MinimalTestValExprContext a) where
    funcDefs ctx = funcDefs (testFuncContext ctx)
    addFuncDefs ctx fds = case addFuncDefs (testFuncContext ctx) fds of
                            Left e     -> Left e
                            Right tctx -> Right $ ctx { testFuncContext = tctx }

instance SortContext a => VarContext (MinimalTestValExprContext a) where
    varDefs = _varDefs
    addVarDefs ctx vs
        | not $ null nuVarDefs               = Left $ MinError (T.pack ("Non unique variable definitions: " ++ show nuVarDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of variable definitions with undefined sorts: " ++ show undefinedSorts))
        | otherwise                          = Right $ ctx { _varDefs = HashMap.union (toMapByName vs) (varDefs ctx)}
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . elemSort ctx . sort) vs


instance TestFuncContext a => TestFuncContext (MinimalTestValExprContext a)

instance TestFuncContext a => TestValExprContext (MinimalTestValExprContext a)