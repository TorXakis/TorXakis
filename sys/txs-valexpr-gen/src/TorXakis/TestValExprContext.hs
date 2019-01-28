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
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.TestValExprContext
(-- * Test Sort Context
  TestValExprContext(..)
, MinimalTestValExprContext(..)
)
where
--import           Debug.Trace

import qualified Data.HashMap        as HashMap
import qualified Data.List           as List
import qualified Data.Text           as T
import           Test.QuickCheck

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortGen
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.ValueGen
import           TorXakis.VarContext
import           TorXakis.VarDef

-------------------------------------------------------------------------------
-- Generic Generators
-------------------------------------------------------------------------------
genValExprVar :: TestValExprContext a => [RefByName VarDef] -> a -> Gen ValExpression
genValExprVar vs ctx = do
    v <- elements vs
    case mkVar ctx v of
        Left e  -> error ("genValExprVar constructor with " ++ show v ++ " fails " ++ show e)
        Right x -> return x

genValExprValueOfSort :: TestValExprContext a => Sort -> a -> Gen ValExpression
genValExprValueOfSort s ctx = do
    v <- arbitraryValueOfSort ctx s
    case mkConst ctx v of
        Left e  -> error ("genValExprValueOfSort constructor with value " ++ show v ++ " of sort " ++ show s ++ " fails " ++ show e)
        Right x -> return x
-------------------------------------------------------------------------------
-- Boolean Generators
-------------------------------------------------------------------------------
genValExprNot :: TestValExprContext a => a -> Gen ValExpression
genValExprNot ctx = do
    n <- getSize
    arg <- resize (n-1) (arbitraryValExprOfSort ctx SortBool)
    case mkNot ctx arg of
         Left e  -> error ("genValExprNot constructor fails " ++ show e)
         Right x -> return x
-------------------------------------------------------------------------------
-- Integer Generators
-------------------------------------------------------------------------------


nonZero :: TestValExprContext a => a -> Gen ValExpression
nonZero ctx = do
    n <- arbitraryValExprOfSort ctx SortInt
    case view n of
        Vconst (Cint 0) -> nonZero ctx
        _               -> return n

genValExprModulo :: TestValExprContext a => a -> Gen ValExpression
genValExprModulo ctx = do
    n <- getSize
    teller <- resize (n-2) (arbitraryValExprOfSort ctx SortInt)
    noemer <- resize (n-2) (nonZero ctx)
    case mkModulo ctx teller noemer of
         Left e  -> error ("genValExprNot constructor fails " ++ show e)
         Right x -> return x
-------------------------------------------------------------------------------------------------------------
-- Test Func Context
-------------------------------------------------------------------------------------------------------------

class (TestSortContext a, VarContext a)
      --, TestFuncSignatureContext a) 
      => TestValExprContext a where
    arbitraryValExpr :: a -> Gen ValExpression
    arbitraryValExpr ctx = do
        s <- arbitrarySort ctx
        arbitraryValExprOfSort ctx s
    arbitraryValExprOfSort :: a -> Sort -> Gen ValExpression

data MinimalTestValExprContext = MinimalTestValExprContext 
                                    { testSortContext :: MinimalTestSortContext
                                    , _genMap :: HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
                                    -- to be added
                                    -- 0. predefined operators (modulo, and, not, sum , etc)
                                    -- 1. value generation for all defined sorts
                                    -- 2. generators related to ADTs (constructors and accessors)
                                    -- 3. If THEN Else etc.
                                    -- 4. generators for variables (based on variables defined in context)
                                    -- 5. generators for funcInstantiation (based on funcSignatures defined in context)

                                    , _varDefs :: HashMap.Map (RefByName VarDef) VarDef
                                    }

instance SortContext MinimalTestValExprContext where
    empty = MinimalTestValExprContext empty
                                      ( HashMap.fromList [ (SortBool, HashMap.fromList [ (0, genValExprValueOfSort SortBool)
                                                                                       , (1, genValExprNot)
                                                                                       ])
                                                         , (SortInt, HashMap.fromList [ (0, genValExprValueOfSort SortInt)
                                                                                      , (2, genValExprModulo)
                                                                                      ])
                                                         , (SortChar, HashMap.fromList [ (0, genValExprValueOfSort SortChar)
                                                                                       ])
                                                         , (SortString, HashMap.fromList [ (0, genValExprValueOfSort SortString)
                                                                                         ])
                                                         , (SortRegex, HashMap.fromList [ (0, genValExprValueOfSort SortRegex)
                                                                                        ])
                                                         ]
                                      )
                                      HashMap.empty
    adtDefs ctx    = adtDefs (testSortContext ctx)
    addAdtDefs ctx as = case addAdtDefs (testSortContext ctx) as of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx { _genMap = addValueGens (_genMap ctx) as
                                                    , testSortContext = vctx
                                                    }
        where
            addValueGens :: HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
                        -> [ADTDef]
                        -> HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
            addValueGens = foldl addValueGen
                where
                    addValueGen :: HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
                                -> ADTDef
                                -> HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
                    addValueGen m a = let srt = SortADT (RefByName (adtName a))
                                          hm :: HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression)
                                          hm = HashMap.fromList [(0, genValExprValueOfSort srt)]
                                        in
                                          case HashMap.lookup srt m of
                                            Just _  -> error ("ADT " ++ show a ++ " added yet sort already present " ++ show srt)
                                            Nothing -> HashMap.insert srt hm m


instance TestSortContext MinimalTestValExprContext where
    mapSortSize ctx                 = mapSortSize (testSortContext ctx)
    mapAdtMapConstructorSize ctx    = mapAdtMapConstructorSize (testSortContext ctx)

instance VarContext MinimalTestValExprContext where
    varDefs = _varDefs
    addVarDefs ctx vs
        | not $ null nuVarDefs               = Left $ MinError (T.pack ("Non unique variable definitions: " ++ show nuVarDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of variable definitions with undefined sorts: " ++ show undefinedSorts))
        | otherwise                          = Right $ ctx { _genMap = addVarGen vs (_genMap ctx)
                                                           , _varDefs = HashMap.union (toMapByName vs) (varDefs ctx)
                                                           }
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . elemSort ctx . TorXakis.VarDef.sort) vs

        addVarGen :: [VarDef]
                  -> HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
                  -> HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
        addVarGen []       m = m
        addVarGen xs@(x:_) m = let t = TorXakis.VarDef.sort x
                                   (ts,os) = List.partition (\v -> TorXakis.VarDef.sort v == t) xs
                                   oldSortMap = HashMap.findWithDefault HashMap.empty t m -- TODO: when all sort have a value generator the default is NOT needed!
                                   newMap :: HashMap.Map Sort (HashMap.Map Int (MinimalTestValExprContext -> Gen ValExpression))
                                   newMap = HashMap.insert t (HashMap.insert 0 (genValExprVar (map (RefByName . name) ts)) oldSortMap) m -- TODO: overwrites all generators with zero zero... should be added!!!!
                                 in
                                    addVarGen os newMap

instance TestValExprContext MinimalTestValExprContext where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case HashMap.lookup s (_genMap ctx) of
            Nothing -> error "arbitraryValExprOfSort: Sort not in HashMap"
            Just m  -> let gens = HashMap.elems (HashMap.filterWithKey (\k _ -> k <= n) m) in do
                            generator <- elements gens
                            value <- generator ctx
                            return value
    
{-    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
class (VarContext a, TestFuncSignatureContext a) => TestValExprContext a

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
-}