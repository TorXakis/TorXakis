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
import qualified TorXakis.GenCollection
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

division :: TestValExprContext a => a -> Gen (ValExpression, ValExpression)
division ctx = do
    n <- getSize
    teller <- resize (n-2) (arbitraryValExprOfSort ctx SortInt)
    noemer <- resize (n-2) (nonZero ctx)
    return (teller, noemer)

genValExprModulo :: TestValExprContext a => a -> Gen ValExpression
genValExprModulo ctx = do
    (teller, noemer) <- division ctx
    case mkModulo ctx teller noemer of
         Left e  -> error ("genValExprModulo constructor fails " ++ show e)
         Right x -> return x

genValExprDivide :: TestValExprContext a => a -> Gen ValExpression
genValExprDivide ctx = do
    (teller, noemer) <- division ctx
    case mkDivide ctx teller noemer of
         Left e  -> error ("genValExprDivide constructor fails " ++ show e)
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
                                    , _genMap :: TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
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
                                      ( addSuccess SortBool   0 (genValExprValueOfSort SortBool)
                                      $ addSuccess SortBool   1 genValExprNot
                                      $ addSuccess SortInt    0 (genValExprValueOfSort SortInt)
                                      $ addSuccess SortInt    2 genValExprModulo
                                      $ addSuccess SortInt    2 genValExprDivide
                                      $ addSuccess SortChar   0 (genValExprValueOfSort SortChar)
                                      $ addSuccess SortString 0 (genValExprValueOfSort SortString)
                                      $ addSuccess SortRegex  0 (genValExprValueOfSort SortRegex)
                                        TorXakis.GenCollection.empty
                                      )
                                      HashMap.empty
        where
             addSuccess :: Sort 
                        -> Int 
                        -> (MinimalTestValExprContext -> Gen ValExpression) 
                        -> TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
                        -> TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
             addSuccess s n g c = let ctx = MinimalTestValExprContext empty TorXakis.GenCollection.empty HashMap.empty
                                    in 
                                        case TorXakis.GenCollection.add ctx s n g c of
                                            Left e -> error ("empty - successful add expected, yet " ++ show e)
                                            Right c' -> c'

    adtDefs ctx    = adtDefs (testSortContext ctx)
    addADTs ctx as = case addADTs (testSortContext ctx) as of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx { _genMap = addValueGens (_genMap ctx) as
                                                    , testSortContext = vctx
                                                    }
        where
            addValueGens :: TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
                        -> [ADTDef]
                        -> TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
            addValueGens = foldl addValueGen
                where
                    addValueGen :: TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
                                -> ADTDef
                                -> TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
                    addValueGen m a = let srt = SortADT (toRefByName a)
                                        in
                                          case TorXakis.GenCollection.add ctx srt 0 (genValExprValueOfSort srt) m of
                                            Left e -> error ("addADTs - successful add expected, yet " ++ show e)
                                            Right c -> c

instance TestSortContext MinimalTestValExprContext where
    mapSortSize ctx                 = mapSortSize (testSortContext ctx)
    mapAdtMapConstructorSize ctx    = mapAdtMapConstructorSize (testSortContext ctx)

instance VarContext MinimalTestValExprContext where
    varDefs = _varDefs
    addVars ctx vs
        | not $ null nuVarDefs               = Left $ MinError (T.pack ("Non unique variable definitions: " ++ show nuVarDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of variable definitions with undefined sorts: " ++ show undefinedSorts))
        | otherwise                          = Right $ ctx { _genMap = addVarGen vs (_genMap ctx)
                                                           , _varDefs = HashMap.union (toMapByName vs) (varDefs ctx)
                                                           }
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . memberSort ctx . TorXakis.VarDef.sort) vs

        addVarGen :: [VarDef]
                  -> TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
                  -> TorXakis.GenCollection.GenCollection MinimalTestValExprContext ValExpression
        addVarGen []       m = m
        addVarGen xs@(x:_) m = let t = TorXakis.VarDef.sort x
                                   (ts,os) = List.partition (\v -> TorXakis.VarDef.sort v == t) xs
                                 in
                                   case TorXakis.GenCollection.add ctx t 0 ( genValExprVar (map toRefByName ts) ) m of
                                            Left e -> error ("addVars - successful add expected, yet " ++ show e)
                                            Right c -> addVarGen os c

instance TestValExprContext MinimalTestValExprContext where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case TorXakis.GenCollection.get (_genMap ctx) s n of
            [] -> error ("No Generators for " ++ show s ++ " at " ++ show n)
            xs -> do
                    generator <- elements xs
                    generator ctx


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
    addADTs ctx as = case addADTs (testSortContext ctx) as of
                            Left e     -> Left e
                            Right tctx -> Right $ ctx { testSortContext = tctx }

instance TestSortContext a => TestSortContext (MinimalTestFuncContext a) where
    mapSortSize ctx              = mapSortSize (testSortContext ctx)
    mapAdtMapConstructorSize ctx = mapAdtMapConstructorSize (testSortContext ctx)

instance SortContext a => FuncContext (MinimalTestFuncContext a) where
    funcDefs = _funcDefs
    addFuncDefs = undefined -- TODO: add look functionality for generation of ValExpression

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
    addADTs ctx as = case addADTs (testFuncContext ctx) as of
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
    addVars ctx vs
        | not $ null nuVarDefs               = Left $ MinError (T.pack ("Non unique variable definitions: " ++ show nuVarDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of variable definitions with undefined sorts: " ++ show undefinedSorts))
        | otherwise                          = Right $ ctx { _varDefs = HashMap.union (toMapByName vs) (varDefs ctx)}
      where
        nuVarDefs :: [VarDef]
        nuVarDefs = repeatedByName vs

        undefinedSorts :: [VarDef]
        undefinedSorts = filter (not . memberSort ctx . sort) vs


instance TestFuncContext a => TestFuncContext (MinimalTestValExprContext a)

instance TestFuncContext a => TestValExprContext (MinimalTestValExprContext a)
-}