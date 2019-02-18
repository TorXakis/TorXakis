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
, ContextTestValExpr(..)
)
where
--import           Debug.Trace

import qualified Data.List           as List
import qualified Data.Text           as T
import           Test.QuickCheck

import           TorXakis.Error
import qualified TorXakis.GenCollection
import           TorXakis.Name
import           TorXakis.RefMap
import           TorXakis.Sort
import           TorXakis.SortGen
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.ValueGen
import           TorXakis.VarContext
import           TorXakis.Var

-------------------------------------------------------------------------------
-- Generic Generators
-------------------------------------------------------------------------------
genValExprVar :: TestValExprContext a => [Ref VarDef] -> a -> Gen ValExpression
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
    let available = n - 2 in do -- distrubte available size over two intervals
        t <- choose (0, available)
        teller <- resize t             (arbitraryValExprOfSort ctx SortInt)
        noemer <- resize (available-t) (nonZero ctx)
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

class (TestFuncSignatureContext a, VarContext a)
      => TestValExprContext a where
    arbitraryValExpr :: a -> Gen ValExpression
    arbitraryValExpr ctx = do
        s <- arbitrarySort ctx
        arbitraryValExprOfSort ctx s
    arbitraryValExprOfSort :: a -> Sort -> Gen ValExpression

data ContextTestValExpr = ContextTestValExpr 
                                    { testSortContext :: ContextTestSort
                                    , _genMap :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                                    -- to be added
                                    -- 0. predefined operators (modulo, and, not, sum , etc)
                                    -- 1. value generation for all defined sorts
                                    -- 2. generators related to ADTs (constructors and accessors)
                                    -- 3. If THEN Else etc.
                                    -- 4. generators for variables (based on variables defined in context)
                                    -- 5. generators for funcInstantiation (based on funcSignatures defined in context)
                                    , varDefs :: RefMap VarDef
                                    }

initialGenMap :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
initialGenMap =   addSuccess SortBool   0 (genValExprValueOfSort SortBool)
                $ addSuccess SortBool   1 genValExprNot
                $ addSuccess SortInt    0 (genValExprValueOfSort SortInt)
                $ addSuccess SortInt    2 genValExprModulo
                $ addSuccess SortInt    2 genValExprDivide
                $ addSuccess SortChar   0 (genValExprValueOfSort SortChar)
                $ addSuccess SortString 0 (genValExprValueOfSort SortString)
                $ addSuccess SortRegex  0 (genValExprValueOfSort SortRegex)
                  TorXakis.GenCollection.empty
    where
         addSuccess :: Sort
                    -> Int
                    -> (ContextTestValExpr -> Gen ValExpression) 
                    -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                    -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
         addSuccess s n g c = let ctx = ContextTestValExpr TorXakis.SortGen.empty TorXakis.GenCollection.empty TorXakis.RefMap.empty
                                in 
                                    case TorXakis.GenCollection.add ctx s n g c of
                                        Left e -> error ("empty - successful add expected, yet " ++ show e)
                                        Right c' -> c'

empty :: ContextTestValExpr
empty = ContextTestValExpr TorXakis.SortGen.empty
                           initialGenMap
                           TorXakis.RefMap.empty
    
instance SortContext ContextTestValExpr where
    memberSort r = memberSort r . testSortContext

    memberADT r = memberADT r . testSortContext

    lookupADT r = lookupADT r . testSortContext

    elemsADT  = elemsADT . testSortContext

    addADTs as ctx = case addADTs as (testSortContext ctx) of
                          Left e     -> Left e
                          Right vctx -> Right $ ctx { _genMap = addValueGens (_genMap ctx) as
                                                    , testSortContext = vctx
                                                    }
        where
            addValueGens :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                        -> [ADTDef]
                        -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
            addValueGens = foldl addValueGen
                where
                    addValueGen :: TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                                -> ADTDef
                                -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
                    addValueGen m a = let srt = SortADT (toRef a)
                                        in
                                          case TorXakis.GenCollection.add ctx srt 0 (genValExprValueOfSort srt) m of
                                            Left e -> error ("addADTs - successful add expected, yet " ++ show e)
                                            Right c -> c

instance TestSortContext ContextTestValExpr where
    sortSize r = sortSize r . testSortContext
    adtSize a = adtSize a . testSortContext
    constructorSize a c = constructorSize a c . testSortContext

-- | non unique Variable Definitions (i.e. duplicate names)
nuVarDefs :: [VarDef] -> [VarDef]
nuVarDefs = repeatedByRef

-- | undefined Sorts of Variable Definitions.
undefinedSorts :: SortContext a => a -> [VarDef] -> [VarDef]
undefinedSorts ctx = filter (not . flip memberSort ctx . TorXakis.Var.sort)

addVarGen :: ContextTestValExpr
          -> [VarDef]
          -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
          -> TorXakis.GenCollection.GenCollection ContextTestValExpr ValExpression
addVarGen _ []       m = m
addVarGen ctx xs@(x:_) m = let t = TorXakis.Var.sort x
                               (ts,os) = List.partition (\v -> TorXakis.Var.sort v == t) xs
                            in
                              case TorXakis.GenCollection.add ctx t 0 ( genValExprVar (map toRef ts) ) m of
                                       Left e -> error ("addVars - successful add expected, yet " ++ show e)
                                       Right c -> addVarGen ctx os c

instance VarContext ContextTestValExpr where
    memberVar v ctx = member v (varDefs ctx)

    lookupVar v ctx = TorXakis.RefMap.lookup v (varDefs ctx)

    elemsVar ctx    = elems (varDefs ctx)

    addVars vs ctx
        | not $ null (nuVarDefs vs)          = Left $ Error ("Non unique variable definitions: " ++ show (nuVarDefs vs))
        | not $ null (undefinedSorts ctx vs) = Left $ Error ("List of variable definitions with undefined sorts: " ++ show (undefinedSorts ctx vs))
        | otherwise                          = Right $ ctx { _genMap = addVarGen ctx vs (_genMap ctx)
                                                           , varDefs = union (toRefMap vs) (varDefs ctx)}

    replaceVars vs ctx
        | not $ null (nuVarDefs vs)          = Left $ Error ("Non unique variable definitions: " ++ show (nuVarDefs vs))
        | not $ null (undefinedSorts ctx vs) = Left $ Error ("List of variable definitions with undefined sorts: " ++ show (undefinedSorts ctx vs))
        | otherwise                          = Right $ ctx { _genMap = addVarGen ctx vs initialGenMap
                                                           , varDefs = toRefMap vs}

instance TestValExprContext ContextTestValExpr where
    arbitraryValExprOfSort ctx s = do
        n <- getSize
        case TorXakis.GenCollection.get (_genMap ctx) s n of
            [] -> error ("No Generators for " ++ show s ++ " at " ++ show n)
            xs -> do
                    generator <- elements xs
                    generator ctx


{-   TODO 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
data ContextTestValExpr a = ContextTestValExpr 
                                    { testFuncContext :: a
                                    , _varDefs :: HashMap.Map (Ref VarDef) VarDef
                                    } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance SortContext a => SortContext (ContextTestValExpr a) where
    empty             = ContextTestValExpr empty HashMap.empty
    adtDefs ctx       = adtDefs (testFuncContext ctx)
    addADTs ctx as = case addADTs (testFuncContext ctx) as of
                            Left e     -> Left e
                            Right tctx -> Right $ ctx { testFuncContext = tctx }

instance TestSortContext a => TestSortContext (ContextTestValExpr a) where
    mapSortSize ctx              = mapSortSize (testFuncContext ctx)
    mapAdtMapConstructorSize ctx = mapAdtMapConstructorSize (testFuncContext ctx)

instance FuncContext a => FuncContext (ContextTestValExpr a) where
    funcDefs ctx = funcDefs (testFuncContext ctx)
    addFuncDefs ctx fds = case addFuncDefs (testFuncContext ctx) fds of
                            Left e     -> Left e
                            Right tctx -> Right $ ctx { testFuncContext = tctx }

instance SortContext a => VarContext (ContextTestValExpr a) where
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


instance TestFuncContext a => TestFuncContext (ContextTestValExpr a)

instance TestFuncContext a => TestValExprContext (ContextTestValExpr a)
-}