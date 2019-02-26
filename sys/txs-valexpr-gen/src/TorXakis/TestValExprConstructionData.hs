{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestValExprConstructionData
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- FuncSignature Data for Test:
-- Additional data to ensure termination for QuickCheck
-----------------------------------------------------------------------------
module TorXakis.TestValExprConstructionData
(-- * Test FuncSignature Data
  TestValExprConstructionData
  -- dependencies, yet part of interface
, module TorXakis.TestSortData
)
where
import           Test.QuickCheck

import           TorXakis.Error
import           TorXakis.FuncContext
import qualified TorXakis.GenCollection
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.Sort
import           TorXakis.SortGen
import           TorXakis.TestSortData
import           TorXakis.TestValExprConstructionContext
import           TorXakis.ValExpr
import           TorXakis.ValExprConstructionContext
import           TorXakis.Value
import           TorXakis.ValueGen
import           TorXakis.VarContext
import           TorXakis.Var

-- | Test FuncSignature Data
data ValExprConstructionContext a => TestValExprConstructionData a =
        TestValExprConstructionData { tsd :: TestSortData
                                    , genMap :: TorXakis.GenCollection.GenCollection a ValExpression
                                    -- to be added
                                    -- 0. predefined operators (modulo, and, not, sum , etc)
                                    -- 1. value generation for all defined sorts
                                    -- 2. generators related to ADTs (constructors and accessors)
                                    -- 3. If THEN Else etc.
                                    -- 4. generators for variables (based on variables defined in context)
                                    -- 5. generators for funcInstantiation (based on funcSignatures defined in context)
                                    }

-- | Sort Size
--   The size of the provided 'TorXakis.Sort' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.Sort' reference.
sortSize :: Sort -> TestValExprConstructionData a -> Int
sortSize s = TorXakis.TestSortData.sortSize s . tsd

-- |  adt Size
--   The size of the provided reference to 'TorXakis.ADTDef' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef' and any related 'TorXakis.Sort' references.
adtSize :: RefByName ADTDef -> TestValExprConstructionData a -> Int
adtSize r = TorXakis.TestSortData.adtSize r . tsd

-- |  constructor Size
--   The size of the provided constructor as specified by the references to 'TorXakis.ADTDef' and 'TorXakis.ConstructorDef' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef', 'TorXakis.ConstructorDef' and any related 'TorXakis.Sort' references.
constructorSize :: RefByName ADTDef -> RefByName ConstructorDef -> TestValExprConstructionData a -> Int
constructorSize r c = TorXakis.TestSortData.constructorSize r c . tsd

-- | Constructor of empty Test Val Expr Construction Data
empty :: ValExprConstructionContext a => a -> TestValExprConstructionData a
empty ctx = TestValExprConstructionData (TorXakis.TestSortData.empty) initialGenMap
    where
        initialGenMap :: ValExprConstructionContext a => TorXakis.GenCollection.GenCollection a ValExpression
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
                            -> (a -> Gen ValExpression)
                            -> TorXakis.GenCollection.GenCollection a ValExpression
                            -> TorXakis.GenCollection.GenCollection a ValExpression
                 addSuccess s n g c = case TorXakis.GenCollection.add ctx s n g c of
                                           Left e -> error ("empty - successful add expected, yet " ++ show e)
                                           Right c' -> c'

afterAddADTs :: ValExprConstructionContext a => a -> [ADTDef] -> TestValExprConstructionData a -> TestValExprConstructionData a
afterAddADTs ctx as tvecd = TestValExprConstructionData (TorXakis.TestSortData.afterAddADTs ctx as (tsd tvecd))
                                                        (addValueGens (genMap tvecd))
                                                      -- TODO: add constructors and accessors
    where
            addValueGens :: ValExprConstructionContext a => TorXakis.GenCollection.GenCollection a ValExpression
                        -> [ADTDef]
                        -> TorXakis.GenCollection.GenCollection a ValExpression
            addValueGens = foldl addValueGen
                where
                    addValueGen :: ValExprConstructionContext a
                                => TorXakis.GenCollection.GenCollection a ValExpression
                                -> ADTDef
                                -> TorXakis.GenCollection.GenCollection a ValExpression
                    addValueGen m a = let srt = SortADT ((RefByName . adtName) a)
                                        in
                                          case TorXakis.GenCollection.add ctx srt 0 (genValExprValueOfSort srt) m of
                                            Left e -> error ("addADTs - successful add expected, yet " ++ show e)
                                            Right c -> c

afterAddVars :: ValExprConstructionContext a => a -> [VarDef] -> TestValExprConstructionData a -> TestValExprConstructionData a
afterAddVars ctx vs tvecd = TestValExprConstructionData (tsd tvecd)
                                                        (foldl addVarGen (genMap tvecd) vs)
    where
        addVarGen :: TorXakis.GenCollection.GenCollection a ValExpression
                  -> VarDef
                  -> TorXakis.GenCollection.GenCollection a ValExpression
        addVarGen m v = case TorXakis.GenCollection.add ctx s useSize (genValExprVar (RefByName n)) m of
                             Left e -> error ("addVars - successful add expected, yet " ++ show e)
                             Right x -> x
            where
                s = TorXakis.Var.sort v
                n = TorXakis.Var.name v
                useSize = TorXakis.TestValExprConstructionContext.sortSize s ctx + 1
-------------------------------------------------------------------------------
-- Generic Generators
-------------------------------------------------------------------------------
genValExprVar :: ValExprConstructionContext a => RefByName VarDef -> a -> Gen ValExpression
genValExprVar v ctx =
    case mkVar ctx v of
        Left e  -> error ("genValExprVar constructor with " ++ show v ++ " fails " ++ show e)
        Right x -> return x

genValExprValueOfSort :: ValExprConstructionContext a => Sort -> a -> Gen ValExpression
genValExprValueOfSort s ctx = do
    v <- arbitraryValueOfSort ctx s
    case mkConst ctx v of
        Left e  -> error ("genValExprValueOfSort constructor with value " ++ show v ++ " of sort " ++ show s ++ " fails " ++ show e)
        Right x -> return x
-------------------------------------------------------------------------------
-- Boolean Generators
-------------------------------------------------------------------------------
genValExprNot :: ValExprConstructionContext a => a -> Gen ValExpression
genValExprNot ctx = do
    n <- getSize
    arg <- resize (n-1) (arbitraryValExprOfSort ctx SortBool)
    case mkNot ctx arg of
         Left e  -> error ("genValExprNot constructor fails " ++ show e)
         Right x -> return x
-------------------------------------------------------------------------------
-- Integer Generators
-------------------------------------------------------------------------------
nonZero :: ValExprConstructionContext a => a -> Gen ValExpression
nonZero ctx = do
    n <- arbitraryValExprOfSort ctx SortInt
    case view n of
        Vconst (Cint 0) -> nonZero ctx
        _               -> return n

division :: ValExprConstructionContext a => a -> Gen (ValExpression, ValExpression)
division ctx = do
    n <- getSize
    let available = n - 2 in do -- distribute available size over two intervals
        t <- choose (0, available)
        teller <- resize t             (arbitraryValExprOfSort ctx SortInt)
        noemer <- resize (available-t) (nonZero ctx)
        return (teller, noemer)

genValExprModulo :: ValExprConstructionContext a => a -> Gen ValExpression
genValExprModulo ctx = do
    (teller, noemer) <- division ctx
    case mkModulo ctx teller noemer of
         Left e  -> error ("genValExprModulo constructor fails " ++ show e)
         Right x -> return x

genValExprDivide :: ValExprConstructionContext a => a -> Gen ValExpression
genValExprDivide ctx = do
    (teller, noemer) <- division ctx
    case mkDivide ctx teller noemer of
         Left e  -> error ("genValExprDivide constructor fails " ++ show e)
         Right x -> return x