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
{-# LANGUAGE GADTs                #-}
module TorXakis.TestValExprConstructionData
(-- * Test FuncSignature Data
  TestValExprConstructionData
, TorXakis.TestValExprConstructionData.empty
, TorXakis.TestValExprConstructionData.sortSize
, TorXakis.TestValExprConstructionData.adtSize
, TorXakis.TestValExprConstructionData.constructorSize
, TorXakis.TestValExprConstructionData.afterAddADTs
, afterAddVars
, afterAddFuncSignatures
, genMap
)
where
import           Test.QuickCheck

import           TorXakis.Distribute
import           TorXakis.FuncContext
import           TorXakis.FuncSignature
import qualified TorXakis.GenCollection
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.TestFuncSignatureData
import           TorXakis.TestSortData
import           TorXakis.TestValExprConstructionContext
import           TorXakis.TestVarData
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.ValueGen
import           TorXakis.Var

                                    -- to be added to genMap
                                    -- 0. predefined operators (modulo, and, not, sum , etc)
                                    -- 1. value generation for all defined sorts
                                    -- 2. generators related to ADTs (constructors and accessors)
                                    -- 3. If THEN Else etc.
                                    -- 4. generators for variables (based on variables defined in context)
                                    -- 5. generators for funcInstantiation (based on funcSignatures defined in context)
                                    

-- | Test FuncSignature Data
data TestValExprConstructionData a where
        TestValExprConstructionData :: TestValExprConstructionContext a 
                                    => TestSortData
                                    -> TorXakis.GenCollection.GenCollection a ValExpression
                                    -> TestValExprConstructionData a

-- | TestSortData accessor
tsd :: TestValExprConstructionData a -> TestSortData
tsd (TestValExprConstructionData d _) = d

-- | Generator Map accessor
genMap :: TestValExprConstructionData a -> TorXakis.GenCollection.GenCollection a ValExpression
genMap (TestValExprConstructionData _ gM) = gM

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

-- | variable Size
--   The size of the provided var as specified by its name is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.Var' and any related 'TorXakis.Sort' references.
varSize :: VarContext a 
        => RefByName VarDef 
        -> a
        -> TestValExprConstructionData d
        -> Int
varSize r ctx = TorXakis.TestVarData.varSize r ctx . tsd

-- | FuncSignature Size
--   The size of the provided funcSignature as specified by the references to 'TorXakis.FuncSignature' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.FuncSignature' and any related 'TorXakis.Sort' references.
funcSize :: RefByFuncSignature -> a -> TestValExprConstructionData d -> Int
funcSize r ctx = TorXakis.TestFuncSignatureData.funcSize r ctx . tsd

-- | Constructor of empty Test Val Expr Construction Data
empty :: (SortContext a, TestValExprConstructionContext d) => a -> TestValExprConstructionData d
empty ctx = TestValExprConstructionData (TorXakis.TestSortData.empty) initialGenMap
    where
        initialGenMap :: TestValExprConstructionContext d => TorXakis.GenCollection.GenCollection d ValExpression
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
                            -> (d -> Gen ValExpression)
                            -> TorXakis.GenCollection.GenCollection d ValExpression
                            -> TorXakis.GenCollection.GenCollection d ValExpression
                 addSuccess s n g c = case TorXakis.GenCollection.add ctx s n g c of
                                           Left e -> error ("empty - successful add expected, yet " ++ show e)
                                           Right c' -> c'

afterAddADTs :: (SortContext a, TestValExprConstructionContext d) => a -> [ADTDef] -> TestValExprConstructionData d -> TestValExprConstructionData d
afterAddADTs ctx as tvecd = TestValExprConstructionData (TorXakis.TestSortData.afterAddADTs ctx as (tsd tvecd))
                                                        (addValueGens (genMap tvecd) as)
                                                      -- TODO: add constructors and accessors
    where
            addValueGens :: TestValExprConstructionContext d
                         => TorXakis.GenCollection.GenCollection d ValExpression
                         -> [ADTDef]
                         -> TorXakis.GenCollection.GenCollection d ValExpression
            addValueGens = foldl addValueGen
                where
                    addValueGen :: TestValExprConstructionContext d
                                => TorXakis.GenCollection.GenCollection d ValExpression
                                -> ADTDef
                                -> TorXakis.GenCollection.GenCollection d ValExpression
                    addValueGen m a = let srt = SortADT ((RefByName . adtName) a)
                                        in
                                          case TorXakis.GenCollection.add ctx srt 0 (genValExprValueOfSort srt) m of
                                            Left e -> error ("addADTs - successful add expected, yet " ++ show e)
                                            Right c -> c

afterAddVars :: (VarContext a, TestValExprConstructionContext d) => a -> [VarDef] -> TestValExprConstructionData d -> TestValExprConstructionData d
afterAddVars ctx vs tvecd = TestValExprConstructionData (tsd tvecd)
                                                        (foldl addVarGen (genMap tvecd) vs)
    where
        addVarGen :: TestValExprConstructionContext a 
                  => TorXakis.GenCollection.GenCollection a ValExpression
                  -> VarDef
                  -> TorXakis.GenCollection.GenCollection a ValExpression
        addVarGen m v = case TorXakis.GenCollection.add ctx s vSize (genValExprVar (RefByName n)) m of
                             Left e -> error ("addVars - successful add expected, yet " ++ show e)
                             Right x -> x
            where
                s = TorXakis.Var.sort v
                n = TorXakis.Var.name v
                vSize = TorXakis.TestValExprConstructionData.varSize (RefByName n) ctx tvecd -- seems inefficient: throw away sort, yet localize function in one spot! TODO can it be smarter?

afterAddFuncSignatures :: (FuncSignatureContext a, TestValExprConstructionContext d) => a -> [FuncSignature] -> TestValExprConstructionData d -> TestValExprConstructionData d
afterAddFuncSignatures ctx fs tvecd = TestValExprConstructionData (tsd tvecd)
                                                                  (foldl addFuncGen (genMap tvecd) fs)
    where
        addFuncGen :: TestValExprConstructionContext a 
                  => TorXakis.GenCollection.GenCollection a ValExpression
                  -> FuncSignature
                  -> TorXakis.GenCollection.GenCollection a ValExpression
        addFuncGen m f = case TorXakis.GenCollection.add ctx rs fSize (genValExprFunc (RefByFuncSignature f)) m of
                             Left e -> error ("addFuncSignatures - successful add expected, yet " ++ show e)
                             Right x -> x
            where
                rs = TorXakis.FuncSignature.returnSort f
                fSize = TorXakis.TestValExprConstructionData.funcSize (RefByFuncSignature f) ctx tvecd

-------------------------------------------------------------------------------
-- Generic Generators
-------------------------------------------------------------------------------
genValExprVar :: TestValExprConstructionContext a => RefByName VarDef -> a -> Gen ValExpression
genValExprVar v ctx =
    case mkVar ctx v of
        Left e  -> error ("genValExprVar constructor with " ++ show v ++ " fails " ++ show e)
        Right x -> return x

genValExprFunc :: TestValExprConstructionContext a => RefByFuncSignature -> a -> Gen ValExpression
genValExprFunc r@(RefByFuncSignature f) ctx = do
    n <- getSize
    let availableSize = n - TorXakis.TestValExprConstructionContext.funcSize r ctx
        nrOfArgs = length (args f)
      in do
        additionalComplexity <- distribute availableSize nrOfArgs
        let sizeArgs = map (flip TorXakis.TestValExprConstructionContext.sortSize ctx) (args f)
            paramComplexity = zipWith (+) sizeArgs additionalComplexity
          in do
            vs <- mapM (\(c,s) -> resize c (arbitraryValExprOfSort ctx s)) $ zip paramComplexity (args f)
            case mkFunc ctx r vs of
                Left e  -> error ("genValExprFunc constructor fails " ++ show e)
                Right x -> return x

genValExprValueOfSort :: TestValExprConstructionContext a => Sort -> a -> Gen ValExpression
genValExprValueOfSort s ctx = do
    v <- arbitraryValueOfSort ctx s
    case mkConst ctx v of
        Left e  -> error ("genValExprValueOfSort constructor with value " ++ show v ++ " of sort " ++ show s ++ " fails " ++ show e)
        Right x -> return x
-------------------------------------------------------------------------------
-- Boolean Generators
-------------------------------------------------------------------------------
genValExprNot :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprNot ctx = do
    n <- getSize
    arg <- resize (n-1) (arbitraryValExprOfSort ctx SortBool)
    case mkNot ctx arg of
         Left e  -> error ("genValExprNot constructor fails " ++ show e)
         Right x -> return x
-------------------------------------------------------------------------------
-- Integer Generators
-------------------------------------------------------------------------------
nonZero :: TestValExprConstructionContext a => a -> Gen ValExpression
nonZero ctx = do
    n <- arbitraryValExprOfSort ctx SortInt
    case view n of
        Vconst (Cint 0) -> nonZero ctx
        _               -> return n

division :: TestValExprConstructionContext a => a -> Gen (ValExpression, ValExpression)
division ctx = do
    n <- getSize
    let available = n - 2 in do -- distribute available size over two intervals
        t <- choose (0, available)
        teller <- resize t             (arbitraryValExprOfSort ctx SortInt)
        noemer <- resize (available-t) (nonZero ctx)
        return (teller, noemer)

-- TODO: make safe for substitution by using IF n == 0 then t else t % n FI
genValExprModulo :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprModulo ctx = do
    (teller, noemer) <- division ctx
    case mkModulo ctx teller noemer of
         Left e  -> error ("genValExprModulo constructor fails " ++ show e)
         Right x -> return x

-- TODO: make safe for substitution by using IF n == 0 then t else t / n FI
genValExprDivide :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprDivide ctx = do
    (teller, noemer) <- division ctx
    case mkDivide ctx teller noemer of
         Left e  -> error ("genValExprDivide constructor fails " ++ show e)
         Right x -> return x