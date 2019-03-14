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
, TorXakis.TestValExprConstructionData.varSize
, TorXakis.TestValExprConstructionData.funcSize
, TorXakis.TestValExprConstructionData.afterAddADTs
, afterAddVars
, afterAddFuncSignatures
, genMap
)
where
import           Test.QuickCheck

import           TorXakis.ContextSort
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
data TestValExprConstructionData c where
        TestValExprConstructionData :: TestValExprConstructionContext c
                                    => TestSortData
                                    -> TorXakis.GenCollection.GenCollection c ValExpression
                                    -> TestValExprConstructionData c

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
varSize :: VarContext c
        => RefByName VarDef
        -> c
        -> TestValExprConstructionData d
        -> Int
varSize r ctx = TorXakis.TestVarData.varSize r ctx . tsd

-- | FuncSignature Size
--   The size of the provided funcSignature as specified by the references to 'TorXakis.FuncSignature' is returned.
--   The size is a measurement of complexity and is indicated by an 'Int'.
--   Note that the function should crash when the context does not contain the 'TorXakis.FuncSignature' and any related 'TorXakis.Sort' references.
funcSize :: RefByFuncSignature -> c -> TestValExprConstructionData d -> Int
funcSize r ctx = TorXakis.TestFuncSignatureData.funcSize r ctx . tsd

-- | Constructor of empty Test Val Expr Construction Data
empty :: (SortContext c, TestValExprConstructionContext d) => c -> TestValExprConstructionData d
empty ctx = TestValExprConstructionData TorXakis.TestSortData.empty initialGenMap
    where
        initialGenMap :: TestValExprConstructionContext d => TorXakis.GenCollection.GenCollection d ValExpression
        initialGenMap = -- Bool
                          addSuccess SortBool   0 (genValExprValueOfSort SortBool)
                        $ addSuccess SortBool   1 genValExprNot
                        $ addSuccess SortBool   1 genValExprGEZ
                        -- Bool -- Equal
                        $ addSuccess SortBool   2 (genValExprEqual SortBool)
                        $ addSuccess SortBool   2 (genValExprEqual SortInt)
                        $ addSuccess SortBool   2 (genValExprEqual SortChar)
                        $ addSuccess SortBool   2 (genValExprEqual SortString)
                        
                        $ addSuccess SortBool   2 genValExprAnd
                        $ addSuccess SortBool   3 (genValExprITE SortBool)

                        -- Int
                        $ addSuccess SortInt    0 (genValExprValueOfSort SortInt)
                        $ addSuccess SortInt    1 genValExprUnaryMinus
                        $ addSuccess SortInt    1 genValExprLength
                        $ addSuccess SortInt    2 genValExprModulo
                        $ addSuccess SortInt    2 genValExprDivide
                        $ addSuccess SortInt    2 genValExprSum
                        $ addSuccess SortInt    2 genValExprProduct
                        $ addSuccess SortInt    3 (genValExprITE SortInt)

                        -- Char
                        $ addSuccess SortChar   0 (genValExprValueOfSort SortChar)
                        $ addSuccess SortChar   3 (genValExprITE SortChar)

                        -- String
                        $ addSuccess SortString 0 (genValExprValueOfSort SortString)
                        $ addSuccess SortString 2 genValExprConcat
                        $ addSuccess SortString 2 genValExprAt
                        $ addSuccess SortString 3 (genValExprITE SortString)

                        -- Regex
                        $ addSuccess SortRegex  0 (genValExprValueOfSort SortRegex) -- TODO: generate a VALID regex string
                        -- TODO: add StrInRe  <- needs valid REGEX strings
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

-- | Update TestValExprConstructionData to remain consistent after
-- a successful addition of ADTs to the context.
afterAddADTs :: (SortContext c, TestValExprConstructionContext d) => c -> [ADTDef] -> TestValExprConstructionData d -> TestValExprConstructionData d
afterAddADTs ctx as tvecd = TestValExprConstructionData newTsd
                                                        (addConstructorsGens
                                                         (addValueGens (genMap tvecd) as)
                                                         as
                                                        )
                                                        -- TODO: add IsConstructors and accessors functions
    where
            newTsd = TorXakis.TestSortData.afterAddADTs ctx as (tsd tvecd)
            -- TODO: should we merge ???
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
                                            Left e -> error ("addADTs - addValueGens - successful add expected, yet " ++ show e)
                                            Right m' -> m'

            addConstructorsGens :: TestValExprConstructionContext d
                                => TorXakis.GenCollection.GenCollection d ValExpression
                                -> [ADTDef]
                                -> TorXakis.GenCollection.GenCollection d ValExpression
            addConstructorsGens = foldl addConstructorsGen
                where
                    addConstructorsGen :: TestValExprConstructionContext d
                                       => TorXakis.GenCollection.GenCollection d ValExpression
                                       -> ADTDef
                                       -> TorXakis.GenCollection.GenCollection d ValExpression
                    addConstructorsGen m a = foldl addConstructorGen m (elemsConstructor a)
                        where 
                            srt = SortADT ((RefByName . adtName) a)
                            addConstructorGen :: TestValExprConstructionContext d
                                              => TorXakis.GenCollection.GenCollection d ValExpression
                                              -> ConstructorDef
                                              -> TorXakis.GenCollection.GenCollection d ValExpression
                            addConstructorGen m' c = case TorXakis.GenCollection.add ctx srt (TorXakis.TestSortData.constructorSize (RefByName (adtName a)) (RefByName (constructorName c)) newTsd) (genValExprConstructor a c) m' of
                                                            Left e -> error ("addADTs - addConstructorGen - successful add expected, yet " ++ show e)
                                                            Right m'' -> m''

                            
-- | Update TestValExprConstructionData to remain consistent after
-- a successful addition of Vars to the context.
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

-- | Update TestValExprConstructionData to remain consistent after
-- a successful addition of FuncSignatures to the context.
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

genValExprEqual :: TestValExprConstructionContext a => Sort -> a -> Gen ValExpression
genValExprEqual s ctx = do
    n <- getSize
    let available = n - 2
        sizeSort = TorXakis.TestValExprConstructionContext.sortSize s ctx
      in do
        t <- choose (sizeSort, available-sizeSort)
        left  <- resize t             (arbitraryValExprOfSort ctx s)
        right <- resize (available-t) (arbitraryValExprOfSort ctx s)
        case mkEqual ctx left right of
            Left e  -> error ("genValExprEqual constructor with sort " ++ show s ++ " fails " ++ show e)
            Right x -> return x

genValExprITE :: TestValExprConstructionContext a => Sort -> a -> Gen ValExpression
genValExprITE s ctx = do
    n <- getSize
    let available = n - 3
        sizeBool = TorXakis.TestValExprConstructionContext.sortSize SortBool ctx
        sizeBranch = TorXakis.TestValExprConstructionContext.sortSize s ctx
        availableSize = available - sizeBool - 2 * sizeBranch
      in do
        adds <- distribute availableSize 3
        case adds of 
            [addBool, addTrue, addFalse] -> do
                                                c <- resize (sizeBool + addBool) (arbitraryValExprOfSort ctx SortBool)
                                                t <- resize (sizeBranch + addTrue) (arbitraryValExprOfSort ctx s)
                                                f <- resize (sizeBranch + addFalse) (arbitraryValExprOfSort ctx s)
                                                case mkITE ctx c t f of
                                                    Left e  -> error ("genValExprITE constructor with sort " ++ show s ++ " fails " ++ show e)
                                                    Right x -> return x
            _                            -> error ("distribute with 3 returned list with length <> 3: " ++ show adds)

genValExprFunc :: TestValExprConstructionContext a => RefByFuncSignature -> a -> Gen ValExpression
genValExprFunc r@(RefByFuncSignature f) ctx = do
    n <- getSize
    let availableSize = n - TorXakis.TestValExprConstructionContext.funcSize r ctx
        nrOfArgs = length (args f)
      in do
        additionalComplexity <- distribute availableSize nrOfArgs
        let sizeArgs = map (`TorXakis.TestValExprConstructionContext.sortSize` ctx) (args f)
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

serie :: TestValExprConstructionContext a => Sort -> a -> Gen [ValExpression]
serie s ctx = do
    n <- getSize
    let available = n - 1 in do
        size <- choose (0, available)
        let remaining = available - size in do
            additionalComplexity <- distribute remaining size
            mapM (\c -> resize c (arbitraryValExprOfSort ctx s)) additionalComplexity

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

genValExprGEZ :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprGEZ ctx = do
    n <- getSize
    arg <- resize (n-1) (arbitraryValExprOfSort ctx SortInt)
    case mkGEZ ctx arg of
         Left e  -> error ("genValExprGEZ constructor fails " ++ show e)
         Right x -> return x

-- | large And arrays have high likelihood of containing False
-- do we want this reduction to False?
genValExprAnd :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprAnd ctx = do
    ps <- serie SortBool ctx
    case mkAnd ctx ps of
         Left e  -> error ("genValExprAnd constructor fails " ++ show e)
         Right x -> return x

-------------------------------------------------------------------------------
-- Integer Generators
-------------------------------------------------------------------------------
genValExprUnaryMinus :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprUnaryMinus ctx = do
    n <- getSize
    arg <- resize (n-1) (arbitraryValExprOfSort ctx SortInt)
    case mkUnaryMinus ctx arg of
         Left e  -> error ("genValExprUnaryMinus constructor fails " ++ show e)
         Right x -> return x

genValExprLength :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprLength ctx = do
    n <- getSize
    arg <- resize (n-1) (arbitraryValExprOfSort ctx SortString)
    case mkLength ctx arg of
         Left e  -> error ("genValExprLength constructor fails " ++ show e)
         Right x -> return x

zero :: ValExpression
zero = case mkConst TorXakis.ContextSort.empty (Cint 0) of
            Left e -> error ("Unable to make zero " ++ show e)
            Right x -> x

nonZero :: TestValExprConstructionContext a => a -> Gen ValExpression
nonZero ctx = do
    n <- arbitraryValExprOfSort ctx SortInt
    if n == zero
        then discard
        else return n

division :: TestValExprConstructionContext a => a -> Gen (ValExpression, ValExpression)
division ctx = do
    n <- getSize
    let available = n - 2 in do -- distribute available size over two intervals
        t <- choose (0, available)
        teller <- resize t             (arbitraryValExprOfSort ctx SortInt)
        noemer <- resize (available-t) (nonZero ctx)
        return (teller, noemer)

genValExprModulo :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprModulo ctx = do
    (teller, noemer) <- division ctx
    case mkEqual ctx noemer zero of
        Left e -> error ("genValExprModulo mkEqual fails " ++ show e)
        Right c -> case mkModulo ctx teller noemer of
                    Left e  -> error ("genValExprModulo mkModulo fails " ++ show e)
                    Right f -> case mkITE ctx c teller f of
                                    Left e  -> error ("genValExprModulo mkITE fails " ++ show e)
                                    Right x -> return x

genValExprDivide :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprDivide ctx = do
    (teller, noemer) <- division ctx
    case mkEqual ctx noemer zero of
        Left e -> error ("genValExprDivide mkEqual fails " ++ show e)
        Right c -> case mkDivide ctx teller noemer of
                    Left e  -> error ("genValExprDivide mkDivide fails " ++ show e)
                    Right f -> case mkITE ctx c teller f of
                                    Left e  -> error ("genValExprDivide mkITE fails " ++ show e)
                                    Right x -> return x

genValExprSum :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprSum ctx = do
    ps <- serie SortInt ctx
    case mkSum ctx ps of
         Left e  -> error ("genValExprSum constructor fails " ++ show e)
         Right x -> return x

genValExprProduct :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprProduct ctx = do
    ps <- serie SortInt ctx
    case mkProduct ctx ps of
         Left e  -> error ("genValExprProduct constructor fails " ++ show e)
         Right x -> return x

-------------------------------------------------------------------------------
-- String Generators
-------------------------------------------------------------------------------
genValExprAt :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprAt ctx = do
    n <- getSize
    let available = n - 2
      in do
        t <- choose (0, available)
        str <- resize t             (arbitraryValExprOfSort ctx SortString)
        pos <- resize (available-t) (arbitraryValExprOfSort ctx SortInt)
        case mkAt ctx str pos of
            Left e  -> error ("genValExprAt constructor fails " ++ show e)
            Right x -> return x

genValExprConcat :: TestValExprConstructionContext a => a -> Gen ValExpression
genValExprConcat ctx = do
    ps <- serie SortString ctx
    case mkConcat ctx ps of
         Left e  -> error ("genValExprConcat constructor fails " ++ show e)
         Right x -> return x

-----------------------------------------------
-- ADT generators
------------------------------------------------
genValExprConstructor :: TestValExprConstructionContext a => ADTDef -> ConstructorDef -> a -> Gen ValExpression
genValExprConstructor a c ctx = 
    let aRef :: RefByName ADTDef
        aRef = RefByName (adtName a)
        cRef :: RefByName ConstructorDef
        cRef = RefByName (constructorName c)
        fields = elemsField c
        fieldSorts = map TorXakis.Sort.sort fields
     in do
        n <- getSize
        let availableSize = n - TorXakis.TestValExprConstructionContext.constructorSize aRef cRef ctx in do
            additionalComplexity <- distribute availableSize (length fields)
            let sizeFields = map (`TorXakis.TestValExprConstructionContext.sortSize` ctx) fieldSorts
                fieldComplexity = zipWith (+) sizeFields additionalComplexity
              in do
                fs <- mapM (\(size,srt) -> resize size (arbitraryValExprOfSort ctx srt)) $ zip fieldComplexity fieldSorts
                case mkCstr ctx aRef cRef fs of
                    Left e  -> error ("genValExprConstructor constructor fails " ++ show e)
                    Right x -> return x
