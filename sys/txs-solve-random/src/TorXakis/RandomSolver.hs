{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.RandomSolver
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the Random Solver.
-- The randomization works incrementally: one variable at a time.
-- When the selected variable can be changed,
-- its value space is divided into bins.
-- By randomly selecting a bin, a random solution is approximated.
-- Randomly solving a variable can result in additional variables.
-- E.g. when a constructor is selected, its fields are added as new variables.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TorXakis.RandomSolver
( RandomM (..)
, RandomState (maxDepth, nrOfBins, ratio)
, Ratio(..)
, mkRandomState
)
where
import           Control.Exception
import           Control.Monad.State
import           Data.Char
import           Data.Either
import qualified Data.HashMap
import qualified Data.List
import           Data.Maybe
import qualified Data.Text
import           Numeric
import           System.Random
import           System.Random.Shuffle


import           TorXakis.Name
import           TorXakis.ProblemSolver
import           TorXakis.Regex
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.ValExprContext
import           TorXakis.Value
import           TorXakis.Var

-- | What is the size ratio between consecutive bins?
data  Ratio      = Const | Factor | Exponent
     deriving (Eq,Ord,Read,Show)

-- | Random Solver State
data RandomState = RandomState { -- | Maximum Depth for ADT randomizations
                                 -- necessary to handle recursive data types by preventing infinite generation
                                 -- precondition `maxDepth` > 0
                                 maxDepth                 :: Integer
                                 -- | Number of Bins for Integer generation
                                 -- necessary since an Integer represent an infinite amount of values
                                 -- precondition `nrOfBins` > 0
                               , nrOfBins                 :: Int
                                 -- | Size ratio for generation of Integer bins
                                 -- necessary to control the likelihood of Integer values.
                                 -- How likely is e.g. 2^64 - needed for overflow tests, yet maybe too long as the length of a list
                               , ratio                    :: Ratio
                               , tmpId                    :: Integer
                               } deriving (Eq,Ord,Read,Show)

-- | constructor for RandomState
mkRandomState :: Integer -- ^ max Depth
              -> Int     -- ^ nrofBins
              -> Ratio   -- ^ ratio of bins
              -> RandomState
mkRandomState m n r = RandomState m n r 0

-- | Random Problem Solver Monad
newtype RandomM p a = RandomM { -- | to `StateT`
                                toStateT :: StateT RandomState p a
                              }
                              deriving (Functor, Applicative, Monad, MonadState RandomState, MonadIO, MonadTrans)


instance ProblemSolver p => ProblemSolver (RandomM p) where
    info = do
                s <- lift info
                return $ "Random Solver with " ++ s

    addADTs as = lift $ TorXakis.ProblemSolver.addADTs as

    addFunctions fs = lift $ addFunctions fs

    depth = lift depth

    push = lift push

    pop = lift pop

    declareVariables vs = lift $ declareVariables vs

    addAssertions as = lift $ addAssertions as

    solvable = lift solvable

    solvePartSolution [] = lift $ solvePartSolution []
    solvePartSolution vs = do
            s <- lift $ solvePartSolution []
            case s of
                Solved _ -> do
                                d <- gets maxDepth
                                shuffledVs <- liftIO $ shuffleM vs
                                _ <- push
                                randomSolve (zip shuffledVs (repeat d))
                                s' <- lift $ solvePartSolution vs
                                _ <- pop
                                assert (case s' of
                                            Solved _ -> True
                                            _        -> False
                                        ) $ return s'
                _          -> return s

    toValExprContext = lift toValExprContext

-- | make Ranges: a list of tuples consisting of lower bound and upper bound.
mkRanges :: (Integer -> Integer) -> Integer -> Integer -> [(Integer, Integer)]
mkRanges nxt lw hgh = (lw, hgh-1): mkRanges nxt hgh (nxt hgh)

-- | make basic ranges for integer
basicIntRanges :: Int -> Integer -> Ratio -> [(Integer, Integer)]
basicIntRanges n step r = let nxt = case r of
                                            Const    -> nextConst
                                            Factor   -> nextFactor
                                            Exponent -> nextExponent
                              in
                                take n (mkRanges nxt 0 step)
    where
        -- | next function for constant Ratio
        nextConst :: Integer -> Integer
        nextConst = (step +)

        -- | next function for factor Ratio
        nextFactor :: Integer -> Integer
        nextFactor = (step *)

        -- | next function for Exponent Ratio
        nextExponent :: Integer -> Integer
        nextExponent i = i * i

justConst :: SortContext c => c -> Value -> ValExpression
justConst ctx v = case mkConst ctx v of
                    Right x -> x
                    Left e -> error ("mkConst unexpectedly failed with " ++ show e)

justVar :: VarContext c => c -> RefByName VarDef -> ValExpression
justVar ctx v = case mkVar ctx v of
                    Right x -> x
                    Left e -> error ("mkVar unexpectedly failed with " ++ show e)

justLE :: ValExprContext c => c -> ValExpression -> ValExpression -> ValExpression
justLE ctx v1 v2 = case mkLE ctx v1 v2 of
                    Right x -> x
                    Left e -> error ("mkLE unexpectedly failed with " ++ show e)

justLT :: ValExprContext c => c -> ValExpression -> ValExpression -> ValExpression
justLT ctx v1 v2 = case mkLT ctx v1 v2 of
                    Right x -> x
                    Left e -> error ("mkLT unexpectedly failed with " ++ show e)

justEqual :: ValExprContext c => c -> ValExpression -> ValExpression -> ValExpression
justEqual ctx v1 v2 = case mkEqual ctx v1 v2 of
                    Right x -> x
                    Left e -> error ("mkEqual unexpectedly failed with " ++ show e)

justAt :: ValExprContext c => c -> ValExpression -> ValExpression -> ValExpression
justAt ctx v1 v2 = case mkAt ctx v1 v2 of
                    Right x -> x
                    Left e -> error ("mkAt unexpectedly failed with " ++ show e)

justAnd :: ValExprContext c => c -> [ValExpression] -> ValExpression
justAnd ctx vs = case mkAnd ctx vs of
                    Right x -> x
                    Left e -> error ("mkAnd unexpectedly failed with " ++ show e)

justLength :: ValExprContext c => c -> ValExpression -> ValExpression
justLength ctx v = case mkLength ctx v of
                    Right x -> x
                    Left e -> error ("mkLength unexpectedly failed with " ++ show e)

justStrInRe :: ValExprContext c => c -> ValExpression -> ValExpression -> ValExpression
justStrInRe ctx v1 v2 = case mkStrInRe ctx v1 v2 of
                    Right x -> x
                    Left e -> error ("mkStrInRe unexpectedly failed with " ++ show e)

justAccess :: VarContext c => c -> RefByName ADTDef -> RefByName ConstructorDef -> RefByName FieldDef -> ValExpression -> ValExpression
justAccess ctx ar cr fr v = case mkAccess ctx ar cr fr v of
                                    Right x -> x
                                    Left e -> error ("mkAccess unexpectedly failed with " ++ show e)

justAssignment :: ValExprContext c => c -> RefByName VarDef -> Value -> ValExpression
justAssignment ctx v c = justEqual ctx (justVar ctx v) (justConst ctx c)

justRegexRange :: Char -> Char -> Regex
justRegexRange c1 c2 = case mkRegexRange c1 c2 of
                                Right x -> x
                                Left e -> error ("mkRegexRange unexpectedly failed with " ++ show e)

justRegexUnion :: [Regex] -> Regex
justRegexUnion cs = case mkRegexUnion cs of
                                Right x -> x
                                Left e -> error ("mkRegexUnion unexpectedly failed with " ++ show e)

-- | select random integer values
randomIntValues :: Int -> Integer -> Ratio -> IO [Integer]
randomIntValues n step r =
    let ranges = basicIntRanges n step r
     in do
        neg <- mapM randomRIO ranges
        pos <- mapM randomRIO ranges
        return $ reverse (map negate neg) ++ pos

-- | make the `ValExpression` constraints for integer range given the integer boundary values
mkIntConstraints :: ValExprContext c => c -> ValExpression -> [Integer] -> [ValExpression]
mkIntConstraints ctx _ []      = [justConst ctx (Cbool True)]
mkIntConstraints ctx v l@(x:_) = justLE ctx v (justConst ctx (Cint x)) : mkRestBins l
    where
        mkRestBins :: [Integer] -> [ValExpression]
        mkRestBins [y]          = [justLT ctx (justConst ctx (Cint y)) v]
        mkRestBins (y1:y2:ys)   = justAnd ctx [ justLT ctx (justConst ctx (Cint y1)) v
                                              , justLE ctx v (justConst ctx (Cint y2))
                                              ]
                                  : mkRestBins (y2:ys)
        mkRestBins []           = error "mkIntConstraints - unexpectedly mkRestBins called with empty list"

-- | make and randomize the `ValExpression` constraints
randomIntegerConstraints :: ProblemSolver p => ValExpression -> RandomM p [ValExpression]
randomIntegerConstraints v = do
            st <- get
            let n = nrOfBins st
                r = ratio st
              in do
                ns <- liftIO $ randomIntValues n 10 r
                ctx <- toValExprContext
                let cs = mkIntConstraints ctx v ns
                  in
                    liftIO $ shuffleM cs

-- | select random integer values
randomNonNegativeValues :: Int -> Integer -> Ratio -> IO [Integer]
randomNonNegativeValues n step r =
    let ranges = basicIntRanges n step r
     in 
        mapM randomRIO ranges

-- | make the `ValExpression` constraints for non-negativew range (such as string length) given the integer boundary values
mkNonNegativeConstraints :: ValExprContext c => c -> ValExpression -> [Integer] -> [ValExpression]
mkNonNegativeConstraints ctx _ []      = [justConst ctx (Cbool True)]
mkNonNegativeConstraints ctx v l@(x:_) =    justAnd ctx [ justLE ctx (justConst ctx (Cint 0)) v
                                                        , justLE ctx v (justConst ctx (Cint x))
                                                        ]
                                         : mkRestBins l
    where
        mkRestBins :: [Integer] -> [ValExpression]
        mkRestBins [y]          = [justLT ctx (justConst ctx (Cint y)) v]
        mkRestBins (y1:y2:ys)   = justAnd ctx [ justLT ctx (justConst ctx (Cint y1)) v
                                              , justLE ctx v (justConst ctx (Cint y2))
                                              ]
                                  : mkRestBins (y2:ys)
        mkRestBins []           = error "mkNonNegativeConstraints - unexpectedly mkRestBins called with empty list"

-- | String generation
-- First fix  a string length
-- make and randomize the `ValExpression` constraints for string length
randomStringLengthConstraints :: ProblemSolver p => ValExpression -> RandomM p [ValExpression]
randomStringLengthConstraints v = do
            ns <- liftIO $ randomNonNegativeValues 4 2 Factor
            ctx <- toValExprContext
            let cs = mkNonNegativeConstraints ctx v ns
              in
                liftIO $ shuffleM cs

-- | String generation
-- Second fix the character values
-- Split range of characters in equal parts (use wrapping to ensure characters are in range)
randomStringCharConstraints :: ProblemSolver p => ValExpression -> RandomM p [ValExpression]
randomStringCharConstraints v = 
    let nrOfRanges :: Int
        nrOfRanges = 8
        boundaries = take nrOfRanges [0, nrOfCharsInRange ..]
      in
        assert (nrOfChars == nrOfCharsInRange * nrOfRanges) $ do
            offset <- liftIO $ randomRIO (0, nrOfCharsInRange -1)
            cs <- mapM ( makeConstraint . (offset+) ) boundaries
            liftIO $ shuffleM cs
    where
        nrOfChars :: Int
        nrOfChars = ord regexRangeHigh - ord regexRangeLow +1
        nrOfCharsInRange :: Int
        nrOfCharsInRange = 32

        makeConstraint :: ProblemSolver p => Int -> RandomM p ValExpression
        makeConstraint i | i <= nrOfChars - nrOfCharsInRange =
            let l = chr (i                       + ord regexRangeLow)
                u = chr (i + nrOfCharsInRange -1 + ord regexRangeLow)
              in do
                ctx <- toValExprContext
                return $ justStrInRe ctx v (justConst ctx (Cregex (justRegexRange l u)))
        makeConstraint i                                     =
            let l = chr (  i                                         + ord regexRangeLow)
                u = chr ( (i + nrOfCharsInRange -1) `mod` nrOfChars + ord regexRangeLow)
              in do
                ctx <- toValExprContext
                return $ justStrInRe ctx v (justConst ctx (Cregex (justRegexUnion [ justRegexRange l regexRangeHigh
                                                                                  , justRegexRange regexRangeLow u
                                                                                  ])))

-- | ADT generation
-- First fix a constructor
-- Second fix the fields
randomIsCstrConstraints :: ProblemSolver p => RefByName ADTDef -> ValExpression -> RandomM p [ValExpression]
randomIsCstrConstraints ar e = do
    ctx <- toValExprContext
    case lookupADT (toName ar) ctx of
        Nothing -> error ("ADT unexpectedly not present in context - " ++ show ar)
        Just ad -> let cds = elemsConstructor ad
                       eithers = Data.List.map ( (\cr -> mkIsCstr ctx ar cr e) . RefByName . constructorName ) cds
                    in case partitionEithers eithers of
                            ([] , is) -> liftIO $ shuffleM is
                            (es, _)   -> error ("mkIsCstr unexpectedly failed with " ++ show es)

-- | create new variable names
-- temporarily Variable Names for internal usages
createNewVariableName :: ProblemSolver p => RandomM p Name
createNewVariableName = do
    st <- get
    ctx <- toValExprContext
    let i = tmpId st 
        t = Data.Text.pack ( "__" ++ showHex i "__tmp__")
        n = justName t
      in do
        put st { tmpId = i + 1 }
        if memberVar n ctx
        then createNewVariableName
        else return n
    where
        justName :: Data.Text.Text -> Name
        justName t = case mkName t of
                            Right x -> x
                            Left e -> error ("mkName unexpectedly failed with " ++ show e)

-- | Solve with constraints
-- The union (or) of all provided constraints should be true
-- given the variable's properties (such as length of string is >= 0, or string length of a character string is 1)
solveWithConstraints :: ProblemSolver p => RefByName VarDef -> [ValExpression] -> RandomM p Value
solveWithConstraints _ [] = error "solveWithConstraints - Solution exists, yet no solution found in all bins"
solveWithConstraints v (x:xs) = do
    _ <- push
    addAssertions [x]
    s <- lift $ solvePartSolution [v]
    _ <- pop
    case s of
        Solved (Solution sol) -> assert (Data.HashMap.size sol == 1) $
                                     return $ fromMaybe (error "solveWithConstraints - Solver hasn't returned the value of requested variable.")
                                                        (Data.HashMap.lookup v sol)
        _                     -> solveWithConstraints v xs

-- | solve randomly
-- first check whether the variable can be changed
-- Prevent checking all possible values for x, when x == 4 is a constraint.
randomSolve :: ProblemSolver p =>  [(RefByName VarDef, Integer)] -> RandomM p ()
randomSolve []              = return ()
randomSolve ((_,0): _)      = error "randomSolve: At maximum depth - should not have been added"
randomSolve vs@((v,_):xs)   = do
        s <- lift $ solvePartSolution [v]
        case s of
            Solved sol -> do
                            ctx <- toValExprContext
                            case negateSolution ctx sol of
                                Left e    -> error ("randomSolve - negate Solution unexpectedly failed with "++ show e)
                                Right neg -> do
                                                _ <- push
                                                addAssertions [neg]
                                                s' <- solvable
                                                _ <- pop
                                                case s' of
                                                    SolvableProblem (Just b) -> if b
                                                                                then randomSolveBins vs
                                                                                else case assertSolution ctx sol of
                                                                                            Left e  -> error ("randomSolve - assert Solution unexpectedly failed with "++ show e)
                                                                                            Right e -> do
                                                                                                            addAssertions [e]
                                                                                                            randomSolve xs
                                                    _                        -> error "randomSolve - Problem and negation is unexpectedly not solvable"
                            
            _                     -> error "randomSolve - Problem is unexpectedly no longer solvable"

randomSolveBins :: ProblemSolver p =>  [(RefByName VarDef, Integer)] -> RandomM p ()
randomSolveBins [] = error "randomSolveBins should never be called with an empty list"
randomSolveBins ((v,d):xs) = do
    ctx <- toValExprContext
    case lookupVar (toName v) ctx of
        Nothing  -> error ("randomSolveBins - look up of variable (" ++ show v ++ ") unexpectedly failed")
        Just vd  -> case TorXakis.Var.sort vd of
                        SortBool    -> do
                                        -- since the variable can be changed both values are possible: pick one and be done
                                        b <- liftIO randomIO
                                        addAssertions [ justAssignment ctx v (Cbool b) ]
                                        randomSolve xs
                        SortInt     -> do
                                        bins <- randomIntegerConstraints (justVar ctx v)
                                        c@Cint{} <- solveWithConstraints v bins
                                        addAssertions [ justAssignment ctx v c ]
                                        randomSolve xs
                        SortChar    -> error "not supported yet"
                        SortRegex   -> error "TorXakis only supports regex constants, not variables"
                        SortString  -> if d > 0
                                        then do -- arbritrary string
                                                lengthName <- createNewVariableName
                                                case mkVarDef ctx lengthName SortInt of
                                                    Left e          -> error ("randomSolveBins - mkVarDef unexpectedly failed with " ++ show e)
                                                    Right lengthVar -> do
                                                                            declareVariables [lengthVar]
                                                                            ctx' <- toValExprContext
                                                                            let lengthRef :: RefByName VarDef
                                                                                lengthRef = RefByName lengthName
                                                                                lengthExpr = justVar ctx' lengthRef
                                                                            addAssertions [ justEqual ctx' (justLength ctx' (justVar ctx' v)) lengthExpr ]
                                                                            bins <- randomStringLengthConstraints lengthExpr
                                                                            val@(Cint l) <- solveWithConstraints lengthRef bins
                                                                            addAssertions [ justAssignment ctx' lengthRef val ]
                                                                            if d > 1 && l > 0
                                                                            then do
                                                                                varNames <- mapM (const createNewVariableName) [1..l]
                                                                                let eithers = Data.List.map (\n -> mkVarDef ctx n SortString) varNames
                                                                                case partitionEithers eithers of
                                                                                    ([], varDefs) -> do
                                                                                                        declareVariables varDefs
                                                                                                        ctx'' <- toValExprContext
                                                                                                        let varRefs :: [RefByName VarDef]
                                                                                                            varRefs = Data.List.map RefByName varNames
                                                                                                            exprs = Data.List.map (\(r,p) -> justEqual ctx'' (justVar ctx'' r) (justAt ctx'' (justVar ctx'' v) (justConst ctx'' (Cint p)))) (zip varRefs [0..])
                                                                                                        addAssertions exprs
                                                                                                        sxs <- liftIO $ shuffleM (xs ++ zip varRefs (repeat (-123)))
                                                                                                        randomSolve sxs
                                                                                    (es, _)       -> error ("randomSolveBins - mkVarDef unexpectedly failed with " ++ show es)
                                                                            else randomSolve xs
                                        else do -- string of length 1 (to represent an arbitrary character)
                                            bins <- randomStringCharConstraints (justVar ctx v)
                                            c@(Cstring s) <- solveWithConstraints v bins
                                            assert (1 == Data.Text.length s) $ do
                                                addAssertions [ justAssignment ctx v c ]
                                                randomSolve xs
                        SortADT ar  -> case lookupADT (toName ar) ctx of
                                            Nothing -> error ("Datatype " ++ show ar ++ " can unexpectedly not been found in context")
                                            Just ad -> do
                                                            (d', cr) <- case elemsConstructor ad of
                                                                            []   -> error ("Datatype " ++ show ar ++ " has unexpectedly no constructors")
                                                                            [cd] -> return (d, RefByName (constructorName cd))  -- no choice and no decrease of depth
                                                                            _    -> do
                                                                                        bins <- randomIsCstrConstraints ar (justVar ctx v)
                                                                                        (Ccstr ar' cr' _) <- solveWithConstraints v bins
                                                                                        assert (ar == ar') $ return (d-1, cr')
                                                            case mkIsCstr ctx ar cr (justVar ctx v) of
                                                                Left e  -> error ("mkIsCstr unexpectedly failed with " ++ show e)
                                                                Right e -> do
                                                                        addAssertions [ e ]
                                                                        case lookupConstructor (toName cr) ad of
                                                                            Nothing -> error ("Constructor " ++ show cr ++ " can unexpectedly not been found in adt " ++ show ar)
                                                                            Just cd -> if d' == 0 || null (elemsField cd)
                                                                                        then randomSolve xs
                                                                                        else do
                                                                                                eithers <- mapM (\f -> do 
                                                                                                                           n <- createNewVariableName
                                                                                                                           return $ mkVarDef ctx n (TorXakis.Sort.sort f)
                                                                                                                )
                                                                                                                (elemsField cd)
                                                                                                case partitionEithers eithers of
                                                                                                    ([], varDefs) -> do
                                                                                                                        declareVariables varDefs
                                                                                                                        ctx' <- toValExprContext
                                                                                                                        let varRefs :: [RefByName VarDef]
                                                                                                                            varRefs = Data.List.map (RefByName . TorXakis.Var.name) varDefs
                                                                                                                            exprs = Data.List.map (\(r,n) -> justEqual ctx' (justVar ctx' r) (justAccess ctx' ar cr (RefByName n) (justVar ctx' v))) (zip varRefs (map fieldName (elemsField cd)))
                                                                                                                        addAssertions exprs
                                                                                                                        sxs <- liftIO $ shuffleM (xs ++ zip varRefs (repeat d'))
                                                                                                                        randomSolve sxs
                                                                                                    (es, _)       -> error ("randomSolveBins - mkVarDef unexpectedly failed with " ++ show es)
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
