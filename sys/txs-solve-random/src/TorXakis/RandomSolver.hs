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

-- | select random integer values
randomIntValues :: Int -> Integer -> Ratio -> IO [Integer]
randomIntValues n step r =
    let ranges = basicIntRanges n step r
     in do
        neg <- mapM randomRIO ranges
        pos <- mapM randomRIO ranges
        return $ reverse (map negate neg) ++ pos

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

-- | make the `ValExpression` constraints given the integer boundary values
mkConstraints :: ValExprContext c => c -> ValExpression -> [Integer] -> [ValExpression]
mkConstraints ctx _ []      = [justConst ctx (Cbool True)]
mkConstraints ctx v l@(x:_) = justLE ctx v (justConst ctx (Cint x)) : mkRestBins l
    where
        mkRestBins :: [Integer] -> [ValExpression]
        mkRestBins [y]          = [justLE ctx (justConst ctx (Cint y)) v]
        mkRestBins (y1:y2:ys)   = justAnd ctx [ justLT ctx (justConst ctx (Cint y1)) v
                                              , justLE ctx v (justConst ctx (Cint y2))
                                              ]
                                  : mkRestBins (y2:ys)
        mkRestBins []           = error "mkConstraints - unexpectedly mkRestBins called with empty list"

-- | make and randomize the `ValExpression` constraints
randomIntegerConstraints :: ProblemSolver p => ValExpression -> RandomM p [ValExpression]
randomIntegerConstraints v = do
            st <- get
            let n = nrOfBins st
                r = ratio st
              in do
                ns <- liftIO $ randomIntValues n 10 r
                ctx <- toValExprContext
                let cs = mkConstraints ctx v ns
                  in
                    liftIO $ shuffleM cs

-- | String generation
-- First fix  a string length
-- make and randomize the `ValExpression` constraints for string length
randomStringLengthConstraints :: ProblemSolver p => ValExpression -> RandomM p [ValExpression]
randomStringLengthConstraints v = do
            ns <- liftIO $ randomIntValues 3 3 Factor
            ctx <- toValExprContext
            let cs = mkConstraints ctx v ns
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
        nrOfChars = (ord regexRangeHigh) - (ord regexRangeLow) +1
        nrOfCharsInRange :: Int
        nrOfCharsInRange = 32

        makeConstraint :: ProblemSolver p => Int -> RandomM p ValExpression
        makeConstraint i | i <= nrOfChars - nrOfCharsInRange =
            let l = chr (i + ord regexRangeLow)
                u = chr (i + ord regexRangeLow + nrOfCharsInRange -1)
              in do
                ctx <- toValExprContext
                return $ justStrInRe ctx v (justConst ctx (Cregex (justRegexRange l u)))
        makeConstraint i                                     =
            let l = chr (i + ord regexRangeLow)
                u = chr (i `mod` (nrOfChars -1) + ord regexRangeLow)
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


-- | solve with constraints
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
                        SortADT ar  -> do
                                            case lookupADT (toName ar) ctx of
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


{-




-- ----------------------------------------------------------------------------------------- --
-- give a random solution for constraint vexps with free variables vars


randValExprsSolveIncrementBins :: (Variable v) => ParamIncrementBins -> [v] -> [ValExpr v] -> SMT (SolveProblem v)
randValExprsSolveIncrementBins p freevars exprs  =
    -- if not all constraints are of type boolean: stop, otherwise solve the constraints
    if all ( (sortIdBool == ) . sortOf ) exprs
    then do
        push
        addDeclarations freevars
        addAssertions exprs
        sat <- getSolvable
        sp <- case sat of
                Sat     -> do
                            shuffledVars <- shuffleM freevars
                            randomSolve p (zip shuffledVars (map (const (maxDepth p)) [1::Integer .. ]) ) 0
                            sat2 <- getSolvable
                            case sat2 of
                                Sat -> Solved <$> getSolution freevars
                                _   -> do
                                        lift $ hPutStrLn stderr "TXS RandIncrementBins randValExprsSolveIncrementBins: Problem no longer solvable\n"
                                        return UnableToSolve
                Unsat   -> return Unsolvable
                Unknown -> return UnableToSolve
        pop
        return sp
    else do
        lift $ hPutStrLn stderr "TXS RandIncrementBins randValExprsSolveIncrementBins: Not all added constraints are Bool\n"
        return UnableToSolve


toRegexString :: Int -> Text
toRegexString  45 = "\\-"
toRegexString  91 = "\\["
toRegexString  92 = "\\\\"
toRegexString  93 = "\\]"
toRegexString  94 = "\\^"
toRegexString   c = T.singleton (Char.chr c)

-- * configuration parameters
-- * List of tuples of (Variable, Depth)
-- * available variable id
randomSolve :: Variable v => ParamIncrementBins -> [(v, Int)] -> Int -> SMT ()
randomSolve _ []        _    = return ()                                         -- empty list -> done
randomSolve _ ((_,0):_) _    = error "At maximum depth: should not be added"
randomSolve p vs@((v,_):xs) i = do
        sat <- getSolvable
        case sat of
            Sat -> do
                sol <- getSolution [v]
                let val = fromMaybe (error "randomSolve - SMT hasn't returned the value of requested variable.")
                                    (Map.lookup v sol)
                push
                addAssertions [ cstrNot ( cstrEqual (cstrVar v) (cstrConst val) ) ]
                sat2 <- getSolvable
                pop
                case sat2 of
                    Sat -> randomSolveBins p vs i
                    _   -> do
                                addAssertions [ cstrEqual (cstrVar v) (cstrConst val) ]
                                randomSolve p xs i
            _   -> error "randomSolve - Unexpected SMT issue - previous solution is no longer valid"


randomSolveBins :: Variable v => ParamIncrementBins -> [(v, Int)] -> Int -> SMT ()
randomSolveBins _ [] _                                    = error "randomSolveBins - should always be called with no empty list"
randomSolveBins p ((v,_):xs) i    | vsort v == sortIdBool =
    -- since the variable can be changed both values are possible: pick one and be done
    do
        b <- liftIO randomIO
        addAssertions [ cstrEqual (cstrVar v) (cstrConst (Cbool b) ) ]
        randomSolve p xs i

randomSolveBins p ((v,_):xs) i    | vsort v == sortIdInt =
    do
        rndBins <- liftIO $ mkRndIntBins p (cstrVar v)
        val@Cint{} <- solveWithConstraints v rndBins
        addAssertions [ cstrEqual (cstrVar v) (cstrConst val) ]
        randomSolve p xs i

randomSolveBins p ((v,-123):xs) i    | vsort v == sortIdString =                 -- abuse depth to encode char versus string
    let nrofChars :: Int
        nrofChars = 256         -- nrofChars == nrOfRanges * nrofCharsInRange
        nrOfRanges :: Int
        nrOfRanges = 8
        nrofCharsInRange :: Int
        nrofCharsInRange = 32
        low :: Int
        low = 0
        high :: Int
        high = nrofChars -1
        boundaries :: [Int]
        boundaries = take (nrOfRanges-1) [0, nrofCharsInRange .. ]
      in do
        offset <- liftIO $ randomRIO (0,nrofCharsInRange-1)
        let rndBins = case offset of
                    0                               -> map ( toConstraint v . toCharGroup . (\b -> toCharRange b (b+nrofCharsInRange-1)) ) (nrofChars-nrofCharsInRange:boundaries)
                    1                               -> map ( toConstraint v . toCharGroup ) ( toCharRange (nrofChars - nrofCharsInRange + 1) high <> toRegexString low
                                                                                            : map (\b -> toCharRange (b+1) (b+nrofCharsInRange)) boundaries
                                                                                            )
                    n | n == nrofCharsInRange -1    -> map ( toConstraint v . toCharGroup ) ( toRegexString high <> toCharRange low (nrofCharsInRange-2)
                                                                                            : map ( (\b -> toCharRange b (b+nrofCharsInRange-1)) . (offset+) ) boundaries
                                                                                            )
                    _                               -> map ( toConstraint v . toCharGroup ) ( toCharRange (nrofChars - nrofCharsInRange + offset) high <> toCharRange low (offset-1)
                                                                                            : map ( (\b -> toCharRange b (b+nrofCharsInRange-1)) . (offset+) ) boundaries
                                                                                            )
        val@(Cstring s) <- solveWithConstraints v rndBins
        if T.length s /= 1
            then error "randomSolveBins - Unexpected Result - length of char must be 1"
            else do
                addAssertions [ cstrEqual (cstrVar v) (cstrConst val) ]
                randomSolve p xs i
    where
        toCharGroup :: Text -> Text
        toCharGroup t = "[" <> t <> "]"

        toCharRange :: Int -> Int -> Text
        toCharRange l h = toRegexString l <> "-" <> toRegexString h

        toConstraint :: v -> Text -> ValExpr v
        toConstraint v' t = cstrStrInRe (cstrVar v') (cstrConst (Cregex t))


randomSolveBins p ((v,d):xs) i    | vsort v == sortIdString =
    do
        let lengthStringVar = cstrVariable ("$$$l$" ++ show i) (10000000+i) sortIdInt
        addDeclarations [lengthStringVar]
        addAssertions [cstrEqual (cstrLength (cstrVar v)) (cstrVar lengthStringVar)]

        boundaryValues <- liftIO $ mapM randomRIO basicStringLengthRanges
        let bins = mkIntConstraintBins (cstrVar lengthStringVar) boundaryValues
        rndBins <- shuffleM bins
        val@(Cint l) <- solveWithConstraints lengthStringVar rndBins
        addAssertions [ cstrEqual (cstrVar lengthStringVar) (cstrConst val) ]
        if l > 0 && d > 1
            then do
                    let charVars = map (\iNew -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) sortIdString) [i+1 .. i+fromIntegral l]
                    addDeclarations charVars
                    let exprs = map (\(vNew,pos) -> cstrEqual (cstrVar vNew) (cstrAt (cstrVar v) (cstrConst (Cint pos)))) (zip charVars [0..])
                    addAssertions exprs
                    shuffledVars <- shuffleM (xs ++ zip charVars (map (const (-123)) [1::Integer .. ]) )
                    randomSolve p shuffledVars (i+1+fromIntegral l)
            else randomSolve p xs (i+1)

randomSolveBins p ((v,d):xs) i =
    do
        let sid = vsort v
        cstrs <- lookupCstrIds sid
        (cid, d') <- case cstrs of
                        []     -> error $ "Unexpected: no constructor for " ++ show v
                        [cid'] -> return (cid', d) -- No choice, no decrease of depth
                        _      -> do
                                    shuffledCstrs <- shuffleM cstrs
                                    let shuffledBins = map (\tempCid -> cstrIsCstr tempCid (cstrVar v)) shuffledCstrs
                                    Ccstr{cstrId = cid'} <- solveWithConstraints v shuffledBins
                                    return (cid', d-1)
        addIsConstructor v cid
        fieldVars <- if d' > 1 then addFields v i cid
                              else return []
        let l = length fieldVars
        if l > 0
            then do
                shuffledVars <- shuffleM (xs ++ zip fieldVars (map (const d') [1::Integer .. ]) )
                randomSolve p shuffledVars (i+l)
            else
                randomSolve p xs i

-- lookup constructors given its sort id
lookupCstrIds :: SortId -> SMT [CstrId]
lookupCstrIds sid  =  do
     edefs <- gets envDefs
     return [cstrid | cstrid@CstrId{cstrsort = sid'} <- Map.keys (cstrDefs edefs), sid == sid']

addIsConstructor :: (Variable v) => v -> CstrId -> SMT ()
addIsConstructor v cid = addAssertions [cstrIsCstr cid (cstrVar v)]

addFields :: (Variable v) => v -> Int -> CstrId -> SMT [v]
addFields v i cid@CstrId{ cstrargs = args' } = do
    let fieldVars = map (\(iNew,sNew) -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) sNew) (zip [i .. ] args')
    addDeclarations fieldVars
    edefs <- gets envDefs
    let mcdef = Map.lookup cid (cstrDefs edefs)
    case mcdef of
        Nothing               -> error $ "Unexpected: no constructor definition for " ++ show cid
        Just (CstrDef _ fIds) -> do
            let names = map FuncId.name fIds
                exprs = map (\(nm, pos, fieldVar) -> cstrEqual (cstrVar fieldVar) (cstrAccess cid nm pos (cstrVar v))) (zip3 names [0..] fieldVars)
            addAssertions exprs
            return fieldVars
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

-}