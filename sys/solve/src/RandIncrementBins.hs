{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE OverloadedStrings #-}
module RandIncrementBins
-- ----------------------------------------------------------------------------------------- --
--
-- Module RandIncrementBins :  Randomization of SMT solutions 
-- Solving by incrementally fixing one variable at a time
-- When the variable can be changed, 
-- bins are created and shuffled to find a random solution.
--
-- ----------------------------------------------------------------------------------------- --
-- export
( randValExprsSolveIncrementBins
, ParamIncrementBins (..)
)
where
-- ----------------------------------------------------------------------------------------- --
-- import
import           Control.Monad.State
import           System.IO
import           System.Random
import           System.Random.Shuffle

import qualified Data.Char             as Char
import qualified Data.Map              as Map
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as T

import           ConstDefs
import           CstrDef
import           CstrId
import           SMT
import           SMTData
import           Solve.Params
import           SolveDefs
import           SortId
import           SortOf
import           ValExpr
import           Variable


data ParamIncrementBins =
     ParamIncrementBins { maxDepth                 :: Int
                        , next                     :: Next
                        , nrOfBins                 :: Int
                        , maxGeneratedStringLength :: Int
                        }
    deriving (Eq,Ord,Read,Show)

-- ---------------------------
nextFunction :: ParamIncrementBins -> Integer -> Integer
nextFunction p =
    case RandIncrementBins.next p  of
        Linear   -> nextLinear
        Power    -> nextPower
        Exponent -> nextExponent

step :: Integer
step = 10

nextLinear :: Integer -> Integer
nextLinear n = n + step

nextPower :: Integer -> Integer
nextPower n = n * step

nextExponent :: Integer -> Integer
nextExponent n = n * n

-- -----------------------------------------------------------------
-- randomly draw values from multiple bins
-- the number of bins is controlled by `nrOfBins`
-- the size of the bins is controlled by `nextFunction`
-- ---------------------------------------------------------------------
values :: ParamIncrementBins -> IO [Integer]
values p = valueRecursive (nrOfBins p) 1 step
    where
        valueRecursive :: Int -> Integer -> Integer -> IO [Integer]
        valueRecursive 0 _ _ = return []
        valueRecursive n' lw hgh = do
                r <- randomRIO (lw, hgh-1)
                rr <- valueRecursive (n'-1) hgh (nextFunction p hgh)
                return (r:rr)

toBins :: (Variable v) => ParamIncrementBins -> ValExpr v -> IO [ValExpr v]
toBins p v = do
    neg <- liftIO $ values p            -- faster to reuse the same bins for positive and negative?
    pos <- liftIO $ values p
    let binSamples = reverse (map negate neg) ++ pos
    liftIO shuffleM ( [ cstrLT v (cstrConst (Cint (head binSamples) ))
                      , cstrLE (cstrConst (Cint (last binSamples) )) v
                      ]
                      ++ midBins v binSamples
                    )
      where
        midBins :: (Variable v) => ValExpr v -> [Integer] -> [ValExpr v]
        midBins v (x1:x2:xs) = cstrAnd ( Set.fromList [cstrLE (cstrConst (Cint x1)) v, cstrLT v (cstrConst (Cint x2)) ] ) : midBins v (x2:xs)
        midBins _ _ = []

findRndValue :: Variable v => v -> [ValExpr v] -> SMT ()
findRndValue _ [] = error "findRndValue - Solution exists, yet no solution found in all bins"
findRndValue v (x:xs) = do
    push
    addAssertions [x]
    sat <- getSolvable
    pop
    case sat of
        Sat -> do
                sol <- getSolution [v]
                let val = fromMaybe (error "findRndValue - SMT hasn't returned the value of requested variable.")
                                    (Map.lookup (vname v) sol)
                    in do
                        addAssertions [ cstrEq (cstrVar v) (cstrConst val) ]
                        sat2 <- getSolvable
                        case sat2 of
                            Sat -> return
                            _   -> "Unexpected SMT issue - previous solution is no longer valid - findRndValue"
        _   -> findRndValue v xs
    
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
                            initSol <- getSolution freevars
                            shuffledVars <- shuffleM freevars
                            randomSolve p initSol (zip shuffledVars (map (const (maxDepth p)) [1::Integer .. ]) ) 0
                            sol <- getSolution freevars
                            return $ Solved sol
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

-- precondition: SMT has returned sat
--
-- * configuration parameters
-- * Current solution
-- * List of tuples of (Variable, Depth)
-- * available variable id
randomSolve :: Variable v => ParamIncrementBins -> Solution v -> [(v, Int)] -> Int -> SMT ()
randomSolve _ _ []        _    = return ()                                         -- empty list -> done
randomSolve _ _ ((_,0):_) _    = error "At maximum depth: should not be added"
randomSolve p sol ((v,d):xs) i = 
    let val = fromMaybe (error "randomSolve - SMT hasn't returned the value of requested variable.")
                        (Map.lookup (vname v) sol)
        in do 
            push
            addAssertions [ cstrNot ( cstrEq (cstrVar v) (cstrConst val) ) ]
            sat <- getSolvable
            pop
            case sat of
                Sat -> randomSolveBin p ((v,d):xs) i
                _   -> do
                            addAssertions [ cstrEq (cstrVar v) (cstrConst val) ]
                            randomSolve    p sol        xs  i -- Both Unsat and Unknown: robustness if not equal can be solve, 
                                                              --                         we expect that intervals might also yield no solution


randomSolveBins :: Variable v => ParamIncrementBins -> [(v, Int)] -> Int -> SMT ()
randomSolveBins p ((v,_):xs) i    | vsort v == sortIdBool =
    -- since the variable can be changed both values are possible: pick one and be done
    do
        b <- liftIO randomIO
        addAssertions [ cstrEq (cstrVar v) (cstrConst (Cbool b) ) ]
        sat <- getSolvable
        case sat of
            Sat     -> do
                        newSol <- getSolution xs
                        randomSolve p newSol xs i
            _       -> error "Unexpected SMT issue - previous solution is no longer valid - Bool"

randomSolveBins p ((v,_):xs) i    | vsort v == sortIdInt =
    do
        rndBins <- toBins p (cstrVar v)
        findRndValue v rndBins
        newSol <- getSolution xs
        randomSolve p newSol xs i

-- HERE
randomSolve p ((v,-123):xs) i    | vsort v == sortIdString =                 -- abuse depth to encode char versus string
    do
        r <- lift $ randomRIO (0,127)
        s <- randomSolveVar v (choicesFunc v r)
        case s of
            Cstring str | T.length str == 1 ->
              addAssertions [cstrEqual (cstrVar v) (cstrConst (Cstring str))]
            Cstring _ ->
              error "RandIncrementChoice: expected a single character"
            _           -> error "RandIncrementChoice: impossible constant - char"
        sat <- getSolvable
        case sat of
            Sat     -> randomSolve p xs i
            _       -> error "Unexpected SMT issue - previous solution is no longer valid - char"
    where
        choicesFunc :: Variable v => v -> Int -> Const -> SMT [(Bool, Text)]
        choicesFunc v' r (Cstring str) |
          T.length str == 1 = do
            let
              c = T.head str
              cond = r <= Char.ord c && Char.ord c <= r+127
            st <- valExprToString $ cstrStrInRe (cstrVar v') (cstrConst (Cregex ("[" <> toRegexString r <> "-" <> toRegexString (r+127) <> "]")))
            sf <- valExprToString $
              case r of
                0       -> cstrStrInRe (cstrVar v') (cstrConst (Cregex ("[" <> toRegexString 128 <> "-" <> toRegexString 255 <> "]")))
                1       -> cstrStrInRe (cstrVar v') (cstrConst (Cregex ("[" <> toRegexString 129 <> "-" <> toRegexString 255 <> toRegexString 0 <> "]")))
                127     -> cstrStrInRe (cstrVar v') (cstrConst (Cregex ("[" <> toRegexString 255 <> toRegexString 0 <> "-" <> toRegexString 126 <> "]")))
                _       -> cstrStrInRe (cstrVar v') (cstrConst
                                                               (Cregex ("[" <> toRegexString (r+128) <> "-" <> toRegexString 255 <> toRegexString 0 <> "-" <> toRegexString (r-1) <> "]")))
            return [ (cond, st), (not cond, sf) ]
        choicesFunc _ _ _         = error "RandIncrementChoice: impossible choice - char"


randomSolve p ((v,d):xs) i    | vsort v == sortIdString =
    do
        r <- lift $ randomRIO (0,maxGeneratedStringLength p)
        c <- randomSolveVar v (choicesFunc v r)
        case c of
            Cstring s   -> do
                                let l = T.length s
                                addAssertions [cstrEqual (cstrLength (cstrVar v)) (cstrConst (Cint (toInteger l)))]
                                if l > 0 && d > 1
                                then do
                                        let charVars = map (\iNew -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) sortIdString) [i .. i+l-1]
                                        addDeclarations charVars
                                        let exprs = map (\(vNew,pos) -> cstrEqual (cstrVar vNew) (cstrAt (cstrVar v) (cstrConst (Cint pos)))) (zip charVars [0..])
                                        addAssertions exprs
                                        shuffledVars <- shuffleM (xs ++ zip charVars (map (const (-123)) [1::Integer .. ]) )
                                        sat <- getSolvable
                                        case sat of
                                            Sat     -> randomSolve p shuffledVars (i+l)
                                            _       -> error "Unexpected SMT issue - previous solution is no longer valid - String - l > 0"
                                else do
                                        sat <- getSolvable
                                        case sat of
                                            Sat     -> randomSolve p xs i
                                            _       -> error "Unexpected SMT issue - previous solution is no longer valid - String - l == 0"
            _              -> error "RandIncrementChoice: impossible constant - string"
    where
        choicesFunc :: Variable v => v -> Int -> Const -> SMT [(Bool, Text)]
        choicesFunc v' r (Cstring s) = do
                                            let cond = T.length s < r
                                            st <- valExprToString $ cstrLT (cstrLength (cstrVar v')) (cstrConst (Cint (toInteger r)))
                                            sf <- valExprToString $ cstrGE (cstrLength (cstrVar v')) (cstrConst (Cint (toInteger r)))
                                            return [ (cond, st)
                                                   , (not cond, sf)
                                                   ]
        choicesFunc _ _ _         = error "RandIncrementChoice: impossible choice - string"


randomSolve p ((v,d):xs) i =
    do
        let sid = vsort v
        cstrs <- lookupConstructors sid
        case cstrs of
            []  -> error $ "Unexpected: no constructor for " ++ show v
            [(cid,_)] -> -- no choice -- one constructor
                    do
                        addIsConstructor v cid
                        fieldVars <- addFields v i cid
                        sat <- getSolvable
                        case sat of
                            Sat     -> do
                                            let l = length fieldVars
                                            if l > 0
                                                then do
                                                    shuffledVars <- shuffleM (xs ++ zip fieldVars (map (const d) [1::Integer .. ]) )
                                                    randomSolve p shuffledVars (i+l)
                                                else
                                                    randomSolve p xs i
                            _       -> error "Unexpected SMT issue - previous solution is no longer valid - ADT - 1"
            _   ->
                    do
                        shuffledCstrs <- shuffleM cstrs
                        let (partA, partB) = splitAt (div (length cstrs) 2) shuffledCstrs
                        c <- randomSolveVar v (choicesFunc v partA partB)
                        case c of
                            Cstr{cstrId = cid}  ->
                                case Map.lookup cid (Map.fromList cstrs) of
                                    Just CstrDef{} ->
                                        do
                                            addIsConstructor v cid
                                            fieldVars <- if d > 1 then addFields v i cid
                                                                  else return []
                                            sat2 <- getSolvable
                                            case sat2 of
                                                Sat     -> do
                                                                let l = length fieldVars
                                                                if l > 0
                                                                    then do
                                                                        shuffledVars <- shuffleM (xs ++ zip fieldVars (map (const (d-1)) [1::Integer .. ]) )
                                                                        randomSolve p shuffledVars (i+l)
                                                                    else
                                                                        randomSolve p xs i
                                                _       -> error "Unexpected SMT issue - previous solution is no longer valid - ADT - n"
                                    Nothing                 -> error "RandIncrementChoice: value not found - ADT - n"
                            _                   -> error "RandIncrementChoice: impossible constant - ADT - n"
    where
        choicesFunc :: Variable v => v -> [(CstrId, CstrDef)] -> [(CstrId, CstrDef)] -> Const -> SMT [(Bool, Text)]
        choicesFunc v' partA partB Cstr{cstrId = cId} =
            do
                let cond = Map.member cId (Map.fromList partA)
                lA <- mapM (\(tempCid,CstrDef{}) -> valExprToString $ cstrIsCstr tempCid (cstrVar v')) partA
                lB <- mapM (\(tempCid,CstrDef{}) -> valExprToString $ cstrIsCstr tempCid (cstrVar v')) partB
                return [ (cond, case lA of
                                    [a] -> a
                                    _   -> "(or " <> T.intercalate " " lA <> ") ")
                       , (not cond, case lB of
                                        [b] -> b
                                        _   -> "(or " <> T.intercalate " " lB <> ") ")
                       ]
        choicesFunc _ _ _ _        = error "RandIncrementChoice: impossible choice - string"


-- Find random solution for variable, using the different choices
randomSolveVar :: (Variable v) => v -> (Const -> SMT [(Bool, Text)]) -> SMT Const
randomSolveVar v choicesFunc = do
    sol <- getSolution [v]
    case Map.lookup v sol of
        Just c  -> do
                        choices <- choicesFunc c
                        shuffledChoices <- shuffleM choices
                        case head shuffledChoices of
                            (True , _)          -> return c
                            (False, assertion)  -> do
                                                        push
                                                        SMT.putT $ "(assert " <> assertion <> ")"
                                                        sat <- getSolvable
                                                        case sat of
                                                            Sat -> do
                                                                        sol2 <- getSolution [v]
                                                                        pop
                                                                        case Map.lookup v sol2 of
                                                                            Just c2 -> return c2
                                                                            _       -> error "RandIncrementChoice: value not found - randomSolveVar - 2"

                                                            _   -> do
                                                                        pop
                                                                        return c
        _       -> error "RandIncrementChoice: value not found - randomSolveVar"

-- lookup a constructor given its sort and constructor name
lookupConstructors :: SortId -> SMT [(CstrId, CstrDef)]
lookupConstructors sid  =  do
     edefs <- gets envDefs
     return [(cstrid, cdef) | (cstrid@(CstrId _ _ _ sid'), cdef) <- Map.toList (cstrDefs edefs), sid == sid']

addIsConstructor :: (Variable v) => v -> CstrId -> SMT ()
addIsConstructor v cid = addAssertions [cstrIsCstr cid (cstrVar v)]

addFields :: (Variable v) => v -> Int -> CstrId -> SMT [v]
addFields v i cid@CstrId{ cstrargs = args' } = do
    let fieldVars = map (\(iNew,sNew) -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) sNew) (zip [i .. ] args')
    addDeclarations fieldVars
    let exprs = map (\(pos, fieldVar) -> cstrEqual (cstrVar fieldVar) (cstrAccess cid pos (cstrVar v))) (zip [0..] fieldVars)
    addAssertions exprs
    return fieldVars
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
