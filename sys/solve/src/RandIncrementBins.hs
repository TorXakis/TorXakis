{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Char             as Char
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as T
import           System.IO
import           System.Random
import           System.Random.Shuffle

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
                        }
    deriving (Eq,Ord,Read,Show)

-- ---------------------------
step :: Integer
step = 10

nextFunction :: ParamIncrementBins -> Integer -> Integer
nextFunction p =
    case RandIncrementBins.next p  of
        Linear   -> (step +)
        Power    -> (step *)
        Exponent -> nextExponent

nextExponent :: Integer -> Integer
nextExponent n = n * n

mkRanges :: (Integer -> Integer) -> Integer -> Integer -> [(Integer, Integer)]
mkRanges nxt lw hgh = (lw, hgh-1): mkRanges nxt hgh (nxt hgh)

basicIntRanges :: ParamIncrementBins -> [(Integer, Integer)]
basicIntRanges p = take (nrOfBins p) (mkRanges (nextFunction p) 1 step)

basicStringLengthRanges :: [(Integer, Integer)]
basicStringLengthRanges = take 3 (mkRanges (3*) 0 3)

-- from ascending boundary values make intervals
mkIntConstraintBins :: forall v. Ord v => ValExpr v -> [Integer] -> [ValExpr v]
mkIntConstraintBins _ []     = [cstrConst (Cbool True)]       -- no boundary values, single interval
mkIntConstraintBins v l@(x:_) = cstrLE v (cstrConst (Cint x)) : mkRestBins l
    where
        mkRestBins :: Ord v => [Integer] -> [ValExpr v]
        mkRestBins [y]        = [cstrLT (cstrConst (Cint y)) v]
        mkRestBins (y1:y2:ys) = cstrAnd ( Set.fromList [cstrLT (cstrConst (Cint y1)) v, cstrLE v (cstrConst (Cint y2)) ] ) : mkRestBins (y2:ys)
        mkRestBins []         = error "mkIntConstraintBins - Should not happen - at least one element in mkRestBins"

mkRndIntBins :: Ord v => ParamIncrementBins -> ValExpr v -> IO [ValExpr v]
mkRndIntBins p v = do
    neg <- mapM randomRIO (basicIntRanges p)            -- faster to reuse the same bins for positive and negative?
    pos <- mapM randomRIO (basicIntRanges p)
    let boundaryValues = reverse (map negate neg) ++ pos
        bins = mkIntConstraintBins v boundaryValues
    shuffleM bins

findRndValue :: Variable v => v -> [ValExpr v] -> SMT Const
findRndValue _ [] = error "findRndValue - Solution exists, yet no solution found in all bins"
findRndValue v (x:xs) = do
    push
    addAssertions [x]
    sat <- getSolvable
    case sat of
        Sat -> do
                sol <- getSolution [v]
                pop
                let val = fromMaybe (error "findRndValue - SMT hasn't returned the value of requested variable.")
                                    (Map.lookup v sol)
                return val
        _   -> do
                    pop
                    findRndValue v xs
    
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
randomSolve :: Variable v => ParamIncrementBins -> [(v, Int)] -> Int -> SMT ()
randomSolve _ []        _    = return ()                                         -- empty list -> done
randomSolve _ ((_,0):_) _    = error "At maximum depth: should not be added"
randomSolve p vs@((v,_):xs) i = do
            sol <- getSolution [v]
            let val = fromMaybe (error "randomSolve - SMT hasn't returned the value of requested variable.")
                                (Map.lookup v sol)
            push
            addAssertions [ cstrNot ( cstrEqual (cstrVar v) (cstrConst val) ) ]
            sat <- getSolvable
            pop
            case sat of
                Sat -> randomSolveBins p vs i
                _   -> do
                            addAssertions [ cstrEqual (cstrVar v) (cstrConst val) ]
                            sat2 <- getSolvable
                            case sat2 of
                                Sat -> randomSolve p xs i
                                -- Both Unsat and Unknown: robustness if not equal can't be solve, 
                                --                         we expect that intervals might also yield no solution
                                _   -> error "randomSolve - Unexpected SMT issue - previous solution is no longer valid"


randomSolveBins :: Variable v => ParamIncrementBins -> [(v, Int)] -> Int -> SMT ()
randomSolveBins _ [] _                                    = error "randomSolveBins - should always be called with no empty list"
randomSolveBins p ((v,_):xs) i    | vsort v == sortIdBool =
    -- since the variable can be changed both values are possible: pick one and be done
    do
        b <- liftIO randomIO
        addAssertions [ cstrEqual (cstrVar v) (cstrConst (Cbool b) ) ]
        sat <- getSolvable
        case sat of
            Sat     -> randomSolve p xs i
            _       -> error "Unexpected SMT issue - previous solution is no longer valid - Bool"

randomSolveBins p ((v,_):xs) i    | vsort v == sortIdInt =
    do
        rndBins <- liftIO $ mkRndIntBins p (cstrVar v)
        val <- findRndValue v rndBins
        addAssertions [ cstrEqual (cstrVar v) (cstrConst val) ]
        sat <- getSolvable
        case sat of
            Sat -> randomSolve p xs i
            _   -> error "Unexpected SMT issue - previous solution is no longer valid - Int"
        

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
        val <- findRndValue v rndBins
        addAssertions [ cstrEqual (cstrVar v) (cstrConst val) ]
        sat <- getSolvable
        case sat of
            Sat -> randomSolve p xs i
            _   -> error "Unexpected SMT issue - previous solution is no longer valid - Int"
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
        val@(Cint l) <- findRndValue lengthStringVar rndBins
        addAssertions [ cstrEqual (cstrVar lengthStringVar) (cstrConst val) ]
        if l > 0 && d > 1
            then do
                    let charVars = map (\iNew -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) sortIdString) [i+1 .. i+fromIntegral l]
                    addDeclarations charVars
                    let exprs = map (\(vNew,pos) -> cstrEqual (cstrVar vNew) (cstrAt (cstrVar v) (cstrConst (Cint pos)))) (zip charVars [0..])
                    addAssertions exprs
                    shuffledVars <- shuffleM (xs ++ zip charVars (map (const (-123)) [1::Integer .. ]) )
                    sat <- getSolvable
                    case sat of
                        Sat     -> randomSolve p shuffledVars (i+1+fromIntegral l)
                        _       -> error "Unexpected SMT issue - previous solution is no longer valid - String - l > 0 && d > 1"
            else do
                    sat <- getSolvable
                    case sat of
                        Sat     -> randomSolve p xs (i+1)
                        _       -> error "Unexpected SMT issue - previous solution is no longer valid - String - else"

randomSolveBins p ((v,d):xs) i =
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
                        let shuffledBins = map (\(tempCid, _) -> cstrIsCstr tempCid (cstrVar v)) shuffledCstrs
                        Cstr{cstrId = cid} <- findRndValue v shuffledBins
                        addIsConstructor v cid
                        fieldVars <- if d > 1 then addFields v i cid
                                              else return []
                        sat <- getSolvable
                        case sat of
                            Sat -> do
                                    let l = length fieldVars
                                    if l > 0
                                        then do
                                            shuffledVars <- shuffleM (xs ++ zip fieldVars (map (const (d-1)) [1::Integer .. ]) )
                                            randomSolve p shuffledVars (i+l)
                                        else
                                            randomSolve p xs i
                            _       -> error "Unexpected SMT issue - previous solution is no longer valid - ADT - n"

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