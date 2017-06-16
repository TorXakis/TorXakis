{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module RandTrueBins

-- ----------------------------------------------------------------------------------------- --
--
--   Module RandTrueBins :  Randomization of SMT solutions -  Partitioning using True Bins
--
-- ----------------------------------------------------------------------------------------- --
-- export

( randValExprsSolveTrueBins  --  :: (Variable v) => [v] -> [ValExpr v] -> SMT (Satisfaction v)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Numeric (showHex)
import System.IO
import System.Random
import System.Random.Shuffle
import Control.Monad.State

import qualified Data.Char as Char
import qualified Data.Map  as Map

import SMT
import SMTData
import SolveDefs
import SolveDefs.Params
import StdTDefs
import TxsDefs
import TxsUtils

data ParamTrueBins = 
    ParamTrueBins { maxDepth                :: Int
                  , nrOfBins                :: Int
                  , next                    :: 
                  
                         }


-- ----------------------------------------------------------------------------------------- --
-- give a random solution for constraint vexps with free variables vars


randValExprsSolveTrueBins :: (Variable v) => ParamTrueBins -> [v] -> [ValExpr v] -> SMT (SolveProblem v)
randValExprsSolveTrueBins p freevars exprs  =
    -- if not all constraints are of type boolean: stop, otherwise solve the constraints
    if all ( (sortId_Bool == ) . sortOf ) exprs
    then do
        push
        addDeclarations freevars
        randvars <- foldr combine (return []) freevars
        addAssertions randvars
        addAssertions exprs
        sat <- getSolvable
        sp <- case sat of 
                Sat     -> do 
                            sol <- getSolution freevars
                            return $ Solved sol
                Unsat   -> return Unsolvable
                Unknown -> return UnableToSolve
        pop
        return sp
    else do
        lift $ hPutStrLn stderr "TXS RandTrueBins randValExprsSolveTrueBins: Not all added constraints are Bool\n"
        return UnableToSolve
    where
        combine :: (Variable v) => v -> SMT [ValExpr v] -> SMT [ValExpr v]
        combine vid sexprs = do
            expr <- randomValue p (vsort vid) (cstrVar vid) (maxDepth p)
            exprs' <- sexprs
            return $ expr : exprs'

-- -----------------------------------------------------------------
next :: ParamTrueBins -> SMT (Integer -> Integer)
next p = case next p  of
            Linear      -> return nextLinear
            Power       -> return nextPower
            Exponent    -> return nextExponent

step :: Integer
step = 10

nextLinear :: Integer -> Integer
nextLinear n = n + step

nextPower :: Integer -> Integer
nextPower n = n * step

nextExponent :: Integer -> Integer
nextExponent n = n * n
-- -----------------------------------------------------------------

toOr :: (Variable v) => [ValExpr v] -> ValExpr v
toOr [] = cstrConst (Cbool False)
toOr [x] = x
toOr (x:xs) = cstrFunc funcId_or [x, toOr xs]

toAnd :: (Variable v) => [ValExpr v] -> ValExpr v
toAnd [] = cstrConst (Cbool True)
toAnd [x] = x
toAnd (x:xs) = cstrFunc funcId_and [x, toAnd xs]

-- --------------------------------------
-- randomly draw boolean value
-- -----------------------------------------
trueBool :: (Variable v) => ValExpr v -> SMT (ValExpr v) 
trueBool expr = do
    let orList = [expr, cstrPredef SSB funcId_not [expr]]
    shuffledOrList <- shuffleM orList
    return $ toOr shuffledOrList
-- -----------------------------------------------------------------
-- randomly draw values from multiple bins
-- the size of the bins is controlled by the next function: either equal size, proportional larger, or exponential larger  
-- ---------------------------------------------------------------------
values :: Integer -> (Integer -> Integer) -> SMT [Integer]
values n nxt = valueRecursive n 1 step
    where
        valueRecursive :: Integer -> Integer -> Integer -> SMT [Integer]
        valueRecursive 0 _ _ = return []
        valueRecursive n' lw hgh = do
                r <- lift $ randomRIO (lw, hgh-1)
                rr <- valueRecursive (n'-1) hgh (nxt hgh)
                return (r:rr)
                
toBins :: (Variable v) => ValExpr v -> [Integer] -> [ValExpr v]
toBins v (x1:x2:xs) = cstrFunc funcId_and [cstrFunc funcId_leInt [cstrConst (Cint x1), v], cstrFunc funcId_ltInt [v, cstrConst (Cint x2)] ] : toBins v (x2:xs)
toBins _ _ = []
                
trueBins :: (Variable v) => ValExpr v -> Integer -> (Integer -> Integer) -> SMT (ValExpr v)
trueBins v n nxt = do
    neg <- values n nxt
    pos <- values n nxt
    let binSamples = reverse (map negate neg) ++ pos
    let orList = [cstrFunc funcId_ltInt [v, cstrConst (Cint (head binSamples) )],
                  cstrFunc funcId_leInt [cstrConst (Cint (last binSamples) ), v] ] 
                 ++ toBins v binSamples
    shuffledOrList <- shuffleM orList
    return $ toOr shuffledOrList
    
-- -----------------------------------------------------------------
-- enable randomly draw strings
-- ---------------------------------------------------------------------


toRegexString :: Int -> String
toRegexString  45 = "\\-"
toRegexString  91 = "\\["
toRegexString  92 = "\\\\"
toRegexString  93 = "\\]"
toRegexString  94 = "\\^"
toRegexString   c = [Char.chr c]

-- UTF8 - extended ascii 256 characters
-- related to the value of the smt (expert) option: string alphabet cardinality
-- (set-option :strings-alphabet-card n)
-- 256 by default in cvc4
low :: Int
low = 0
high :: Int
high = 255      

range :: String
range = "[" ++ toRegexString low ++ "-" ++ toRegexString high ++ "]"

trueCharRegex :: SMT String
trueCharRegex = do
    charId <- lift $ randomRIO (low, high)
    case charId of
        x | x == low  -> return range
        x | x == high -> return $ "[" ++ toRegexString high ++ toRegexString low ++ "-" ++ toRegexString (high-1) ++ "]"
        _             -> return $ "[" ++ toRegexString charId ++ "-" ++ toRegexString high ++ toRegexString low ++ "-" ++ toRegexString (charId-1) ++ "]"

trueCharsRegex :: Integer -> SMT String
trueCharsRegex 0           = return ""
trueCharsRegex n | n > 0   = do
    hd <- trueCharRegex
    tl <- trueCharsRegex (n-1)
    return $ hd ++ tl
trueCharsRegex n          = error ("trueCharsRegex: Illegal argument n = " ++ show n)
        
trueCharsRegexes :: Integer -> SMT [String]
trueCharsRegexes 0          = return [""]
trueCharsRegexes n | n > 0  = do
    hd <- trueCharsRegex n
    tl <- trueCharsRegexes (n-1)
    return $ hd:tl
trueCharsRegexes n          = error ("trueCharsRegexes: Illegal argument n = " ++ show n)
    
trueStringLength :: (Variable v) => Integer -> ValExpr v -> SMT (ValExpr v)
trueStringLength n v = do
    let exprs = map (\m -> cstrFunc funcId_eqInt [cstrFunc funcId_lenString [v], cstrConst (Cint m)]) [0..n] ++ [cstrFunc funcId_gtInt [cstrFunc funcId_lenString [v], cstrConst (Cint n)]]
    sexprs <- shuffleM exprs
    return $ toOr sexprs

trueStringRegex :: (Variable v) => Integer -> ValExpr v -> SMT (ValExpr v)
trueStringRegex n v = do
    regexes <- trueCharsRegexes n
    sregexes <- shuffleM (regexes ++ [range ++ "{"++ show(n+1) ++ ",}"])               -- Performance gain in problem solver? Use string length for length 0 and greater than n
    let sexprs = map (\regex -> cstrFunc funcId_strinre [v, cstrConst (Cregex regex)]) sregexes
    return $ toOr sexprs
    
trueString :: (Variable v) => ValExpr v -> SMT (ValExpr v)
trueString v = do
    param_TrueBins_StringLength_String <- getParam "param_TrueBins_StringLength"
    let param_TrueBins_StringLength = read param_TrueBins_StringLength_String
    
    param_TrueBins_StringMode_String <- getParam "param_TrueBins_StringMode"
    let param_TrueBins_StringMode = read param_TrueBins_StringMode_String
    
    case param_TrueBins_StringMode of
        Regex   -> trueStringRegex  param_TrueBins_StringLength v
        Length  -> trueStringLength param_TrueBins_StringLength v

-- -----------------------------------------------------------------
-- enable randomly draw constructors
-- ---------------------------------------------------------------------
-- lookup a constructor given its sort and constructor name
lookupConstructors :: SortId -> SMT [(CstrId, CstrDef)]
lookupConstructors sid  =  do
     tdefs <- gets txsDefs
     return [ def | def@(CstrId{ cstrsort = sid' } , _) <- Map.toList (cstrDefs tdefs), sid == sid']

randomValue :: (Variable v) => ParamTrueBins -> SortId -> ValExpr v -> Integer -> SMT (ValExpr v)
randomValue _ _sid _expr 0 = return $ cstrConst (Cbool True)
randomValue p sid expr n | n > 0 = 
    case sid of
        x | x == sortId_Bool   -> trueBool expr
        x | x == sortId_Int    -> do
                                    nxt <- RandTrueBins.next
                                    param_TrueBins_NrOfBins_String <- getParam "param_TrueBins_NrOfBins"
                                    let param_TrueBins_NrOfBins = read param_TrueBins_NrOfBins_String
                                    trueBins expr param_TrueBins_NrOfBins nxt
        x | x == sortId_String -> trueString expr
        _ -> do
                cstrs <- lookupConstructors sid
                orList <- processConstructors cstrs expr
                shuffledOrList <- shuffleM orList
                return (toOr shuffledOrList)
            where
                processConstructors :: (Variable v) => [(CstrId, CstrDef)] -> ValExpr v -> SMT [ValExpr v]
                processConstructors [] _ = return []
                processConstructors (x:xs) expr' = do
                    r <- processConstructor x expr'
                    rr <- processConstructors xs expr'
                    return (r:rr)
        
                processConstructor :: (Variable v) => (CstrId,CstrDef) -> ValExpr v -> SMT (ValExpr v)
                processConstructor (cid,CstrDef isX []) expr' = return $ cstrFunc isX [expr']
                processConstructor (cid,CstrDef isX accessors) expr' = do
                    args <- processArguments (zip (cstrargs cid) accessors) expr'
                    return $ cstrFunc funcId_and [cstrFunc isX [expr'], toAnd args]
                
                processArguments ::  (Variable v) => [(SortId,FuncId)] -> ValExpr v -> SMT [ValExpr v]
                processArguments [] _ =  return []
                processArguments ((sid',fid):xs) expr' = do
                    r <- randomValue sid' (cstrFunc fid [expr']) (n-1)
                    rr <- processArguments xs expr'
                    return (r:rr)
randomValue _sid _expr n = error ("Illegal argument n = " ++ show n)
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --


