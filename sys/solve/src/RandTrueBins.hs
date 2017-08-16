{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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
, ParamTrueBins(..)
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
import qualified Data.Set  as Set
import qualified Data.String.Utils as Utils

import SMT
import SMTData
import SolveDefs
import SolveDefs.Params
import StdTDefs
import TxsDefs
import TxsUtils

data ParamTrueBins = 
    ParamTrueBins { maxDepth                :: Int
                  , next                    :: Next
                  , nrOfBins                :: Int
                  , stringMode              :: StringMode
                  , stringLength            :: Int
                  }
    deriving (Eq,Ord,Read,Show)

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
        case randvars of
            [randvar] -> SMT.put $ "(assert " ++ randvar ++ ")"
            _         -> do
                            shuffledList <- shuffleM randvars
                            SMT.put $ "(assert (and " ++ Utils.join " " shuffledList ++ ") )"
        
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
        combine :: (Variable v) => v -> SMT [String] -> SMT [String]
        combine vid sexprs = do
            expr <- randomValue p (vsort vid) (cstrVar vid) (maxDepth p)
            exprs <- sexprs
            return $ (expr:exprs)

-- -----------------------------------------------------------------
nextFunction :: ParamTrueBins -> Integer -> Integer
nextFunction p = 
    case RandTrueBins.next p  of
        Linear      -> nextLinear
        Power       -> nextPower
        Exponent    -> nextExponent

step :: Integer
step = 10

nextLinear :: Integer -> Integer
nextLinear n = n + step

nextPower :: Integer -> Integer
nextPower n = n * step

nextExponent :: Integer -> Integer
nextExponent n = n * n

-- --------------------------------------
-- randomly draw boolean value
-- -----------------------------------------
trueBool :: (Variable v) => ValExpr v -> SMT String
trueBool expr = do
    let orList = [expr, cstrNot expr]
    shuffledOrList <- shuffleM orList
    stringList <- mapM valExprToString shuffledOrList
    return $ "(or " ++ Utils.join " " stringList ++ ") "
    
-- -----------------------------------------------------------------
-- randomly draw values from multiple bins
-- the size of the bins is controlled by the next function: either equal size, proportional larger, or exponential larger  
-- ---------------------------------------------------------------------
values :: Int -> (Integer -> Integer) -> SMT [Integer]
values n nxt = valueRecursive n 1 step
    where
        valueRecursive :: Int -> Integer -> Integer -> SMT [Integer]
        valueRecursive 0 _ _ = return []
        valueRecursive n' lw hgh = do
                r <- lift $ randomRIO (lw, hgh-1)
                rr <- valueRecursive (n'-1) hgh (nxt hgh)
                return (r:rr)
                
toBins :: (Variable v) => ValExpr v -> [Integer] -> [ValExpr v]
toBins v (x1:x2:xs) = cstrAnd ( Set.fromList [cstrFunc funcId_leInt [cstrConst (Cint x1), v], cstrFunc funcId_ltInt [v, cstrConst (Cint x2)] ] ) : toBins v (x2:xs)
toBins _ _ = []
                
trueBins :: (Variable v) => ValExpr v -> Int -> (Integer -> Integer) -> SMT String
trueBins v n nxt = do
    neg <- values n nxt
    pos <- values n nxt
    let binSamples = reverse (map negate neg) ++ pos
    let orList = [cstrFunc funcId_ltInt [v, cstrConst (Cint (head binSamples) )],
                  cstrFunc funcId_leInt [cstrConst (Cint (last binSamples) ), v] ] 
                 ++ toBins v binSamples
    shuffledOrList <- shuffleM orList
    stringList <- mapM valExprToString shuffledOrList
    return $ "(or " ++ Utils.join " " stringList ++ ") "
    
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

trueCharsRegex :: Int -> SMT String
trueCharsRegex 0           = return ""
trueCharsRegex n | n > 0   = do
    hd <- trueCharRegex
    tl <- trueCharsRegex (n-1)
    return $ hd ++ tl
trueCharsRegex n          = error ("trueCharsRegex: Illegal argument n = " ++ show n)
        
trueCharsRegexes :: Int -> SMT [String]
trueCharsRegexes 0          = return [""]
trueCharsRegexes n | n > 0  = do
    hd <- trueCharsRegex n
    tl <- trueCharsRegexes (n-1)
    return $ hd:tl
trueCharsRegexes n          = error ("trueCharsRegexes: Illegal argument n = " ++ show n)
    
trueStringLength :: (Variable v) => Int -> ValExpr v -> SMT String
trueStringLength n v = do
    let exprs = map (\m -> cstrEqual (cstrFunc funcId_lenString [v]) (cstrConst (Cint (toInteger m)))) [0..n] ++ [cstrFunc funcId_gtInt [cstrFunc funcId_lenString [v], cstrConst (Cint (toInteger n))]]
    shuffledOrList <- shuffleM exprs
    stringList <- mapM valExprToString shuffledOrList
    return $ "(or " ++ Utils.join " " stringList ++ ") "
    
trueStringRegex :: (Variable v) => Int -> ValExpr v -> SMT String
trueStringRegex n v = do
    regexes <- trueCharsRegexes n
    sregexes <- shuffleM (regexes ++ [range ++ "{"++ show (n+1) ++ ",}"])               -- Performance gain in problem solver? Use string length for length 0 and greater than n
    let shuffledOrList = map (\regex -> cstrFunc funcId_strinre [v, cstrConst (Cregex regex)]) sregexes
    stringList <- mapM valExprToString shuffledOrList
    return $ "(or " ++ Utils.join " " stringList ++ ") "
    
trueString :: (Variable v) => ParamTrueBins -> ValExpr v -> SMT String
trueString p v = 
    case stringMode p of
        Regex   -> trueStringRegex  (stringLength p) v
        Length  -> trueStringLength (stringLength p) v

-- -----------------------------------------------------------------
-- enable randomly draw constructors
-- ---------------------------------------------------------------------
-- lookup a constructor given its sort and constructor name
lookupConstructors :: SortId -> SMT [(CstrId, CstrDef)]
lookupConstructors sid  =  do
     tdefs <- gets txsDefs
     return [ def | def@(CstrId{ cstrsort = sid' } , _) <- Map.toList (cstrDefs tdefs), sid == sid']

randomValue :: (Variable v) => ParamTrueBins -> SortId -> ValExpr v -> Int -> SMT String
randomValue _ _sid _expr 0 = return "true"
randomValue p sid expr n | n > 0 = 
    case sid of
        x | x == sortId_Bool   -> trueBool expr
        x | x == sortId_Int    -> trueBins expr (nrOfBins p) (nextFunction p)
        x | x == sortId_String -> trueString p expr
        _ -> do
                cstrs <- lookupConstructors sid
                orList <- processConstructors cstrs expr
                shuffledOrList <- shuffleM orList
                return $ "(or " ++ Utils.join " " shuffledOrList ++ ") "
            where
                processConstructors :: (Variable v) => [(CstrId, CstrDef)] -> ValExpr v -> SMT [String]
                processConstructors [] _ = return []
                processConstructors (x:xs) expr' = do
                    r <- processConstructor x expr'
                    rr <- processConstructors xs expr'
                    return (r:rr)
        
                processConstructor :: (Variable v) => (CstrId,CstrDef) -> ValExpr v -> SMT String
                processConstructor (cid,CstrDef isX []) expr' = valExprToString $ cstrFunc isX [expr']
                processConstructor (cid,CstrDef isX accessors) expr' = do
                    cstr <- valExprToString $ cstrFunc isX [expr']
                    args <- processArguments (zip (cstrargs cid) accessors) expr'
                    case args of
                        [arg]   -> return $ "(ite " ++ cstr ++ " " ++ arg ++ " false) "
                        _       -> do
                                    shuffledAndList <- shuffleM args
                                    return $ "(ite " ++ cstr ++ " (and " ++ Utils.join " " shuffledAndList ++ ") false) "

                    
                processArguments ::  (Variable v) => [(SortId,FuncId)] -> ValExpr v -> SMT [String]
                processArguments [] _ =  return []
                processArguments ((sid',fid):xs) expr' = do
                    r <- randomValue p sid' (cstrFunc fid [expr']) (n-1)
                    rr <- processArguments xs expr'
                    return (r:rr)
randomValue _p _sid _expr n = error ("Illegal argument n = " ++ show n)
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --