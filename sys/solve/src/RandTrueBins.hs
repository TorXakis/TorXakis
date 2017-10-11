{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Monad.State
import qualified Data.Char             as Char
import qualified Data.Map              as Map
import           Data.Monoid
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as T
import           System.IO
import           System.Random
import           System.Random.Shuffle

import           SMT
import           SMTData
import           SolveDefs
import           SolveDefs.Params
import           StdTDefs
import           TxsDefs

data ParamTrueBins =
    ParamTrueBins { maxDepth     :: Int
                  , next         :: Next
                  , nrOfBins     :: Int
                  , stringMode   :: StringMode
                  , stringLength :: Int
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
            [randvar] -> SMT.putT $ "(assert " <> randvar <> ")"
            _         -> do
                            shuffledList <- shuffleM randvars
                            SMT.putT $ "(assert (and " <> T.intercalate " " shuffledList <> ") )"

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
        combine :: (Variable v) => v -> SMT [Text] -> SMT [Text]
        combine vid sexprs = do
            expr <- randomValue p (vsort vid) (cstrVar vid) (maxDepth p)
            exprs' <- sexprs
            return (expr:exprs')

-- -----------------------------------------------------------------
nextFunction :: ParamTrueBins -> Integer -> Integer
nextFunction p =
    case RandTrueBins.next p  of
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

-- --------------------------------------
-- helper functions

shuffleOrList :: Variable v => [ValExpr v] -> SMT Text
shuffleOrList orList = do
    shuffledOrList <- shuffleM orList
    stringList <- mapM valExprToString shuffledOrList
    return $ "(or " <> T.intercalate " " stringList <> ") "

-- --------------------------------------
-- randomly draw boolean value
-- -----------------------------------------
trueBool :: (Variable v) => ValExpr v -> SMT Text
trueBool expr = do
    let orList = [expr, cstrNot expr]
    shuffleOrList orList

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
toBins v (x1:x2:xs) = cstrAnd ( Set.fromList [cstrLE (cstrConst (Cint x1)) v, cstrLT v (cstrConst (Cint x2)) ] ) : toBins v (x2:xs)
toBins _ _ = []

trueBins :: (Variable v) => ValExpr v -> Int -> (Integer -> Integer) -> SMT Text
trueBins v n nxt = do
    neg <- values n nxt
    pos <- values n nxt
    let binSamples = reverse (map negate neg) ++ pos
    let orList = [ cstrLT v (cstrConst (Cint (head binSamples) ))
                 , cstrLE (cstrConst (Cint (last binSamples) )) v
                 ]
                 ++ toBins v binSamples
    shuffleOrList orList

-- -----------------------------------------------------------------
-- enable randomly draw strings
-- ---------------------------------------------------------------------

toRegexString :: Int -> Text
toRegexString  45 = "\\-"
toRegexString  91 = "\\["
toRegexString  92 = "\\\\"
toRegexString  93 = "\\]"
toRegexString  94 = "\\^"
toRegexString   c = T.singleton (Char.chr c)

-- UTF8 - extended ascii 256 characters
-- related to the value of the smt (expert) option: string alphabet cardinality
-- (set-option :strings-alphabet-card n)
-- 256 by default in cvc4
low :: Int
low = 0
high :: Int
high = 255

range :: Text
range = "[" <> toRegexString low <> "-" <> toRegexString high <> "]"

trueCharRegex :: SMT Text
trueCharRegex = do
    charId <- lift $ randomRIO (low, high)
    case charId of
        x | x == low  -> return range
        x | x == high -> return $ "[" <> toRegexString high <> toRegexString low <> "-" <> toRegexString (high-1) <> "]"
        _             -> return $ "[" <> toRegexString charId <> "-" <> toRegexString high <> toRegexString low <> "-" <> toRegexString (charId-1) <> "]"

trueCharsRegex :: Int -> SMT Text
trueCharsRegex 0           = return ""
trueCharsRegex n | n > 0   = do
    hd <- trueCharRegex
    tl <- trueCharsRegex (n-1)
    return $ hd <> tl
trueCharsRegex n          = error ("trueCharsRegex: Illegal argument n = " ++ show n)

trueCharsRegexes :: Int -> SMT [Text]
trueCharsRegexes 0          = return [""]
trueCharsRegexes n | n > 0  = do
    hd <- trueCharsRegex n
    tl <- trueCharsRegexes (n-1)
    return $ hd:tl
trueCharsRegexes n          = error ("trueCharsRegexes: Illegal argument n = " ++ show n)

trueStringLength :: (Variable v) => Int -> ValExpr v -> SMT Text
trueStringLength n v = do
    let exprs = map (cstrEqual (cstrLength v) . cstrConst . Cint . toInteger ) [0..n] ++ [cstrGT (cstrLength v) (cstrConst (Cint (toInteger n)))]
    shuffledOrList <- shuffleM exprs
    stringList <- mapM valExprToString shuffledOrList
    return $ "(or " <> T.intercalate " " stringList <> ") "

trueStringRegex :: (Variable v) => Int -> ValExpr v -> SMT Text
trueStringRegex n v = do
    regexes <- trueCharsRegexes n
    sregexes <- shuffleM (regexes <> [range <> "{"<> (T.pack . show) (n+1) <> ",}"])               -- Performance gain in problem solver? Use string length for length 0 and greater than n
    let shuffledOrList = map (cstrStrInRe v . cstrConst . Cregex) sregexes
    stringList <- mapM valExprToString shuffledOrList
    return $ "(or " <> T.intercalate " " stringList <> ") "

trueString :: (Variable v) => ParamTrueBins -> ValExpr v -> SMT Text
trueString p v =
  case stringMode p of
    Regex  -> trueStringRegex  (stringLength p) v
    Length -> trueStringLength (stringLength p) v

-- -----------------------------------------------------------------
-- enable randomly draw constructors
-- ---------------------------------------------------------------------
-- lookup a constructor given its sort and constructor name
lookupConstructors :: SortId -> SMT [(CstrId, CstrDef)]
lookupConstructors sid  =  do
     tdefs <- gets txsDefs
     return [ def | def@(CstrId{ cstrsort = sid' } , _) <- Map.toList (cstrDefs tdefs), sid == sid']

randomValue :: (Variable v) => ParamTrueBins -> SortId -> ValExpr v -> Int -> SMT Text
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
                return $ "(or " <> T.intercalate " " shuffledOrList <> ") "
            where
                processConstructors :: (Variable v) => [(CstrId, CstrDef)] -> ValExpr v -> SMT [Text]
                processConstructors [] _ = return []
                processConstructors (x:xs) expr' = do
                    r <- processConstructor x expr'
                    rr <- processConstructors xs expr'
                    return (r:rr)

                processConstructor :: (Variable v) => (CstrId,CstrDef) -> ValExpr v -> SMT Text
                processConstructor (cid,CstrDef _isX []) expr' = valExprToString $ cstrIsCstr cid expr'
                processConstructor (cid, CstrDef _isX _accessors) expr' = do
                    cstr <- valExprToString $ cstrIsCstr cid expr'
                    args' <- processArguments cid (zip (cstrargs cid) [0..]) expr'
                    case args' of
                        [arg]   -> return $ "(ite " <> cstr <> " " <> arg <> " false) "
                        _       -> do
                                    shuffledAndList <- shuffleM args'
                                    return $ "(ite " <> cstr <> " (and " <> T.intercalate " " shuffledAndList <> ") false) "


                processArguments ::  (Variable v) => CstrId -> [(SortId,Int)] -> ValExpr v -> SMT [Text]
                processArguments _   [] _ =  return []
                processArguments cid ((sid',pos):xs) expr' = do
                    r <- randomValue p sid' (cstrAccess cid pos expr') (n-1)
                    rr <- processArguments cid xs expr'
                    return (r:rr)
randomValue _p _sid _expr n = error ("Illegal argument n = " ++ show n)
