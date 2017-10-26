{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE OverloadedStrings #-}
module RandIncrementChoice
-- ----------------------------------------------------------------------------------------- --
--
--   Module RandIncrementChoice :  Randomization of SMT solutions - Solving by incrementally fixing one choice at a time
--
-- ----------------------------------------------------------------------------------------- --
-- export
( randValExprsSolveIncrementChoice  --  :: (Variable v) => [v] -> [ValExpr v] -> SMT (Satisfaction v)
, ParamIncrementChoice (..)
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
import           Data.Text             (Text)
import qualified Data.Text             as T

import           SMT
import           SMTData
import           SolveDefs
import           StdTDefs
import           TxsDefs


data ParamIncrementChoice =
    ParamIncrementChoice { maxDepth                 :: Int
                         , intRange                 :: Int
                         , intPower                 :: Int
                         , maxGeneratedStringLength :: Int
                         }
    deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
-- give a random solution for constraint vexps with free variables vars


randValExprsSolveIncrementChoice :: (Variable v) => ParamIncrementChoice -> [v] -> [ValExpr v] -> SMT (SolveProblem v)
randValExprsSolveIncrementChoice p freevars exprs  =
    -- if not all constraints are of type boolean: stop, otherwise solve the constraints
    if all ( (sortId_Bool == ) . sortOf ) exprs
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
        lift $ hPutStrLn stderr "TXS RandIncrementChoice randValExprsSolveIncrementChoice: Not all added constraints are Bool\n"
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
-- * List of tuples of (Variable, Depth)
-- * available variable id
randomSolve :: Variable v => ParamIncrementChoice -> [(v, Int)] -> Int -> SMT ()
randomSolve _ []        _     = return ()                                         -- empty list -> done
randomSolve _ ((_,0):_) _     = error "At maximum depth: should not be added"     -- todo: remove for performance gain!
randomSolve p ((v,_):xs) i    | vsort v == sortId_Bool =
    do
        b <- lift randomIO
        c <- randomSolveVar v (choicesFunc v b)
        case c of
            Cbool x -> addAssertions [cstrEqual (cstrVar v) (cstrConst (Cbool x))]
            _       -> error "RandIncrementChoice: impossible constant - bool"
        sat <- getSolvable
        case sat of
            Sat     -> randomSolve p xs i
            _       -> error "Unexpected SMT issue - previous solution is no longer valid - Bool"
    where
        choicesFunc :: Variable v => v -> Bool -> Const -> SMT [(Bool, Text)]
        choicesFunc v' b (Cbool b')  = do
                                         let cond = b == b'
                                         st <- valExprToString $ cstrEqual (cstrVar v') (cstrConst (Cbool b))
                                         sf <- valExprToString $ cstrEqual (cstrVar v') (cstrConst (Cbool (not b)))
                                         return [ (cond, st)
                                                , (not cond, sf)
                                                ]
        choicesFunc _ _ _        = error "RandIncrementChoice: impossible choice - bool"

randomSolve p ((v,_):xs) i    | vsort v == sortId_Int =
    do
        let range = intRange p
        b <- lift $ randomRIO (- range, range)
        let power = intPower p
        let r = if b < 0 then - abs b ^ power
                         else b ^ power
        c <- randomSolveVar v (choicesFunc v r)
        case c of
            Cint x  -> addAssertions [cstrEqual (cstrVar v) (cstrConst (Cint x))]
            _       -> error "RandIncrementChoice: impossible constant - int"
        sat <- getSolvable
        case sat of
            Sat     -> randomSolve p xs i
            _       -> error "Unexpected SMT issue - previous solution is no longer valid - Int"
    where
        choicesFunc :: Variable v => v -> Int -> Const -> SMT [(Bool, Text)]
        choicesFunc v' r (Cint x)  = do
                                        let r' = toInteger r
                                            cond = x < r'
                                        st <- valExprToString $ cstrLT (cstrVar v') (cstrConst (Cint r'))
                                        sf <- valExprToString $ cstrGE (cstrVar v') (cstrConst (Cint r'))
                                        return [ (cond, st)
                                               , (not cond, sf)
                                               ]
        choicesFunc _ _ _         = error "RandIncrementChoice: impossible choice - int"

randomSolve p ((v,-123):xs) i    | vsort v == sortId_String =                 -- abuse depth to encode char versus string
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


randomSolve p ((v,d):xs) i    | vsort v == sortId_String =
    do
        r <- lift $ randomRIO (0,maxGeneratedStringLength p)
        c <- randomSolveVar v (choicesFunc v r)
        case c of
            Cstring s   -> do
                                let l = T.length s
                                addAssertions [cstrEqual (cstrLength (cstrVar v)) (cstrConst (Cint (toInteger l)))]
                                if l > 0 && d > 1
                                then do
                                        let charVars = map (\iNew -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) sortId_String) [i .. i+l-1]
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
     tdefs <- gets txsDefs
     return [(cstrid, cdef) | (cstrid@(CstrId _ _ _ sid'), cdef) <- Map.toList (cstrDefs tdefs), sid == sid']

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
