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

import           Control.Exception.Base
import           Control.Monad.State
import qualified Data.Char             as Char
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as T
import           System.IO
import           System.Random
import           System.Random.Shuffle

import           ConstDefs
import           Identifier
import           SMT
import           SMTData
import           SolveDefs
import           Sort
import           SortOf
import           ValExpr
import           Variable

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
    if all ( (SortBool == ) . sortOf ) exprs
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
randomSolve _ []         _    = return ()                                         -- empty list -> done
randomSolve _ ((_,0):_)  _    = error "At maximum depth: should not be added"
randomSolve p ((v,d):xs) i    =
    case vsort v of
        SortBool -> do
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
        SortInt  -> do
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
        SortChar -> do
                        r <- lift $ randomRIO (0,127)
                        s <- randomSolveVar v (choicesFunc v r)
                        case s of
                            Cstring str | T.length str == 1 -> addAssertions [cstrEqual (cstrVar v) (cstrConst (Cstring str))]
                            Cstring _ ->  error "RandIncrementChoice: expected a single character"
                            _         -> error "RandIncrementChoice: impossible constant - char"
                        sat <- getSolvable
                        case sat of
                            Sat     -> randomSolve p xs i
                            _       -> error "Unexpected SMT issue - previous solution is no longer valid - char"
                    where
                        choicesFunc :: Variable v => v -> Int -> Const -> SMT [(Bool, Text)]
                        choicesFunc v' r (Cstring str) | T.length str == 1 = 
                                        let c = T.head str
                                            cond = r <= Char.ord c && Char.ord c <= r+127
                                        in do
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

        SortString -> do
                            r <- lift $ randomRIO (0,maxGeneratedStringLength p)
                            c <- randomSolveVar v (choicesFunc v r)
                            case c of
                                Cstring s   -> do
                                                    let l = T.length s
                                                    addAssertions [cstrEqual (cstrLength (cstrVar v)) (cstrConst (Cint (toInteger l)))]
                                                    if l > 0 && d > 1
                                                    then do
                                                            let charVars = map (\iNew -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) SortString) [i .. i+l-1]
                                                            addDeclarations charVars
                                                            let exprs = map (\(vNew,pos) -> cstrEqual (cstrVar vNew) (cstrAt (cstrVar v) (cstrConst (Cint pos)))) (zip charVars [0..])
                                                            addAssertions exprs
                                                            shuffledVars <- shuffleM (xs ++ zip charVars (map (const d) [1::Integer .. ]) )
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
        SortADT aRef -> do  aDefs <- gets adtDefs
                            let cstrs = Map.toList $ cDefsToMap $ constructors
                                            $ fromMaybe (error $ "RandPartition - randCnrsADT - Ref to ADT not found: " ++ show aRef)
                                                        $ Map.lookup aRef $ adtDefsToMap aDefs
                            case cstrs of
                                []  -> error $ "Unexpected: no constructor for " ++ show v
                                [(cRef,cDef)] -> -- no choice -- one constructor
                                        do
                                            addIsConstructor v aRef cRef
                                            fieldVars <- addFields v i aRef cRef cDef
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
                                            shuffledCstrs <- shuffleM $ map fst cstrs
                                            let (partA, partB) = splitAt (div (length cstrs) 2) shuffledCstrs
                                            c <- randomSolveVar v (choicesFunc v partA partB)
                                            case c of
                                                Cstr{cstrRef = cRef}  ->
                                                    case Map.lookup cRef $ Map.fromList cstrs of
                                                        Just cDef ->
                                                            do
                                                                addIsConstructor v aRef cRef
                                                                fieldVars <- if d > 1
                                                                                then addFields v i aRef cRef cDef
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
                            choicesFunc :: Variable v => v
                                                      -> [Ref ConstructorDef]
                                                      -> [Ref ConstructorDef]
                                                      -> Const
                                                      -> SMT [(Bool, Text)]
                            choicesFunc v' partA partB Cstr{adtRef = aRef', cstrRef = cRef} =
                                assert (aRef == aRef') $ do
                                    let cond = cRef `elem` partA
                                    lA <- mapM (\tempCRef -> valExprToString $ cstrIsCstr aRef tempCRef (cstrVar v')) partA
                                    lB <- mapM (\tempCRef -> valExprToString $ cstrIsCstr aRef tempCRef (cstrVar v')) partB
                                    return [ (    cond, case lA of
                                                            [a] -> a
                                                            _   -> "(or " <> T.intercalate " " lA <> ") ")
                                           , (not cond, case lB of
                                                            [b] -> b
                                                            _   -> "(or " <> T.intercalate " " lB <> ") ")
                                           ]
                            choicesFunc _ _ _ _ = error "RandIncrementChoice: impossible choice - string"
        s -> error $ "RandIncrementChoice: impossible choice - " ++ show s

-- | Find random solution for variable, using the different choices
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

addIsConstructor :: (Variable v) => v -> Ref ADTDef -> Ref ConstructorDef -> SMT ()
addIsConstructor v aRef cRef = addAssertions [cstrIsCstr aRef cRef (cstrVar v)]

addFields :: (Variable v) => v -> Int -> Ref ADTDef -> Ref ConstructorDef -> ConstructorDef -> SMT [v]
addFields v i aRef cRef cDef = do
    let sortVars  = map ( Sort.sort . snd ) $ (fDefsToList . fields) cDef
        fieldVars = map (\(iNew,sNew) -> cstrVariable ("$$$t$" ++ show iNew) (10000000+iNew) sNew)
                        $ zip [i .. ] sortVars
    addDeclarations fieldVars
    let exprs = map (\(pos, fieldVar, sortVar) -> cstrEqual (cstrVar fieldVar) (cstrAccess aRef cRef pos sortVar (cstrVar v))) (zip3 [0..] fieldVars sortVars)
    addAssertions exprs
    return fieldVars
