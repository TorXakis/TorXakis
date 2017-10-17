{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module RandPartition

-- ----------------------------------------------------------------------------------------- --
--
--   Module RandPartition :  Randomization of SMT solutions -  Interval partitioning of types
--
-- ----------------------------------------------------------------------------------------- --
-- export

( randValExprsSolvePartition  --  :: (Variable v) => [v] -> [ValExpr v] -> SMT (Satisfaction v)
, ParamPartition(..)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.IO
import System.Random
import Control.Monad.State

import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import StdTDefs
import SolveDefs
import SMT
import SMTData
import Utils

data ParamPartition = 
    ParamPartition { maxDepth               :: Int
                   , intHalf                :: Int
                   , intNum                 :: Int
                   , adtWidth               :: Int
                   }
    deriving (Eq,Ord,Read,Show)

-- ----------------------------------------------------------------------------------------- --
-- give a random solution for constraint vexps with free variables vars

randValExprsSolvePartition :: (Variable v) => ParamPartition -> [v] -> [ValExpr v] -> SMT (SolveProblem v)
randValExprsSolvePartition p vs exprs  = 
     -- if not all constraints are of type boolean: stop, otherwise solve the constraints
     if all ( (sortId_Bool == ) . sortOf ) exprs
        then do
            push
            addDeclarations vs
            addAssertions exprs
            cnrss <- sequence   [ randCnrs p (cstrVar v) (maxDepth p)
                                | v <- vs
                                ]
            cnrss' <- lift $ randOrder ( map Set.unions (cartProd cnrss) )
            sat    <- randValExprsSolvePartition' vs cnrss'
            pop
            return sat
        else do
            lift $ hPutStrLn stderr "TXS RandPartition randVexprsSolvePartition: Not all added constraints are Bool\n"
            return UnableToSolve
     

-- ----------------------------------------------------------------------------------------- --
-- give a solution for subspace with free variables vs
randValExprsSolvePartition' :: (Variable v) => [v] -> [Set.Set (ValExpr v)] -> SMT (SolveProblem v)
randValExprsSolvePartition' vs cnrss = randValExprsSolvePartition'' vs cnrss Unsolvable

randValExprsSolvePartition'' :: (Variable v) => [v] -> [Set.Set (ValExpr v)] -> SolveProblem v -> SMT (SolveProblem v)

randValExprsSolvePartition'' _ [] x           =  return x

randValExprsSolvePartition'' vs (cnrs:cnrss) x = 
    -- if not all constraints are of type boolean: stop, otherwise solve the constraints
    if all ( (sortId_Bool == ) . sortOf) (Set.toList cnrs)
    then do
        push
        -- addDeclarations already added in randValExprsSolvePartition
        addAssertions (Set.toList cnrs)
        sat <- getSolvable
        sp <- case sat of 
                    Sat     -> do
                                    sol <- getSolution vs
                                    return $ Solved sol
                    Unsat   -> return Unsolvable
                    Unknown -> return UnableToSolve
        pop
        case sp of
            Solved _         -> return sp
            Unsolvable       -> randValExprsSolvePartition'' vs cnrss x
            UnableToSolve    -> randValExprsSolvePartition'' vs cnrss UnableToSolve 
    else do
            lift $ hPutStrLn stderr "TXS RandPartition randValExprsSolvePartition'': Not all added constraints are Bool\n"
            return UnableToSolve

-- ----------------------------------------------------------------------------------------- --
-- n random positive Integers with binomial distribution 

randN :: ParamPartition -> Int -> SMT [Integer]
randN _ 0 = return []
randN p n = do r  <- lift randomIO::(SMT Float)                     -- random float in [0,1)
               rr <- randN p (n-1)
               let h = fromIntegral (intHalf p)
               return ( truncate ( (h-2) / (1-r) + 4 - h ) : rr )


-- ----------------------------------------------------------------------------------------- --
-- turn ordered Integer list into list of constraints for vexp
-- TODO: improve 1. handle empty / first item
--               2. handle next / last item     -> to save call to last 
-- TODO: make single set of constraints?
intList2cnrs :: (Variable v) => ValExpr v -> [Integer] -> [ Set.Set (ValExpr v) ]
intList2cnrs vexp list
  =   Set.singleton ( cstrLT vexp (cstrConst (Cint (head list) ) ) )
    : Set.singleton ( cstrGE vexp (cstrConst (Cint (last list) ) ) )
    : intList2cnrs' list
  where
    intList2cnrs' []        =  []
    intList2cnrs' [_]       =  []
    intList2cnrs' (l:r:rr)
      = Set.insert ( cstrGE vexp (cstrConst (Cint l) ) ) 
                   ( Set.singleton ( cstrLT vexp (cstrConst (Cint r) ) ) )
        : intList2cnrs' (r:rr)


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of Bool for vexp (of sort Bool)

randCnrsBool :: Ord v => ValExpr v -> SMT [ Set.Set (ValExpr v) ]
randCnrsBool vexp  =
     return [ Set.singleton ( cstrEqual vexp ( cstrConst ( Cbool False ) ) )
            , Set.singleton ( cstrEqual vexp ( cstrConst ( Cbool True ) ) )
            ]


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of Int for vexp (of sort Int)

randCnrsInt :: Variable v => ParamPartition-> ValExpr v -> SMT [ Set.Set (ValExpr v) ]
randCnrsInt p vexp  =  do
    let i = intNum p
    rpos  <- randN p i
    rneg  <- randN p i
    let rints = List.sort ( rpos ++ map ((-1)*) rneg ++ [-1,0,1] )
    return ( intList2cnrs vexp rints )


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of String for vexp (of sort String)

randCnrsString :: Variable v => ValExpr v -> SMT [ Set.Set (ValExpr v) ]
randCnrsString vexp  =
    return [ Set.singleton ( cstrEqual ( cstrLength vexp ) ( cstrConst (Cint 0) ) )
           , Set.singleton ( cstrEqual ( cstrLength vexp ) ( cstrConst (Cint 1) ) )
           , Set.singleton ( cstrGE    ( cstrLength vexp ) ( cstrConst (Cint 2) ) )
           ]


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of ADT sort for vexp

randCnrsADT :: Variable v => ParamPartition -> ValExpr v -> Int -> SMT [ Set.Set (ValExpr v) ]
randCnrsADT p vexp depth  =  do
    tdefs <- gets txsDefs
    let cstrs = [ def
                | def@( CstrId{cstrsort = srt}, _ ) <- Map.toList (cstrDefs tdefs)
                , srt == sortOf vexp
                ]
    cnrss <- sequence [ randCnrsCstr p cstr vexp depth | cstr <- cstrs ]
    return $ concat cnrss


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints for one constructor for vexp

randCnrsCstr :: Variable v => ParamPartition -> (CstrId,CstrDef) -> ValExpr v -> Int
                                -> SMT [ Set.Set (ValExpr v) ]
randCnrsCstr p (cid, CstrDef{}) vexp depth  =  do
     let ccCnr = cstrIsCstr cid vexp
     recCnrs <- sequence [ randCnrs p (cstrAccess cid pos vexp) (depth-1) | (_,pos) <- zip (cstrargs cid) [0..] ]
     return [ Set.insert ccCnr cnrs
            | cnrs <- map Set.unions (cartProd recCnrs)
            ]


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of the sort for vexp
-- 
-- Constraints are Sets of ValExpr, ie Sets are conjunctions
-- Lists of ValExpr, or of Sets of ValExpr, are disjunctions


randCnrs :: Variable v => ParamPartition -> ValExpr v -> Int -> SMT [ Set.Set (ValExpr v) ]
randCnrs p vexp depth  =
     let sort = sortOf vexp
      in if  sort == sortId_Bool
           then randCnrsBool vexp
           else if  sort == sortId_Int
                  then randCnrsInt p vexp
                  else if  sort == sortId_String
                         then randCnrsString vexp
                         else if  sort == sortId_Regex
                                then do lift $ hPutStrLn stderr "TXS RandPartition randCnrs: Regex can't be solved\n"
                                        return [ Set.empty ]
                                else if  depth > 0
                                       then randCnrsADT p vexp depth
                                       else return [ Set.empty ]
 

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
