{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
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
import TxsUtils
import SolveDefs
import SMT
import SMTData
import Utils

-- ----------------------------------------------------------------------------------------- --
-- give a random solution for constraint vexps with free variables vars

randValExprsSolvePartition :: (Variable v) => [v] -> [ValExpr v] -> SMT (SolveProblem v)
randValExprsSolvePartition vs exprs  = 
     -- if not all constraints are of type boolean: stop, otherwise solve the constraints
     if all ( (sortId_Bool == ) . sortOf ) exprs
        then do
            push
            addDeclarations vs
            addAssertions exprs
            param_max_rand_depth_string <- getParam "param_max_rand_depth"
            let param_max_rand_depth = read param_max_rand_depth_string
            cnrss <- sequence   [ randCnrs (cstrVar v) param_max_rand_depth
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
        -- why is addDeclarations not needed as in RandTrueBins?
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

randN :: Int -> SMT [Integer]
randN n  = 
     if n==0
       then return []
       else do r  <- lift randomIO::(SMT Float)                     -- random float in [0,1)
               rr <- randN (n-1)
               param_RandSolve_IntHalf_String <- getParam "param_RandSolve_IntHalf"
               let h = read param_RandSolve_IntHalf_String
               return ( truncate ( (h-2) / (1-r) + 4 - h ) : rr )


-- ----------------------------------------------------------------------------------------- --
-- turn ordered Integer list into list of constraints for vexp
-- TODO: improve 1. handle empty / first item
--               2. handle next / last item     -> to save call to last 
-- TODO: make single set of constraints?
intList2cnrs :: (Variable v) => ValExpr v -> [Integer] -> [ Set.Set (ValExpr v) ]
intList2cnrs vexp list
  =   Set.singleton ( cstrPredef SSI funcId_ltInt [ vexp, cstrConst(Cint(head list)) ] ) 
    : Set.singleton ( cstrPredef SSI funcId_geInt [ vexp, cstrConst(Cint(last list)) ] )
    : intList2cnrs' list
  where
    intList2cnrs' []        =  []
    intList2cnrs' [_]       =  []
    intList2cnrs' (l:r:rr)
      = Set.insert ( cstrPredef SSI funcId_geInt [ vexp, cstrConst (Cint l) ] )
          ( Set.singleton ( cstrPredef SSI funcId_ltInt [ vexp, cstrConst (Cint r) ] ) )
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

randCnrsInt :: Variable v => ValExpr v -> SMT [ Set.Set (ValExpr v) ]
randCnrsInt vexp  =  do
    param_RandSolve_IntNum_String <- getParam "param_RandSolve_IntNum"
    let param_RandSolve_IntNum = read param_RandSolve_IntNum_String
    rpos  <- randN param_RandSolve_IntNum
    rneg  <- randN param_RandSolve_IntNum
    let rints = List.sort ( rpos ++ map ((-1)*) rneg ++ [-1,0,1] )
    return ( intList2cnrs vexp rints )


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of String for vexp (of sort String)

randCnrsString :: Variable v => ValExpr v -> SMT [ Set.Set (ValExpr v) ]
randCnrsString vexp  =
     return [ Set.singleton ( cstrEqual (cstrFunc funcId_lenString [vexp]) (cstrConst(Cint 0)) )
            , Set.singleton ( cstrEqual (cstrFunc funcId_lenString [vexp]) (cstrConst(Cint 1)) )
            , Set.singleton
                ( cstrPredef SSI funcId_geInt [ cstrFunc funcId_lenString [vexp]
                                           , cstrConst(Cint 2)
                                           ]
                )
            ]


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of ADT sort for vexp

randCnrsADT :: Variable v => ValExpr v -> Int -> SMT [ Set.Set (ValExpr v) ]
randCnrsADT vexp depth  =  do
    tdefs <- gets txsDefs
    let cstrs = [ def
                | def@( CstrId{cstrsort = srt}, _ ) <- Map.toList (cstrDefs tdefs)
                , srt == sortOf vexp
                ]
    cnrss <- sequence [ randCnrsCstr cstr vexp depth | cstr <- cstrs ]
    return $ concat cnrss


-- ----------------------------------------------------------------------------------------- --
-- give list of constraints for one constructor for vexp

randCnrsCstr :: Variable v => (CstrId,CstrDef) -> ValExpr v -> Int
                                -> SMT [ Set.Set (ValExpr v) ]
randCnrsCstr (_, CstrDef cc fss) vexp depth  =  do
     let ccCnr = cstrPredef ACC cc [vexp]
     recCnrs <- sequence [ randCnrs (cstrFunc fs [vexp]) (depth-1) | fs <- fss ]
     return [ Set.insert ccCnr cnrs
            | cnrs <- map Set.unions (cartProd recCnrs)
            ]



-- ----------------------------------------------------------------------------------------- --
-- give list of constraints forming a partitioning of the sort for vexp
-- 
-- Constraints are Sets of ValExpr, ie Sets are conjunctions
-- Lists of ValExpr, or of Sets of ValExpr, are disjunctions


randCnrs :: Variable v => ValExpr v -> Int -> SMT [ Set.Set (ValExpr v) ]
randCnrs vexp depth  =
     let sort = sortOf vexp
      in if  sort == sortId_Bool
           then randCnrsBool vexp
           else if  sort == sortId_Int
                  then randCnrsInt vexp
                  else if  sort == sortId_String
                         then randCnrsString vexp
                         else if  sort == sortId_Regex
                                then do lift $ hPutStrLn stderr "TXS RandPartition randCnrs: Regex can't be solved\n"
                                        return [ Set.empty ]
                                else if  depth > 0
                                       then randCnrsADT vexp depth
                                       else return [ Set.empty ]
 

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

