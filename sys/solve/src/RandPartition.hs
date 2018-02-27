{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  RandPartition
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental (?)
-- Portability :  portable
--
-- Module RandPartition
--
-- Randomization of SMT solutions
-- Interval partitioning of types
-----------------------------------------------------------------------------
module RandPartition
( randValExprsSolvePartition
, ParamPartition(..)
)
where

import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           System.IO
import           System.Random
import           System.Random.Shuffle

import ConstDefs
import SMT
import SMTData
import SolveDefs
import Sort
import SortOf
import ValExpr
import Variable

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
     if all ( (SortBool == ) . sortOf ) exprs
        then do
            push
            addDeclarations vs
            addAssertions exprs
            cnrss <- sequence   [ randCnrs p (cstrVar v) (maxDepth p)
                                | v <- vs
                                ]
            cnrss' <- lift $ shuffleM ( map Set.unions (cartProd cnrss) )
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
    if all ( (SortBool == ) . sortOf) (Set.toList cnrs)
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
randCnrsADT p vexp depth =
    case sortOf vexp of
        SortADT aRef -> do  aDefs <- gets adtDefs
                            let cstrs = Map.toList $ cDefsToMap $ constructors
                                            $ fromMaybe (error $ "RandPartition - randCnrsADT - Ref to ADT not found: " ++ show aRef)
                                                        $ Map.lookup aRef $ adtDefsToMap aDefs
                            cnrss <- sequence [ randCnrsCstr p aRef cstr vexp depth | cstr <- cstrs ]
                            return $ concat cnrss
        s   -> error $ "RandPartition - randCnrsADT - Impossible: " ++ show s

-- ----------------------------------------------------------------------------------------- --
-- give list of constraints for one constructor for vexp

randCnrsCstr :: Variable v => ParamPartition -> Ref (ADTDef Sort) -> (Ref (ConstructorDef Sort), ConstructorDef Sort) -> ValExpr v -> Int
                           -> SMT [ Set.Set (ValExpr v) ]
randCnrsCstr p aRef (cRef, cDef) vexp depth  =  do
     let ccCnr = cstrIsCstr aRef cRef vexp
     recCnrs <- sequence [ randCnrs p (cstrAccess aRef cRef pos s vexp) (depth-1)
                         | (s,pos) <- zip (map Sort.sort ( (fDefsToList . fields) cDef ) ) [0..]
                         ]
     return [ Set.insert ccCnr cnrs
            | cnrs <- map Set.unions (cartProd recCnrs)
            ]

-- | Give list of constraints forming a partitioning of the sort for vexp
-- 
-- Constraints are Sets of ValExpr, ie Sets are conjunctions
-- Lists of ValExpr, or of Sets of ValExpr, are disjunctions
randCnrs :: Variable v => ParamPartition -> ValExpr v -> Int -> SMT [ Set.Set (ValExpr v) ]
randCnrs p vexp depth  =
    let srt = sortOf vexp
    in  case srt of
            SortBool   -> randCnrsBool vexp
            SortInt    -> randCnrsInt p vexp
            SortString -> randCnrsString vexp
            SortADT _  -> if  depth > 0
                              then randCnrsADT p vexp depth
                              else return [ Set.empty ]
            s          -> error $ "RandPartition - randCnrs - Unexpected sort " ++ show s

-- | cartesian product
cartProd :: [[t]] -> [[t]]
cartProd =  foldr listProd [[]]
  where
    listProd sq acc  =  [ e:a | e <- sq, a <- acc ]
