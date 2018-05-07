{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module SolveRandParam
( SolveRandParam(..)
, toRandParam
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map  as Map

import Solve.Params(Params,SolveStrategy(..))

import RandPartition
import RandTrueBins
import RandIncrementChoice

-- ----------------------------------------------------------------------------------------- --

data SolveRandParam = RandNo
                    | RandPartition         ParamPartition
                    | RandTrueBins          ParamTrueBins
                    | RandIncrementChoice   ParamIncrementChoice
     deriving (Eq,Ord,Read,Show)
  
-- ------------------------------------------------------------------------------
-- convenience function
-- ------------------------------------------------------------------------------
getParam :: String -> Params -> String
getParam p ps = 
    case Map.lookup p ps of
       Nothing    -> error $ "SolveRandParam getParam: No such parameter: " ++ p
       Just (s,_) -> s
  
toRandParam :: Params -> SolveRandParam
toRandParam p = 
    case read (getParam "param_Randomization" p) of
        No              -> RandNo
        Partition       -> 
            RandPartition 
                (   ParamPartition
                    (read (getParam "param_max_rand_depth" p) )
                    (read (getParam "param_RandSolve_IntHalf" p) )
                    (read (getParam "param_RandSolve_IntNum" p) )
                )
        TrueBins        -> 
            RandTrueBins 
                (   ParamTrueBins
                    (read (getParam "param_max_rand_depth" p) )
                    (read (getParam "param_TrueBins_Next" p) )
                    (read (getParam "param_TrueBins_NrOfBins" p) )
                    (read (getParam "param_TrueBins_StringMode" p) )
                    (read (getParam "param_TrueBins_StringLength" p) )  
                )
        IncrementChoice -> 
            RandIncrementChoice 
                (   ParamIncrementChoice
                    ( read (getParam "param_max_rand_depth" p) )
                    ( read (getParam "param_IncrementChoice_IntRange" p) )
                    ( read (getParam "param_IncrementChoice_IntPower" p) )
                    ( read (getParam "param_IncrementChoice_MaxGeneratedStringLength" p) )
                )
            