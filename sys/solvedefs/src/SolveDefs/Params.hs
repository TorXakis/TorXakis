{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module SolveDefs.Params

-- ----------------------------------------------------------------------------------------- --
-- 
-- TorXakis Parameters
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( initParams       --  :: Map.Map String (String,String->Bool)
                   --  initial values of solve parameters
, SolveStrategy (..)
, StringMode(..)
, Next(..)
)

where

import qualified Data.Char as Char
import qualified Data.Map  as Map


-- ----------------------------------------------------------------------------------------- --
-- types of parameters

-- smt solve strategies

data  SolveStrategy     =  No | Partition | TrueBins | IncrementChoice
     deriving (Eq,Ord,Read,Show)

data  StringMode        =  Regex | Length
     deriving (Eq,Ord,Read,Show)

data  Next             =  Linear | Power | Exponent
     deriving (Eq,Ord,Read,Show)

----------------------------------------------------------------------------------------- --
-- initParams


initParams :: Map.Map String (String,String->Bool)
initParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )

-- ----------------------------------------------------------------------------------------- --
-- randomization
  
  [ ( "param_max_rand_depth"     , "4"         , \s ->   ( all Char.isDigit s )
                                                      && ( 0 < read s )             )
            -- param_max_rand_depth :: Int (>0)
  
  , ( "param_Randomization"      , show TrueBins  , \s ->    (s== show No)
                                                          || (s== show Partition)
                                                          || (s== show TrueBins)
                                                          || (s== show IncrementChoice)
                                                         )
            -- param_Randomization :: SolveStrategy
            -- randomization strategy for constraint solving
            -- No        :
            -- Partition : interval partitioning of types
            -- TrueBins  : interval partitioning of types using True Bins
            -- IncrementChoice: incremental randomization using single choices and splitting of value space
            
  , ( "param_TrueBins_StringLength"     , "6"         , \s ->   ( all Char.isDigit s )
                                                             && ( 0 < read s )                 )
            -- param_TrueBins_StringLength :: Int (>0)

  , ( "param_TrueBins_StringMode"     , show Regex         , \s ->    (s== show Regex)
                                                                   || (s== show Length)                     )
            -- param_TrueBins_StringMode :: StringMode

  , ( "param_TrueBins_Next"     , show Linear         , \s ->   (s== show Linear)
                                                             || (s== show Power)
                                                             || (s== show Exponent)                    )
            -- param_TrueBins_Next :: Next
            
  , ( "param_TrueBins_NrOfBins"     , "10"         , \s ->   ( all Char.isDigit s )
                                                          && ( 0 < read s )                      )
            -- param_TrueBins_NrOfBins :: Int (>0)

  , ( "param_IncrementChoice_MaxGeneratedStringLength"     , "10"         , \s ->   ( all Char.isDigit s )
                                                                                 && ( 0 < read s )                      )
            -- param_IncrementChoice_MaxGeneratedStringLength :: Int (>0)

  , ( "param_IncrementChoice_IntRange"     , "65536"         , \s ->   ( all Char.isDigit s )
                                                                    && ( 0 < read s )                      )
            -- param_IncrementChoice_IntRange :: Int (>0) 
            -- default 2^16

  , ( "param_IncrementChoice_IntPower"     , "4"         , \s ->   ( all Char.isDigit s )
                                                                && ( 0 < read s )                      )
            -- param_IncrementChoice_IntPower :: Int (>0)

  , ( "param_RandSolve_IntHalf"  , "1000"      , all Char.isDigit                ) 
            -- param_RandSolve_IntHalf :: Int (>0)
            -- half-value for binomial distribution

  , ( "param_RandSolve_IntNum"   , "5"         , all Char.isDigit                )
            -- param_RandSolve_IntNum :: Int (>0)
            -- number of positive (negative) partitions

  , ( "param_RandSolve_adtWidth" , "5"         , all Char.isDigit               )
            -- param_RandSolve_adtWidth :: Int (>0)
            -- number of positive (negative) partitions
  
-- ----------------------------------------------------------------------------------------- --
-- sut observations
  
  , ( "param_Sut_deltaTime"      , "2000"      , all Char.isDigit               )
            -- param_Sut_deltaTime :: Int (>0)
            -- quiescence output time (millisec >0)

  , ( "param_Sut_ioTime"         , "10"        , all Char.isDigit               )
            -- param_Sut_ioTime :: Int (>0)
            --  timeout for input when trying output (msec, >0)
  
-- ----------------------------------------------------------------------------------------- --
-- test/simulation input generation
  
  , ( "param_Test_inputEager"    , "3"         , \s ->   ( all Char.isDigit s )
                                                      && ( 0 <= read s ) && ( read s <= 3 )   )
            -- param_Test_inputEager :: Int (0 <= param_Test_inputEager <= 3)
            -- level of input eagerness: 0 .. 3

  , ( "param_Sim_inputEager"     , "0"         , \s ->   ( all Char.isDigit s )
                                                      && ( 0 <= read s ) && ( read s <= 3 )   )
            -- param_Sim_inputEager :: Int (0 <= param_Test_inputEager <= 3)
            -- level of input eagerness: 0 .. 3
  
-- ----------------------------------------------------------------------------------------- --
-- test generation

  , ( "param_ImpRel"             , "IOCO"      , \s ->  (s=="IOCO")                           ) 
            -- param_ImpRel :: ImpRel
            -- implementation relation; currently only IOCO

-- ----------------------------------------------------------------------------------------- --
-- simulation

  , ( "param_InputCompletion"    , "ANGELIC"   , \s ->  (s=="ANGELIC")                        ) 
            -- param_InputCompletion :: InputCompletion
            -- input completion for simulation; currently only ANGELIC

  , ( "param_Sim_deltaTime"      , "200"       , all Char.isDigit               )
            -- param_Sim_deltaTime :: Int (>0)
            -- quiescence input time (millisec >0)

  ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

