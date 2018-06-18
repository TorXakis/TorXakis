{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module Solve.Params

-- ----------------------------------------------------------------------------------------- --
-- 
-- TorXakis Parameters
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( Params
, initParams       --  :: Map.Map String (String,String->Bool)
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

data  SolveStrategy     =  No | Partition | TrueBins | IncrementChoice | IncrementBins
     deriving (Eq,Ord,Read,Show)

data  StringMode        =  Regex | Length
     deriving (Eq,Ord,Read,Show)

data  Next             =  Linear | Power | Exponent
     deriving (Eq,Ord,Read,Show)

     
type Params = Map.Map String (String,String->Bool)

-- ----------------------------------------------------------------------------------------- --
-- functions

-- Represent String a positive integer?
positiveInt :: String -> Bool
positiveInt s = not (null s) && all Char.isDigit s
----------------------------------------------------------------------------------------- --
-- initParams

initParams :: Params
initParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )
  [ ( "param_max_rand_depth"                               , "4"          , positiveInt )
  , ( "param_Randomization"                                , show IncrementChoice
                                                                          , \s ->    (s == show No)
                                                                                  || (s == show Partition)
                                                                                  || (s == show TrueBins)
                                                                                  || (s == show IncrementChoice) 
                                                                                  || (s == show IncrementBins) )
  , ( "param_TrueBins_StringLength"                        , "6"          , positiveInt )
  , ( "param_TrueBins_StringMode"                          , show Regex   , \s ->    (s== show Regex)
                                                                                  || (s== show Length) )
  , ( "param_TrueBins_Next"                                , show Linear  , \s ->    (s== show Linear)
                                                                                  || (s== show Power)
                                                                                  || (s== show Exponent) )
  , ( "param_TrueBins_NrOfBins"                            , "10"         , positiveInt )
  , ( "param_IncrementChoice_MaxGeneratedStringLength"     , "10"         , positiveInt )
  , ( "param_IncrementChoice_IntRange"                     , "65536"      , positiveInt )
            -- default 2^16
  , ( "param_IncrementChoice_IntPower"                     , "4"          , positiveInt )
  , ( "param_RandSolve_IntHalf"                            , "1000"       , positiveInt ) 
            -- half-value for binomial distribution
  , ( "param_RandSolve_IntNum"                             , "5"          , positiveInt )
            -- number of positive (negative) partitions
  ]

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
