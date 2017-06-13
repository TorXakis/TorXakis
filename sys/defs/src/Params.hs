{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Params

-- ----------------------------------------------------------------------------------------- --
-- 
-- TorXakis Parameters
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( initParams       --  :: Map.Map String (String,String->Bool)
                   --  initial values of parameters
, ImpRel (..)
, InputCompletion (..)
)

where

import qualified Data.Char as Char
import qualified Data.Map  as Map


-- ----------------------------------------------------------------------------------------- --
-- types of parameters


-- implementation relations

data  ImpRel   =  IOCO
             --  IocoDelta   --  ioco without quiescence; not implemented yet
             --  IocoTick    --  ioco with clock tick
     deriving (Eq,Ord,Read,Show)


-- completion for input-enabledness in simulator

data  InputCompletion  =  ANGELIC
                     --  DEMONIC
                     --  ERRORSTATE
     deriving (Eq,Ord,Read,Show)



----------------------------------------------------------------------------------------- --
-- initParams


initParams :: Map.Map String (String,String->Bool)
initParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )

-- ----------------------------------------------------------------------------------------- --
-- sut observations
  
  [ ( "param_Sut_deltaTime"      , "2000"      , all Char.isDigit               )
            -- param_Sut_deltaTime :: Int (>0)
            -- quiescence output time (millisec >0)

  , ( "param_Sut_ioTime"         , "10"        , all Char.isDigit               )
            -- param_Sut_ioTime :: Int (>0)
            --  timeout for input when trying output (msec, >0)
  
-- ----------------------------------------------------------------------------------------- --
-- test/simulation input generation
  
  , ( "param_Test_inputEager"    , "3"         , \s ->   all Char.isDigit s
                                                      && 0 <= (read s::Integer) && (read s::Integer) <= 3 )
            -- param_Test_inputEager :: Int (0 <= param_Test_inputEager <= 3)
            -- level of input eagerness: 0 .. 3

  , ( "param_Sim_inputEager"     , "0"         , \s ->   all Char.isDigit s 
                                                      && 0 <= (read s::Integer) && (read s::Integer) <= 3 )
            -- param_Sim_inputEager :: Int (0 <= param_Test_inputEager <= 3)
            -- level of input eagerness: 0 .. 3
  
-- ----------------------------------------------------------------------------------------- --
-- test generation

  , ( "param_ImpRel"             , "IOCO"      , ("IOCO"==)                           ) 
            -- param_ImpRel :: ImpRel
            -- implementation relation; currently only IOCO

-- ----------------------------------------------------------------------------------------- --
-- simulation

  , ( "param_InputCompletion"    , "ANGELIC"   , ("ANGELIC"==)                        ) 
            -- param_InputCompletion :: InputCompletion
            -- input completion for simulation; currently only ANGELIC

  , ( "param_Sim_deltaTime"      , "200"       , all Char.isDigit               )
            -- param_Sim_deltaTime :: Int (>0)
            -- quiescence input time (millisec >0)

  ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

