{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module ParamCore

-- ----------------------------------------------------------------------------------------- --
-- 
-- TorXakis Core Parameters
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( Params                 -- Params = Map.Map String (String,String->Bool)
, ImpRel (..)
, InputCompletion (..)
, initParams             -- initParams :: Map.Map String (String,String->Bool)
                         -- initial values of parameters
)

where

import qualified Data.Char as Char
import qualified Data.Map  as Map


----------------------------------------------------------------------------------------- --
-- Params

type  Params  =  Map.Map String (String,String->Bool)


-- ----------------------------------------------------------------------------------------- --
-- types of parameters


-- implementation relations

data  ImpRel   =  IOCO
             -- or IocoDelta   --  ioco without quiescence; not implemented yet
             -- or IocoTick    --  ioco with clock tick
     deriving (Eq,Ord,Read,Show)


-- completion for input-enabledness in simulator

data  InputCompletion  =  ANGELIC
                     -- or DEMONIC
                     -- or ERRORSTATE
     deriving (Eq,Ord,Read,Show)


-- Represent String a positive integer within the given range?
-- lower and upper bound are inclusive.
withinRangeInt :: Int -> Int -> String -> Bool
withinRangeInt low high s = not (null s) && all Char.isDigit s && (let v = read s :: Int in ( low <= v ) && ( v <= high) )
      

-- ----------------------------------------------------------------------------------------- --
-- parameter initialization

initParams :: Params
initParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )

-- ----------------------------------------------------------------------------------------- --
-- test/simulation input generation
  [ ( "param_Test_inputEager"    , "3"         , withinRangeInt 0 3 )
            -- level of input eagerness: 0 .. 3

  , ( "param_Sim_inputEager"     , "0"         , withinRangeInt 0 3 )
            -- level of input eagerness: 0 .. 3

-- ----------------------------------------------------------------------------------------- --
-- test generation

  , ( "param_ImpRel"             , "IOCO"      , (=="IOCO")                           )
            -- implementation relation; currently only IOCO

-- ----------------------------------------------------------------------------------------- --
-- simulation

  , ( "param_InputCompletion"    , "ANGELIC"   , (=="ANGELIC")                        )
            -- input completion for simulation; currently only ANGELIC
  ]

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
