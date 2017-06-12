{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
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
             -- | IocoDelta   --  ioco without quiescence; not implemented yet
             -- | IocoTick    --  ioco with clock tick
     deriving (Eq,Ord,Read,Show)


-- completion for input-enabledness in simulator

data  InputCompletion  =  ANGELIC
                     -- | DEMONIC
                     -- | ERRORSTATE
     deriving (Eq,Ord,Read,Show)


-- ----------------------------------------------------------------------------------------- --
-- parameter initialization

initParams :: Params
initParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )

-- ----------------------------------------------------------------------------------------- --
-- test/simulation input generation
  [ ( "param_Test_inputEager"    , "3"         , \s ->   ( and $ map Char.isDigit s )
                                                      && ( 0 <= read s ) && ( read s <= 3 )   )
            -- param_Test_inputEager :: Int (0 <= param_Test_inputEager <= 3)
            -- level of input eagerness: 0 .. 3

  , ( "param_Sim_inputEager"     , "0"         , \s ->   ( and $ map Char.isDigit s )
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
  ]


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

