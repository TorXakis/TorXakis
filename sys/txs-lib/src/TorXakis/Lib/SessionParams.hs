{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lib.SessionParams where

import qualified Data.Char as Char
import qualified Data.Map  as Map

import           ParamCore (Params)

initSessionParams :: Params
initSessionParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )

-- ----------------------------------------------------------------------------------------- --
-- sut observations

  [ ( "param_Sut_deltaTime"      , "2000"      , \s -> not (null s) && all Char.isDigit s)
            -- param_Sut_deltaTime :: Int (>0)
            -- quiescence output time (millisec >0)

  , ( "param_Sut_ioTime"         , "10"        , \s -> not (null s) && all Char.isDigit s)
            -- param_Sut_ioTime :: Int (>0)
            --  timeout for input when trying output to Sut (msec, >0)

  , ( "param_EW_connDelay"       , "1000"      , all Char.isDigit               )
            -- param_EW_connDelay :: Int (>0)
            -- delay between starting EW and connecting sockets (msec, >0)

-- ----------------------------------------------------------------------------------------- --
-- simulation

  , ( "param_Sim_deltaTime"      , "2000"       , \s -> not (null s) && all Char.isDigit s)
            -- param_Sim_deltaTime :: Int (>0)
            -- quiescence input time (millisec >0)

  , ( "param_Sim_ioTime"         , "10"        , \s -> not (null s) && all Char.isDigit s)
            -- param_Sim_ioTime :: Int (>0)
            --  timeout for output when trying input to Sim (msec, >0)

  ]
