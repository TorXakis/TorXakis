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
    [ ( "param_Sut_deltaTime"      , "2000"      , \s -> not (null s) && all Char.isDigit s)
            -- param_Sut_deltaTime :: Int (>0)
            -- quiescence output time (millisec >0)
    , ( "param_Sim_deltaTime"      , "2000"       , \s -> not (null s) && all Char.isDigit s)
            -- param_Sim_deltaTime :: Int (>0)
            -- quiescence input time (millisec >0)
    ]
-- We don't need ioTime's because we use TChan instead of Chan
