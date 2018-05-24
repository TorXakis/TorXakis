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
, getParamPairs
, paramToPair
, updateParam
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


-- ----------------------------------------------------------------------------------------- --
-- parameter initialization

initParams :: Params
initParams  =  Map.fromList $ map ( \(x,y,z) -> (x,(y,z)) )

-- ----------------------------------------------------------------------------------------- --
-- test/simulation input generation
  [ ( "param_Test_inputEager"    , "3"         , \s -> not (null s) && all Char.isDigit s
                                                       && ( 0 <= (read s::Integer) ) && ( (read s::Integer) <= 3 )   )
            -- param_Test_inputEager :: Int (0 <= param_Test_inputEager <= 3)
            -- level of input eagerness: 0 .. 3

  , ( "param_Sim_inputEager"     , "0"         , \s -> not (null s) && all Char.isDigit s
                                                       && ( 0 <= (read s::Integer) ) && ( (read s::Integer) <= 3 )   )
            -- param_Sim_inputEager :: Int (0 <= param_Test_inputEager <= 3)
            -- level of input eagerness: 0 .. 3

-- ----------------------------------------------------------------------------------------- --
-- test generation

  , ( "param_ImpRel"             , "IOCO"      , (=="IOCO")                           )
            -- param_ImpRel :: ImpRel
            -- implementation relation; currently only IOCO

-- ----------------------------------------------------------------------------------------- --
-- simulation

  , ( "param_InputCompletion"    , "ANGELIC"   , (=="ANGELIC")                        )
            -- param_InputCompletion :: InputCompletion
            -- input completion for simulation; currently only ANGELIC
  ]

getParamPairs :: [String] -> Params -> [(String,String)]
getParamPairs pNms ps =
    case pNms of
      [] -> paramsToPairs ps
      _  -> concat $ mapM (paramToPair ps) pNms

paramsToPairs :: Params -> [(String,String)]
paramsToPairs = map (\(nm,(val,_))->(nm,val)) . Map.toList

paramToPair :: Params -> String -> [(String,String)]
paramToPair ps pNm =
    case Map.lookup pNm ps of
        Nothing           -> []
        Just (val,_check) -> [(pNm,val)]

updateParam :: Params -> (String,String) -> Params
updateParam ps (pNm,pVl) =  case Map.lookup pNm ps of
    Nothing        -> ps
    Just (_,check) -> if check pVl
                            then Map.insert pNm (pVl,check) ps
                            else ps
