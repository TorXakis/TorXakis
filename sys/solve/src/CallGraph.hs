{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module CallGraph

-- ----------------------------------------------------------------------------------------- --
-- 
-- module CallGraph :  .....
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( edgesValExpr   --  :: (Variable v) => Map.Map FuncId Vertex -> (ValExpr v) -> [Vertex]
)


-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map  as Map

import Data.Graph
import Data.Maybe

import TxsDefs


-- ----------------------------------------------------------------------------------------- --
--

toVertex :: Map.Map FuncId Vertex -> FuncId -> [Vertex]
toVertex mapFidVertex fid = let result = Map.lookup fid mapFidVertex in
                                [fromJust result | isJust result]

-- ----------------------------------------------------------------------------------------- --

edgesValExpr :: (Variable v) => Map.Map FuncId Vertex -> ValExpr v -> [Vertex]

edgesValExpr mapFidVertex (Vfunc fid vexps)
  =  toVertex mapFidVertex fid ++ concatMap (edgesValExpr mapFidVertex) vexps

edgesValExpr mapFidVertex (Vpredef _ fid vexps)
  =  toVertex mapFidVertex fid ++ concatMap (edgesValExpr mapFidVertex) vexps

edgesValExpr mapFidVertex (Vite vexps vexp1 vexp2)
  =  concatMap (edgesValExpr mapFidVertex) vexps
     ++ edgesValExpr mapFidVertex vexp1
     ++ edgesValExpr mapFidVertex vexp2

edgesValExpr mapFidVertex (Vcstr _ vexps)
  =  concatMap (edgesValExpr mapFidVertex) vexps

edgesValExpr mapFidVertex (Venv venv vexpr')
  =  edgesValExpr mapFidVertex vexpr'
     ++ concatMap ( edgesValExpr mapFidVertex . snd )  (Map.toList venv) 

edgesValExpr mapFidVertex (Vequal vexp1 vexp2)
  =  edgesValExpr mapFidVertex vexp1 ++ edgesValExpr mapFidVertex vexp2

edgesValExpr _ _
  =  []


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

