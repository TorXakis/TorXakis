{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module StautDef
( translate
, combineParameters   -- TODO for comparison only (should not be exposed in final release)
)

where

import qualified Data.Map           as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T
import           Data.Monoid

import           ConstDefs
import           FuncDef
import           FuncId
import           Id
import           Name
import           SortId
import           TxsDefs
import           ValExpr
import           VarId

-- | combine parameters of stautdef, state identifier, and stautdef variables into single parameter list
-- must be consistent with @combineArguments@
combineParameters :: [VarId] -> VarId -> [VarId] -> [VarId] 
combineParameters params stateId vars' = params ++ (stateId:vars')

-- | combine arguments of stautdef instantiation, state identifier, and stautdef variables into single argument list
-- must be consistent with @combineParameters@
combineArguments :: [VExpr] -> VExpr -> [VExpr] -> [VExpr] 
combineArguments params stateId vars' = params ++ (stateId:vars')

translate :: Map.Map FuncId (FuncDef VarId) -> Id -> Id -> Name -> [ChanId] -> [VarId] -> ExitSort -> [StatId] -> [VarId] -> [Trans] -> StatId -> VEnv -> (ProcDef, BExpr)
translate fdefs unidProc unidS name' chans params exitSort states vars' trans stateInit initialization =
    let Just initIndex = Map.lookup stateInit stateMap in
        (ProcDef chans (combineParameters params stateId vars') (choice $ map alternative trans),
         procInst procId chans (combineArguments args' (cstrConst (Cint initIndex)) (map (subst defaultMap fdefs . cstrVar) vars')))
  where
        defaultMap :: VEnv
        defaultMap = Map.union initialization $ Map.fromList (map (\x -> (x, cstrConst (Cany (varsort x)))) vars')
        
        stateMap :: Map.Map StatId Integer
        stateMap = Map.fromList $ zip states [0..]
        
        stateId :: VarId
        stateId = VarId (T.pack "$s") unidS sortIdInt
        
        procId :: ProcId
        procId = ProcId (T.pack "std_" <> name') unidProc chans (combineParameters params stateId vars') exitSort
        
        args' :: [VExpr]
        args' = map cstrVar params
        
        alternative :: Trans -> BExpr
        alternative (Trans from' (ActOffer offers' _hidvars cond) update' to') =
            let Just fromIndex = Map.lookup from' stateMap
                Just toIndex   = Map.lookup to'   stateMap in
            actionPref (ActOffer offers' Set.empty (cstrITE (cstrEqual (cstrVar stateId) (cstrConst (Cint fromIndex))) cond (cstrConst (Cbool False))))
                       (procInst procId chans (combineArguments args' (cstrConst (Cint toIndex)) (map ( subst update' fdefs . cstrVar ) vars')))
