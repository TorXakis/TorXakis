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

import           Control.Arrow ((&&&))
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import qualified Data.Text     as T

import           BehExprDefs
import           ChanId
import           Constant
import           FuncDef
import           FuncId
import           Id
import           Name
import           ProcDef
import           ProcId
import           SortId
import           StatId
import           ValExpr
import           VarEnv
import           VarId

-- | combine parameters of stautdef, state identifier, and stautdef variables into single parameter list
-- must be consistent with @combineArguments@
combineParameters :: [VarId] -> VarId -> [VarId] -> [VarId]
combineParameters params stateId vars' = params ++ (stateId:vars')

-- | combine arguments of stautdef instantiation, state identifier, and stautdef variables into single argument list
-- must be consistent with @combineParameters@
combineArguments :: [VExpr] -> VExpr -> [VExpr] -> [VExpr]
combineArguments params stateId vars' = params ++ (stateId:vars')

translate
    :: Map.Map FuncId (FuncDef VarId)
    -> Id
    -- ^ Id of the 'std' automata.
    -> Id
    -> Name
    -> [ChanId]
    -> [VarId]
    -> ExitSort
    -> [StatId]
    -> [VarId]
    -> [Trans]
    -> StatId
    -> VEnv
    -> (ProcDef, BExpr)
translate fdefs unidProc unidS name' chans params exitSort states vars' trans stateInit initialization =
    let Just initIndex = Map.lookup stateInit stateMap in
        (ProcDef chans combineParams (choice $ Set.fromList (map alternative trans)),
         procInst procId chans (combineArguments args' (cstrConst (Cint initIndex)) (map (subst defaultMap fdefs . cstrVar) vars')))
  where
        combineParams :: [VarId]
        combineParams = combineParameters params stateId vars'

        defaultMap :: VEnv
        defaultMap = Map.union initialization $ Map.fromList (map (id &&& (cstrConst . Cany . varsort) ) vars')

        stateMap :: Map.Map StatId Integer
        stateMap = Map.fromList $ zip states [0..]

        stateId :: VarId
        stateId = VarId (T.pack "$s") unidS sortIdInt

        procId :: ProcId
        procId = ProcId (T.pack "std_" <> name') unidProc (toChanSort <$> chans) (varsort <$> combineParams) exitSort

        args' :: [VExpr]
        args' = map cstrVar params

        alternative :: Trans -> BExpr
        alternative (Trans from' ao update' to') =
          let Just fromIndex = Map.lookup from' stateMap
              Just toIndex   = Map.lookup to'   stateMap
              vexprEqualStateFrom = cstrEqual (cstrVar stateId) (cstrConst (Cint fromIndex))
              ActOffer offers' hidvars cond = ao
              cond' = case ValExpr.view cond of
                        Vconst (Cbool True) -> vexprEqualStateFrom
                        _                   -> cstrITE vexprEqualStateFrom cond (cstrConst (Cbool False))
            in
              actionPref (ActOffer offers' hidvars cond')
                         (procInst procId chans (combineArguments args' (cstrConst (Cint toIndex)) (map ( subst update' fdefs . cstrVar ) vars')))

