{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --
module ToProcdef
-- ----------------------------------------------------------------------------------------- --
where

import Control.Monad.State
import qualified Data.String.Utils as Utils
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import TxsDefs
import TxsDDefs
import TxsEnv
import TxsShow

import ChanId
import SortId

toProcdef :: [Action] -> TxsDef -> IOE (String) 
toProcdef actions (DefModel (ModelDef inChanSets outChanSets _ _)) = do
    channeldefs <- chanIdsToProcdef ( (Set.toList (Set.unions inChanSets)) ++ (Set.toList (Set.unions outChanSets)) ) 
    actions     <- actionsToProcdef actions
    return $ "PROCDEF trace ["++ channeldefs ++ "]() EXIT ::=\n        " ++ actions ++ "\n    >-> EXIT\nENDDEF\n"

chanIdsToProcdef :: [ChanId] -> IOE (String)
chanIdsToProcdef chids = do
    chans <- mapM chanIdToProcdef chids
    return $ Utils.join "; " chans

chanIdToProcdef :: ChanId -> IOE (String)
chanIdToProcdef chid = do
    sorts <- sortsToProcdef (chansorts chid)
    return $ (ChanId.name chid) ++ " :: " ++ sorts

sortsToProcdef :: [SortId] -> IOE (String)
sortsToProcdef sortids = do
    let sids = map SortId.name sortids
    return $ Utils.join " # " sids

actionsToProcdef :: [Action] -> IOE (String)
actionsToProcdef actions = do
    actions <- mapM actionToProcdef actions
    return $ Utils.join "\n    >-> " actions

actionToProcdef :: Action -> IOE (String)
actionToProcdef ( Act s )    = communicationsToProcdef s
actionToProcdef ActQui       = return "STOP"        -- can't be replayed

communicationsToProcdef :: Set.Set (ChanId, [Const]) -> IOE (String)
communicationsToProcdef set = do
    communications <- mapM communicationToProcdef (Set.toList set)
    return $ Utils.join " | " communications
    
communicationToProcdef :: (ChanId, [Const]) -> IOE (String)
communicationToProcdef (chid, vexprs) = do
    let values = map pshow vexprs
    return $ ((ChanId.name chid) ++ " ! ") ++ (Utils.join " ! " values)

-- ----------------------------------------------------------------------------------------- --

