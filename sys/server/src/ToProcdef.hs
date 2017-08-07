{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

module ToProcdef

-- ----------------------------------------------------------------------------------------- --
-- export

(
toProcdef   -- :: [TxsDDefs.Action] -> String
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad.State

import qualified Data.String.Utils as Utils
import qualified Data.List         as List
import qualified Data.Map          as Map
import qualified Data.Set          as Set

-- import from defs
import qualified TxsDefs    as TxsDefs
import qualified ChanId     as ChanId
import qualified SortId     as SortId
import qualified TxsDDefs   as TxsDDefs
import qualified TxsShow    as TxsShow

-- ----------------------------------------------------------------------------------------- --

toProcdef :: [TxsDDefs.Action] -> String
toProcdef actions
  =  let chids       = Set.toList $ Set.unions
                             [ Set.map fst acts | TxsDDefs.Act acts <- actions ]
         channeldefs = chanIdsToProcdef chids
         actions'    = actionsToProcdef actions
      in "PROCDEF trace ["++ channeldefs ++ "]() EXIT\n" ++
         " ::=\n" ++ actions' ++ "\n    >-> EXIT\nENDDEF\n"

chanIdsToProcdef :: [TxsDefs.ChanId] -> String
chanIdsToProcdef chids
  =  let chans = map chanIdToProcdef chids
      in Utils.join "; " chans

chanIdToProcdef :: TxsDefs.ChanId -> String
chanIdToProcdef chid
  =  let sorts = sortsToProcdef (ChanId.chansorts chid)
      in (ChanId.name chid) ++ " :: " ++ sorts

sortsToProcdef :: [TxsDefs.SortId] -> String
sortsToProcdef sortids
  =  let sids = map SortId.name sortids
      in Utils.join " # " sids

actionsToProcdef :: [TxsDDefs.Action] -> String
actionsToProcdef actions
  =  let actions' = map actionToProcdef actions
      in Utils.join "\n    >-> " actions'

actionToProcdef :: TxsDDefs.Action -> String
actionToProcdef (TxsDDefs.Act s)  =  communicationsToProcdef s
actionToProcdef TxsDDefs.ActQui   =  "STOP"        -- can't be replayed

communicationsToProcdef :: Set.Set (TxsDefs.ChanId, [TxsDefs.Const]) -> String
communicationsToProcdef set
  =  let communications = map communicationToProcdef (Set.toList set)
      in Utils.join " | " communications
    
communicationToProcdef :: (TxsDefs.ChanId, [TxsDefs.Const]) -> String
communicationToProcdef (chid, vexprs)
  =  let values = map TxsShow.pshow vexprs
      in ((ChanId.name chid) ++ " ! ") ++ (Utils.join " ! " values)


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
