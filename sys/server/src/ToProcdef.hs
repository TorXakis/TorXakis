{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module ToProcdef

-- ----------------------------------------------------------------------------------------- --
-- export

( toProcdef   -- :: [TxsDDefs.Action] -> String
, toPurpdef   -- :: [TxsDDefs.Action] -> String
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Set          as Set
import           Data.Text         (Text)
import qualified Data.Text         as T

-- import from defs
import qualified ChanId
import qualified TxsDDefs
import qualified TxsDefs
import qualified TxsShow

-- import from valexpr
import Constant
import SortId

-- ----------------------------------------------------------------------------------------- --

toProcdef :: [TxsDDefs.Action] -> Text
toProcdef actions
  =  let chids       = Set.toList $ Set.unions
                             [ Set.map fst acts | TxsDDefs.Act acts <- actions ]
         channeldefs = chanIdsToProcdef chids
         actions'    = actionsToProcdef actions
      in "PROCDEF trace ["<> channeldefs <> "]() EXIT\n" <>
         " ::=\n" <> actions' <> "\n    >-> EXIT\nENDDEF\n"

chanIdsToProcdef :: [TxsDefs.ChanId] -> Text
chanIdsToProcdef chids
  =  let chans = map chanIdToProcdef chids
      in T.intercalate "; " chans

chanIdToProcdef :: TxsDefs.ChanId -> Text
chanIdToProcdef chid
  =  let sorts = sortsToProcdef (ChanId.chansorts chid)
      in ChanId.name chid <> " :: " <> sorts

sortsToProcdef :: [SortId] -> Text
sortsToProcdef sortids
  =  let sids = map SortId.name sortids
      in T.intercalate " # " sids

actionsToProcdef :: [TxsDDefs.Action] -> Text
actionsToProcdef actions
  =  let actions' = map actionToProcdef actions
      in T.intercalate "\n    >-> " actions'

actionToProcdef :: TxsDDefs.Action -> Text
actionToProcdef (TxsDDefs.Act s) =  communicationsToProcdef s
actionToProcdef TxsDDefs.ActQui  =  "STOP"        -- can't be replayed

communicationsToProcdef :: Set.Set (TxsDefs.ChanId, [Constant]) -> Text
communicationsToProcdef set
  =  let communications = map communicationToProcdef (Set.toList set)
      in T.intercalate " | " communications

communicationToProcdef :: (TxsDefs.ChanId, [Constant]) -> Text
communicationToProcdef (chid, vexprs)
  =  ChanId.name chid <> foldMap ( (" ! " <>) . T.pack . TxsShow.pshow ) vexprs

-- ----------------------------------------------------------------------------------------- --

toPurpdef :: [TxsDDefs.Action] -> Text
toPurpdef actions
  =  let chids       = Set.toList $ Set.unions
                             [ Set.map fst acts | TxsDDefs.Act acts <- actions ]
         channeldefs = chanIdsToProcdef chids
         actions'    = actionsToProcdef actions
      in "PROCDEF replayProc ["<> channeldefs <> "]() HIT\n" <>
         " ::=\n" <> actions' <> "\n    >-> HIT\nENDDEF\n"

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
