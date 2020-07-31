{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Lib.ToProcdef
( toProcdef
, toPurpdef
)
where

import           Data.Semigroup ((<>))
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as T

-- import from defs
import           ChanId         (ChanId, chansorts, name)
import           TxsDDefs       (Action (Act, ActQui))
import           TxsShow        (pshow)

-- import from valexpr
import           Constant       (Constant)
import           SortId         (SortId, name)

-- ----------------------------------------------------------------------------------------- --

toProcdef :: [Action] -> Text
toProcdef actions
  = let (channeldefs, actions') = writeChanDefsAndActions actions
    in  "PROCDEF trace [" <> channeldefs <> "]() EXIT\n" <>
        " ::=\n" <> actions' <> "\n    >-> EXIT\nENDDEF\n"

toPurpdef :: [Action] -> Text
toPurpdef actions =
    let (channeldefs, actions') = writeChanDefsAndActions actions
    in  "PROCDEF replayProc [" <> channeldefs <> "]() HIT\n" <>
        " ::=\n" <> actions' <> "\n    >-> HIT\nENDDEF\n"

writeChanDefsAndActions :: [Action] -> (Text, Text)
writeChanDefsAndActions as =
    let chids = Set.toList $ Set.unions
                [ Set.map fst acts | Act acts <- as ]
    in  (chanIdsToProcdef chids, actionsToProcdef as)

chanIdsToProcdef :: [ChanId] -> Text
chanIdsToProcdef chids =
    let chans = map chanIdToProcdef chids
    in  T.intercalate "; " chans

chanIdToProcdef :: ChanId -> Text
chanIdToProcdef chid =
    let sorts = sortsToProcdef (chansorts chid)
    in  ChanId.name chid <> " :: " <> sorts

sortsToProcdef :: [SortId] -> Text
sortsToProcdef sortids =
    let sids = map SortId.name sortids
    in  T.intercalate " # " sids

actionsToProcdef :: [Action] -> Text
actionsToProcdef actions =
    let actions' = map actionToProcdef actions
    in  T.intercalate "\n    >-> " actions'

actionToProcdef :: Action -> Text
actionToProcdef (Act s) =  communicationsToProcdef s
actionToProcdef ActQui  =  "STOP"        -- can't be replayed

communicationsToProcdef :: Set.Set (ChanId, [Constant]) -> Text
communicationsToProcdef set =
    let communications = map communicationToProcdef (Set.toList set)
    in  T.intercalate " | " communications

communicationToProcdef :: (ChanId, [Constant]) -> Text
communicationToProcdef (chid, vexprs) =
    ChanId.name chid <> foldMap ( (" ! " <>) . T.pack . pshow ) vexprs
