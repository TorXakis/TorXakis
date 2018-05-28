{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns        #-}
module Relabel
( Relabel (..)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where
import qualified Data.Map            as Map
import qualified Data.Set            as Set

import           BehExprDefs
import           ChanId

class Relabel e
  where
    relabel :: Map.Map ChanId ChanId -> e -> e

instance Relabel BExpr
    where
        relabel v = relabel' v . BehExprDefs.view

relabel' :: Map.Map ChanId ChanId -> BExprView -> BExpr
relabel' chanmap (ActionPref (ActOffer offs hidvars cnrs) bexp)
  =  actionPref (ActOffer (Set.map (relabel chanmap) offs) hidvars cnrs) (relabel chanmap bexp)

relabel' chanmap (Guard cnrs bexp)
  =  guard cnrs (relabel chanmap bexp)

relabel' chanmap (Choice bexps)
  =  choice (Set.map (relabel chanmap) bexps)

relabel' chanmap (Parallel chids bexps)
  =  parallel (Set.map (relabel chanmap) chids) (map (relabel chanmap) bexps)

relabel' chanmap (Enable bexp1 choffs bexp2)
  =  enable (relabel chanmap bexp1) choffs (relabel chanmap bexp2)

relabel' chanmap (Disable bexp1 bexp2)
  =  disable (relabel chanmap bexp1) (relabel chanmap bexp2)

relabel' chanmap (Interrupt bexp1 bexp2)
  =  interrupt (relabel chanmap bexp1) (relabel chanmap bexp2)

relabel' chanmap (ProcInst pid chans vexps)
  =  procInst pid (map (relabel chanmap) chans) vexps

relabel' chanmap (Hide chans bexp)
  =  hide chans (relabel (Map.filterWithKey (\k _->k `notElem` chans) chanmap) bexp)

relabel' chanmap (ValueEnv venv bexp)
  =  valueEnv venv (relabel chanmap bexp)

relabel' chanmap (StAut stid venv trans)
  =  stAut stid venv (map (relabel chanmap) trans)


instance Relabel Offer
  where
    relabel chanmap (Offer chid choffs)
      =  Offer (relabel chanmap chid) choffs


instance Relabel ChanId
  where
    relabel chanmap chid
      =  Map.findWithDefault chid chid chanmap


instance Relabel Trans
  where
    relabel chanmap (Trans from' (ActOffer offs hidvars cnrs) venv to')
      =  Trans from' (ActOffer (Set.map (relabel chanmap) offs) hidvars cnrs) venv to'

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

