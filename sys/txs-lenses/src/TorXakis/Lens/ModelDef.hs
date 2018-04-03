{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lens.ModelDef where


import           Data.Set   (Set)
import           Lens.Micro (Lens')

import           ChanId     (ChanId)
import           TxsDefs    (ModelDef (ModelDef))


modelInChans :: Lens' ModelDef [Set ChanId]
-- forall f. Functor f => ([Set ChanId] -> f [Set ChanId]) -> ModelDef -> f ModelDef
modelInChans g (ModelDef ins outs sps bexp) = fmap (\ins' -> ModelDef ins' outs sps bexp) (g ins)

modelOutChans :: Lens' ModelDef [Set ChanId]
-- forall f. Functor f => ([Set ChanId] -> f [Set ChanId]) -> ModelDef -> f ModelDef
modelOutChans g (ModelDef ins outs sps bexp) = fmap (\outs' -> ModelDef ins outs' sps bexp) (g outs)
