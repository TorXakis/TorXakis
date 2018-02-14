{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TreeVarShow

-- ----------------------------------------------------------------------------------------- --
--
-- Pretty and Formatted Show for TorXakisDefs
--
-- ----------------------------------------------------------------------------------------- --

where


import           TreeVars
import           TxsShow

instance PShow CTOffer
  where
    pshow (CToffer btchan btchoffs)
      =  pshow btchan ++ pshow btchoffs

instance PShow IVar where
  pshow (IVar nm uid pos stat _srt) =
    "$"++ show nm ++"$"++ show uid ++"$"++ show stat ++"$"++ show pos ++"$  "
