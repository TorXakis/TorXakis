{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module PShow

-- ----------------------------------------------------------------------------------------- --
--
-- Pretty and Formatted Show for TorXakisDefs 
--
-- ----------------------------------------------------------------------------------------- --

where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.String.Utils as Utils

import TxsDDefs
import TxsDefs
import TxsShow


-- ----------------------------------------------------------------------------------------- --

instance PShow Action
  where
    pshow (Act    set)  =  "Act    { "++(pshow set)++" }\n"
    pshow  ActQui       =  "No Output (Quiescence)\n"

-- ----------------------------------------------------------------------------------------- --

instance PShow SAction
  where
    pshow (SAct h s)  =  "SAct  "++" ! "++(show s)++"\n"
    pshow  SActQui    =  "No Output (Sut is Quiescent)\n"


-- ----------------------------------------------------------------------------------------- --


instance PShow ConnHandle
  where
    pshow (ConnHtoW chan h vars vexp)
      =  (pshow chan) ++ (show h) ++ "\n" ++ (pshow vars) ++ (pshow vexp) ++ "\n"
    pshow (ConnHfroW chan h var vexps)
      =  (pshow chan) ++ (show h) ++ "\n" ++ (pshow var) ++ (pshow vexps) ++ "\n"


-- ----------------------------------------------------------------------------------------- --
instance PShow Verdict
  where
     pshow  Pass       =  "PASS"
     pshow (Fail act)  =  "FAIL:  " ++ ( fshow act )
     pshow  NoVerdict  =  "No Verdict"


instance PShow ()
  where
    pshow _  =  ""


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

