{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- ----------------------------------------------------------------------------------------- --

module STShow

-- ----------------------------------------------------------------------------------------- --
--
-- Pretty and Formatted Show for TorXakisDefs
--
-- ----------------------------------------------------------------------------------------- --

where

import qualified Data.Map          as Map
import qualified Data.String.Utils as Utils

import           STree
import           TreeVarShow()
import           TxsDefs
import           TxsShow


-- ----------------------------------------------------------------------------------------- --
-- PShow SNode

instance PShow SNode
  where
    pshow (SNbexpr ivals bexp)
      =  "VALENV "
         ++ Utils.join ";\n" (map (\(k,v) -> pshow k ++ " = " ++ pshow v) (Map.assocs ivals))
         ++ " IN " ++ pshow bexp ++ " NI"
    pshow (SNguard cond bnode)
      =  "[[ " ++ pshow cond ++ " ]] =>> " ++ pshow bnode
    pshow (SNchoice nodes)
      =  Utils.join "\n" (pshow <$> nodes)
    pshow (SNparallel chans snodes)
      =  case snodes of
           [] -> "STOP\n"
           be -> "( "
                 ++ Utils.join (" )\n"++"|[ "++ Utils.join ", " (map pshow chans) ++" ]|\n( ")
                               (map pshow be)
                 ++ " )"
    pshow (SNenable snode1 choffs snode2)
      =  "( " ++ pshow snode1 ++ " )\n" ++ ">>>"
         ++ case choffs of
              [] -> "\n"
              _  -> " ACCEPT\n" ++ Utils.join ",\n" (map pshow choffs) ++ "\n" ++ "IN\n"
         ++ "( " ++ pshow snode2 ++ " )"
    pshow (SNdisable snode1 snode2)
      =  "( " ++ pshow snode1 ++ " )\n"
         ++ "[>>\n" ++ "( " ++ pshow snode2 ++ " )"
    pshow (SNinterrupt snode1 snode2)
      =  "( " ++ pshow snode1 ++ " )\n" ++ "[><\n"
         ++ "( " ++ pshow snode2 ++ " )"
    pshow (SNhide chans snode)
      =  "HIDE "
         ++ Utils.join "; " [ show n ++ " :: " ++ Utils.join " # " (map pshow srts)
                            | ChanId n _uid srts <- chans
                            ]
         ++ " IN\n"
         ++ pshow snode ++ "\n"
         ++ "NI\n"

-- ----------------------------------------------------------------------------------------- --
-- PShow BTree

instance PShow STtrans
  where
    pshow (STtrans btoffs _hidvars spreds snext)
      =  " ## " ++ pshow btoffs ++ pshow spreds ++ pshow (stnode snext) ++ "\n"


instance PShow STree
  where pshow = pshow . stnode

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
