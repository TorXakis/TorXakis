{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- ----------------------------------------------------------------------------------------- --

module BTShow

-- ----------------------------------------------------------------------------------------- --
--
-- Pretty and Formatted Show for TorXakisDefs
--
-- ----------------------------------------------------------------------------------------- --

where

import qualified Data.Map          as Map
import qualified Data.String.Utils as Utils

import           BTree
import           TxsDefs
import           TxsShow

-- ----------------------------------------------------------------------------------------- --
-- PShow BNode

instance PShow IVar where
  pshow (IVar nm uid pos stat _srt) =
    "$"++ show nm ++"$"++ show uid ++"$"++ show stat ++"$"++ show pos ++"$  "

instance PShow CNode where
    pshow (BNbexpr we bexp)
      =  "VALENV "
         ++ Utils.join ";\n" (map (\(k,v) -> pshow k ++ " = " ++ pshow v) (Map.assocs we))
         ++ " IN " ++ pshow bexp ++ " NI"
    pshow (BNparallel chans bnodes)
      =  case bnodes of
           [] -> "STOP\n"
           be -> "( " ++
                 Utils.join (" )\n"++"|[ "++ Utils.join ", " (map pshow chans) ++" ]|\n( ")
                            (map pshow be)
                 ++ " )"
    pshow (BNenable bnode1 choffs bnode2)
      =  "( " ++ pshow bnode1 ++ " )\n" ++ ">>>"
         ++ case choffs of
              [] -> "\n"
              _  -> " ACCEPT\n" ++ Utils.join ",\n" (map pshow choffs) ++ "\n" ++ "IN\n"
         ++ "( " ++ pshow bnode2 ++ " )"
    pshow (BNdisable bnode1 bnode2)
      =  "( " ++ pshow bnode1 ++ " )\n"
         ++ "[>>\n" ++ "( " ++ pshow bnode2 ++ " )"
    pshow (BNinterrupt bnode1 bnode2)
      =  "( " ++ pshow bnode1 ++ " )\n" ++ "[><\n"
         ++ "( " ++ pshow bnode2 ++ " )"
    pshow (BNhide chans bnode)
      =  "HIDE "
         ++ Utils.join "; " [ show n ++ " :: " ++ Utils.join " # " (map pshow srts)
                            | ChanId n _uid srts <- chans
                            ]
         ++ " IN\n"
         ++ pshow bnode ++ "\n"
         ++ "NI\n"

-- ----------------------------------------------------------------------------------------- --
-- PShow INode

instance PShow INode
  where
    pshow (BNbexpr (we,ivals) bexp)
      =  "VALENV "
         ++ Utils.join ";\n" (map (\(k,v) -> pshow k ++ " = " ++ pshow v) (Map.assocs ivals))
         ++ Utils.join ";\n" (map (\(k,v) -> pshow k ++ " = " ++ pshow v) (Map.assocs we))
         ++ " IN " ++ pshow bexp ++ " NI"
    pshow (BNparallel chans snodes)
      =  case snodes of
           [] -> "STOP\n"
           be -> "( "
                 ++ Utils.join (" )\n"++"|[ "++ Utils.join ", " (map pshow chans) ++" ]|\n( ")
                               (map pshow be)
                 ++ " )"
    pshow (BNenable snode1 choffs snode2)
      =  "( " ++ pshow snode1 ++ " )\n" ++ ">>>"
         ++ case choffs of
              [] -> "\n"
              _  -> " ACCEPT\n" ++ Utils.join ",\n" (map pshow choffs) ++ "\n" ++ "IN\n"
         ++ "( " ++ pshow snode2 ++ " )"
    pshow (BNdisable snode1 snode2)
      =  "( " ++ pshow snode1 ++ " )\n"
         ++ "[>>\n" ++ "( " ++ pshow snode2 ++ " )"
    pshow (BNinterrupt snode1 snode2)
      =  "( " ++ pshow snode1 ++ " )\n" ++ "[><\n"
         ++ "( " ++ pshow snode2 ++ " )"
    pshow (BNhide chans snode)
      =  "HIDE "
         ++ Utils.join "; " [ show n ++ " :: " ++ Utils.join " # " (map pshow srts)
                            | ChanId n _uid srts <- chans
                            ]
         ++ " IN\n"
         ++ pshow snode ++ "\n"
         ++ "NI\n"

-- ----------------------------------------------------------------------------------------- --
-- PShow BTree

instance PShow CTBranch
  where
    pshow (CTpref btoffs _hidvars spreds snext)
      =  " ## " ++ pshow btoffs ++ pshow spreds ++ pshow snext ++ "\n"

instance PShow BBranch
  where
    pshow (BTpref btoffs hidvars spreds snext)
      =  " ## " ++ pshow btoffs ++ pshow hidvars ++ pshow spreds ++ pshow snext ++ "\n"
    pshow (BTtau bt)
      =  " ## ISTEP " ++ pshow bt ++ "\n"

instance PShow CTOffer
  where
    pshow (CToffer btchan btchoffs)
      =  pshow btchan ++ pshow btchoffs

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
