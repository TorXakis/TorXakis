{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE ViewPatterns #-}
module Next

-- ----------------------------------------------------------------------------------------- --
--
-- Making Next Step in Behaviour Tree
--
-- ----------------------------------------------------------------------------------------- --
-- export

( nextNode      --  nextNode :: IWals -> INode -> CNode
                --  nextNode iwals inode
                --  makes step in BTree by instantatiating next inode with iwals into CNode
)

where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import TxsUtils(combineWEnv)
import CTree

-- import SolveDefs


-- ----------------------------------------------------------------------------------------- --
-- next node :  next step in BTree by instantatiating next INode with iwals into CNode


nextNode :: IWals -> INode -> CNode

nextNode iwals (BNbexpr (wenv,ivenv) bexp)
  =  let we = Map.fromList [ ( vid
                             , case Map.lookup ivar iwals of
                               { Nothing  -> error "TXS Next nextNode: Incomplete instance\n"
                               ; Just wal -> wal
                               }
                             )
                           | (vid, view -> Vvar ivar) <- Map.toList ivenv
                           ]
      in BNbexpr (combineWEnv we wenv) bexp

nextNode iwals (BNparallel chids inodes)
  =  BNparallel chids (map (nextNode iwals) inodes)

nextNode iwals (BNenable inode1 choffs inode2)
  =  BNenable (nextNode iwals inode1) choffs (nextNode iwals inode2)

nextNode iwals (BNdisable inode1 inode2)
  =  BNdisable (nextNode iwals inode1) (nextNode iwals inode2)

nextNode iwals (BNinterrupt inode1 inode2)
  =  BNinterrupt (nextNode iwals inode1) (nextNode iwals inode2)

nextNode iwals (BNhide chids inode)
  =  BNhide chids (nextNode iwals inode)


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

