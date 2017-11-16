{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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

import qualified Data.Map  as Map
import Data.Maybe

import BTree
import TxsUtils(combineWEnv)
import ValExpr


-- ----------------------------------------------------------------------------------------- --
-- next node :  next step in BTree by instantatiating next INode with iwals into CNode


nextNode :: IWals -> INode -> CNode

nextNode iwals (BNbexpr (wenv,ivenv) bexp)
  =  let we = Map.fromList [ ( vid
                             , fromMaybe (error "TXS Next nextNode: Incomplete instance\n")
                                         (Map.lookup ivar iwals)
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
