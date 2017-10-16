{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
-- ----------------------------------------------------------------------------------------- --

module Reduce

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Reduction, and Simplification on BTree's
--
-- ----------------------------------------------------------------------------------------- --
-- export

( Reduce (..)
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map


import TxsDefs
import StdTDefs
import BTree
import qualified EnvBTree   as IOB
import Utils

import FreeVar
import Eval
import Subst

import Equiv


-- import System.Random
-- import Debug.Trace


-- ----------------------------------------------------------------------------------------- --
-- reduction


class Reduce e
  where
    reduce :: e -> IOB.IOB e
    reduce = return


-- ----------------------------------------------------------------------------------------- --
-- Reduce :  BTree


instance Reduce BTree
  where

    reduce bt = do

         btree1              <- nubMby (~=~) bt

         let (visibleBranches, tauBranches) = List.partition isPref btree1
             stableTauBranches = [ bb
                                 | bb@(BTtau btree') <- tauBranches
                                 , isStable btree'
                                 ]
             unStableTauBranches = [ bb
                                   | bb@(BTtau btree') <- tauBranches
                                   , not $ isStable btree'
                                   ]
             visAfterTauBranches = [ bb
                                   | BTtau btree' <- unStableTauBranches
                                   , bb@BTpref{} <- btree'
                                   ]
             tauAfterTauBranches = [ bb
                                   | BTtau btree' <- unStableTauBranches
                                   , bb@BTtau{} <- btree'
                                   ]
         visibleTree     <- mapM reduce visibleBranches
         stableTauTree   <- mapM reduce stableTauBranches
         visAfterTauTree <- mapM reduce visAfterTauBranches
         tauAfterTauTree <- mapM reduce tauAfterTauBranches
         btree2 <- nubMby (~=~) $ visibleTree
                                  ++ stableTauTree
                                  ++ visAfterTauTree
                                  ++ tauAfterTauTree

         let afterTauBranches2 = [ bb | bb@BTtau{}  <- btree2 ]
             visibleBranches2  = [ bb | bb@BTpref{} <- btree2 ]
             afterTauBranches3 = concat [ btree'
                                        | BTtau btree' <- afterTauBranches2
                                        ]
             visibleBranches3  = [ bb
                                 | bb <- visibleBranches2
                                 , bb `notElem` afterTauBranches3
                                 ]
             btree3 =  afterTauBranches2 ++ visibleBranches3
         return btree3

-- ----------------------------------------------------------------------------------------- --

instance Reduce BBranch
  where

    reduce (BTpref btoffs bthidvars' btpreds' btnext') =
         if  null bthidvars'
           then do reducedBtnext <- reduce btnext'
                   return $ BTpref btoffs bthidvars' btpreds' reducedBtnext
           else    return $ BTpref btoffs bthidvars' btpreds' btnext'

    reduce (BTtau bt) = do
         btree' <- reduce bt
         return $ BTtau btree'

-- ----------------------------------------------------------------------------------------- --
-- Reduce :  INode  --  assumption: no hidvars (?)

instance Reduce INode
  where

    reduce (BNbexpr (wenv,ivenv) bexp) = do
         bexp' <- reduce $ Subst.subst (Map.map cstrConst wenv) bexp
         return $ BNbexpr (Map.empty, ivenv) bexp'

    reduce (BNparallel chids inodes) = do
         inodes' <- mapM reduce inodes
         let (stops,nstops) = List.partition ( \i -> case i of { BNbexpr _ Stop -> True
                                                               ; _ -> False
                                                               }
                                             )
                                             inodes'
             chans'         = Set.unions $ map (Set.fromList . freeChans) nstops
             chids'         = Set.fromList (chanId_Exit:chids) `Set.intersection` chans'
             chids''        = Set.toList chids' \\\ [chanId_Exit]
         case (stops,nstops) of
           ( _ , [] )       -> return stopINode
           ( [] , [inode] ) -> return inode
           ( [] , inods )   -> return $ BNparallel chids'' inods
           ( j:_ , i:is )   -> if  Set.null chids'
                                  then return $ BNparallel chids'' (i:is)
                                  else return $ BNparallel chids'' (j:i:is)

    reduce (BNenable inode1 choffs inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr (_,_) Stop , BNbexpr (_,_) Stop ) -> return stopINode
           ( BNbexpr (_,_) Stop , _                  ) -> return stopINode
           ( _                  , _                  ) -> return $ BNenable inode1' choffs inode2'

    reduce (BNdisable inode1 inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr (_,_) Stop , BNbexpr (_,_) Stop ) -> return stopINode
           ( BNbexpr (_,_) Stop , _                  ) -> return inode2'
           ( _                  , BNbexpr (_,_) Stop ) -> return inode1'
           ( _                  , _                  ) -> return $ BNdisable inode1' inode2'

    reduce (BNinterrupt inode1 inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr (_,_) Stop , BNbexpr (_,_) Stop ) -> return stopINode
           ( _                  , _                  ) -> return $ BNinterrupt inode1' inode2'

    reduce (BNhide chids inode) = do
         inode' <- reduce inode
         let chans' = Set.fromList $ freeChans inode'
             chids' = Set.fromList chids `Set.intersection` chans'
         case inode' of
           BNbexpr (_,_) Stop -> return stopINode
           _                  -> if  Set.null chids'
                                   then return inode'
                                   else return $ BNhide (Set.toList chids') inode'

-- ----------------------------------------------------------------------------------------- --
-- Reduce :  BExpr

instance Reduce BExpr
  where

    reduce Stop = return Stop

    reduce (ActionPref (ActOffer offs c) bexp) = do
         c'  <- reduce c
         case view c' of
            Vconst (Cbool False) -> return Stop
            _                    -> do offs' <- mapM reduce (Set.toList offs)
                                       bexp' <- reduce bexp
                                       return $ ActionPref (ActOffer (Set.fromList offs') c') bexp'

    reduce (Guard c bexp) = do
         c'  <- reduce c
         case view c' of
            Vconst (Cbool True)  -> reduce bexp
            Vconst (Cbool False) -> return Stop
            _                    -> do bexp' <- reduce bexp
                                       if bexp' == Stop
                                         then return Stop
                                         else return $ Guard c' bexp'

    reduce (Choice bexps) = do
         bexps' <- mapM reduce bexps
         let bexps'' = filter (/= Stop) bexps'
         bexps''' <- nubMby (~=~) bexps''
         if  null bexps'''
           then return Stop
           else return $ Choice bexps'''

    reduce (Parallel chids bexps) = do
         bexps' <- mapM reduce bexps
         let (stops,nstops) = List.partition (== Stop) bexps'
             chans'  = Set.unions $ map (Set.fromList . freeChans) nstops
             chids'  = Set.fromList (chanId_Exit:chids) `Set.intersection` chans'
             chids'' = Set.toList chids' \\\ [chanId_Exit]
         case (stops,nstops) of
           ( _ , [] )      -> return Stop
           ( [] , [bexp] ) -> return bexp
           ( [] , bexs )   -> return $ Parallel chids'' bexs
           ( c:_ , b:bs )  -> if  Set.null chids'
                                then return $ Parallel chids'' (b:bs)
                                else return $ Parallel chids'' (c:b:bs)

    reduce (Enable bexp1 choffs bexp2) = do
         bexp1'  <- reduce bexp1
         bexp2'  <- reduce bexp2
         case (bexp1',bexp2') of
           ( Stop , Stop ) -> return Stop
           ( Stop , _    ) -> return Stop
           ( _    , _    ) -> return $ Enable bexp1' choffs bexp2'

    reduce (Disable bexp1 bexp2) = do
         bexp1' <- reduce bexp1
         bexp2' <- reduce bexp2
         case (bexp1',bexp2') of
           ( Stop , Stop ) -> return Stop
           ( Stop , _    ) -> return bexp2'
           ( _    , Stop ) -> return bexp1'
           ( _    , _    ) -> return $ Disable bexp1' bexp2'

    reduce (Interrupt bexp1 bexp2) = do
         bexp1' <- reduce bexp1
         bexp2' <- reduce bexp2
         case (bexp1',bexp2') of
           ( Stop , Stop ) -> return Stop
           ( _    , _    ) -> return $ Interrupt bexp1' bexp2'

    reduce (ProcInst pid chans vexps) = do
         vexps' <- mapM reduce vexps
         return $ ProcInst pid chans vexps'

    reduce (Hide chids bexp) = do
         bexp' <- reduce bexp
         if  bexp' == Stop
           then return Stop 
           else let chids' = List.nub chids `List.intersect` freeChans bexp'
                in if null chids'
                     then return bexp'
                     else return $ Hide chids' bexp'

    reduce (ValueEnv venv bexp) = reduce $ Subst.subst venv bexp

    reduce (StAut stid ve trns) = return $ StAut stid ve trns

-- ----------------------------------------------------------------------------------------- --

instance Reduce Offer
  where
    reduce (Offer chid choffs) = do
         choffs' <- mapM reduce choffs
         return $ Offer chid choffs'


instance Reduce ChanOffer
  where
    reduce (Quest vid)   = return $ Quest vid
    reduce (Exclam vexp) = do { vexp' <- reduce vexp ; return $ Exclam vexp' }

-- ----------------------------------------------------------------------------------------- --
-- Reduce :  VExpr

instance Reduce VExpr
  where
    reduce vexp =
         if  isClosed vexp
           then do wal <- eval vexp
                   return $ cstrConst wal
           else return vexp

-- ----------------------------------------------------------------------------------------- --
-- class: free channels

class FreeChan t
  where
    freeChans :: t -> [ChanId]


instance FreeChan INode
  where
    freeChans inode  =  List.nub $ freeChans' inode
      where
       freeChans' (BNbexpr (_,_) bexp)        = freeChans bexp
       freeChans' (BNparallel chids inodes)   = chids ++ concatMap freeChans' inodes
       freeChans' (BNenable inode1 _ inode2)  = freeChans' inode1 ++ freeChans' inode2
       freeChans' (BNdisable inode1 inode2)   = freeChans' inode1 ++ freeChans' inode2
       freeChans' (BNinterrupt inode1 inode2) = freeChans' inode1 ++ freeChans' inode2
       freeChans' (BNhide chids inode')       = List.nub (freeChans' inode') List.\\ chids

instance FreeChan BExpr
  where
    freeChans bexpr  =  List.nub $ freeChans' bexpr
      where
        freeChans'  Stop                   = []
        freeChans' (ActionPref ao bexp)    = freeChans ao ++ freeChans' bexp
        freeChans' (Guard _ bexp)          = freeChans' bexp
        freeChans' (Choice bexps)          = concatMap freeChans' bexps
        freeChans' (Parallel chids bexps)  = chids ++ concatMap freeChans' bexps
        freeChans' (Enable bexp1 _ bexp2)  = freeChans' bexp1 ++ freeChans' bexp2
        freeChans' (Disable bexp1 bexp2)   = freeChans' bexp1 ++ freeChans' bexp2
        freeChans' (Interrupt bexp1 bexp2) = freeChans' bexp1 ++ freeChans' bexp2
        freeChans' (ProcInst _ chids _)    = chids
        freeChans' (Hide chids bexp)       = freeChans bexp List.\\ chids         -- use freeChans since list-difference \\ only removes first occurrence of elem
        freeChans' (ValueEnv _ bexp)       = freeChans' bexp
        freeChans' (StAut _ _ trns)        = concatMap freeChans trns

instance FreeChan ActOffer
  where
    freeChans (ActOffer offs _) = map chanid $ Set.toList offs

instance FreeChan Trans
  where
    freeChans (Trans _ actoffer' _ _) = freeChans actoffer'

instance (FreeChan t) => FreeChan [t]
  where
    freeChans = concatMap freeChans

-- ----------------------------------------------------------------------------------------- --
-- helper functions

isPref :: BBranch -> Bool
isPref BTpref{} = True
isPref BTtau{}  = False

isStable :: BTree -> Bool
isStable = all isPref

stopINode :: INode
stopINode = BNbexpr (Map.empty, Map.empty) Stop

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
