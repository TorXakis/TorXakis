{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ViewPatterns        #-}
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
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import           BTree
import           ConstDefs
import qualified EnvBTree  as IOB
import           Subst
import           TxsDefs
import           ValExpr

-- ----------------------------------------------------------------------------------------- --
-- reduction

import           Equiv

class Reduce e where
    reduce :: e -> IOB.IOB e
    reduce = return

instance Reduce BTree where

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

instance Reduce INode where

    reduce (BNbexpr (wenv,ivenv) bexp) = do
        fdefs <- IOB.getFuncDefs
        bexp' <- reduce $ Subst.subst (Map.map cstrConst wenv) fdefs bexp
        return $ BNbexpr (Map.empty, ivenv) bexp'

    reduce (BNparallel chids inodes) = do
         inodes' <- mapM reduce inodes
         let (stops,nstops) = List.partition ( \case 
                                                  BNbexpr _ (TxsDefs.view -> Stop) -> True
                                                  _                                -> False
                                             )
                                             inodes'
             chans'  = Set.unions $ map (Set.fromList . freeChans) nstops
             chids'  = Set.fromList chids `Set.intersection` chans'
             chids'' = Set.toList chids'
         case (stops,nstops) of
           ( _  , [] )       -> return stopINode
           ( []  , [inode] ) -> return inode
           ( []  , inods )   -> return $ BNparallel chids'' inods
           ( j:_ , i:is )    -> if  Set.null chids'
                                  then return $ BNparallel chids'' (i:is)
                                  else return $ BNparallel chids'' (j:i:is)

    reduce (BNenable inode1 choffs inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr (_,_) (TxsDefs.view -> Stop) , BNbexpr (_,_) (TxsDefs.view -> Stop) ) -> return stopINode
           ( BNbexpr (_,_) (TxsDefs.view -> Stop) , _                                    ) -> return stopINode
           ( _                                    , _                                    ) -> return $ BNenable inode1' choffs inode2'

    reduce (BNdisable inode1 inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr (_,_) (TxsDefs.view -> Stop) , BNbexpr (_,_) (TxsDefs.view -> Stop) ) -> return stopINode
           ( BNbexpr (_,_) (TxsDefs.view -> Stop) ,                   _                  ) -> return inode2'
           ( _                                    , BNbexpr (_,_) (TxsDefs.view -> Stop) ) -> return inode1'
           ( _                                    , _                                    ) -> return $ BNdisable inode1' inode2'

    reduce (BNinterrupt inode1 inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr (_,_) (TxsDefs.view -> Stop) , BNbexpr (_,_) (TxsDefs.view -> Stop) ) -> return stopINode
           ( _                                    , _                                    ) -> return $ BNinterrupt inode1' inode2'

    reduce (BNhide chids inode) = do
         inode' <- reduce inode
         let chans' = Set.fromList $ freeChans inode'
             chids' = Set.fromList chids `Set.intersection` chans'
         case inode' of
           BNbexpr (_,_) (TxsDefs.view -> Stop) -> return stopINode
           _                                    -> if  Set.null chids'
                                                     then return inode'
                                                     else return $ BNhide (Set.toList chids') inode'

-- ----------------------------------------------------------------------------------------- --
-- Reduce :  BExpr

instance Reduce BExpr
  where
    reduce = reduce' . TxsDefs.view

reduce' :: BExprView -> IOB.IOB BExpr
reduce' Stop = return stop

reduce' (ActionPref (ActOffer offs c) bexp) = do
     c'  <- reduce c
     case ValExpr.view c' of
        Vconst (Cbool False) -> return stop
        _                    -> do offs' <- mapM reduce (Set.toList offs)
                                   bexp' <- reduce bexp
                                   return $ actionPref (ActOffer (Set.fromList offs') c') bexp'

reduce' (Guard c bexp) = do
     c'  <- reduce c
     case ValExpr.view c' of
        Vconst (Cbool True)  -> reduce bexp
        Vconst (Cbool False) -> return stop
        _                    -> do bexp' <- reduce bexp
                                   if TxsDefs.view bexp' == Stop
                                     then return stop
                                     else return $ guard c' bexp'

reduce' (Choice bexps) = do
     bexps' <- mapM reduce bexps
     let bexps'' = filter (\b -> TxsDefs.view b /= Stop) bexps'
     bexps''' <- nubMby (~=~) bexps''
     if  null bexps'''
       then return stop
       else return $ choice bexps'''

reduce' (Parallel chids bexps) = do
     bexps' <- mapM reduce bexps
     let (stops,nstops) = List.partition (\b -> TxsDefs.view b == Stop) bexps'
         chans'  = Set.unions $ map (Set.fromList . freeChans) nstops
         chids'  = Set.fromList chids `Set.intersection` chans'
         chids'' = Set.toList chids'
     case (stops,nstops) of
       ( _ , [] )      -> return stop
       ( [] , [bexp] ) -> return bexp
       ( [] , bexs )   -> return $ parallel chids'' bexs
       ( c:_ , b:bs )  -> if  Set.null chids'       -- PvdL: performance/change issue - should be equal to chanIdExit? chanIdExit will always be part of intersection....
                            then return $ parallel chids'' (b:bs)
                            else return $ parallel chids'' (c:b:bs)

reduce' (Enable bexp1 choffs bexp2) = do
     bexp1'  <- reduce bexp1
     bexp2'  <- reduce bexp2
     case (TxsDefs.view bexp1', TxsDefs.view bexp2') of
       ( Stop , Stop ) -> return stop
       ( Stop , _    ) -> return stop
       ( _    , _    ) -> return $ enable bexp1' choffs bexp2'

reduce' (Disable bexp1 bexp2) = do
     bexp1' <- reduce bexp1
     bexp2' <- reduce bexp2
     case (TxsDefs.view bexp1', TxsDefs.view bexp2') of
       ( Stop , Stop ) -> return stop
       ( Stop , _    ) -> return bexp2'
       ( _    , Stop ) -> return bexp1'
       ( _    , _    ) -> return $ disable bexp1' bexp2'

reduce' (Interrupt bexp1 bexp2) = do
     bexp1' <- reduce bexp1
     bexp2' <- reduce bexp2
     case (TxsDefs.view bexp1', TxsDefs.view bexp2') of
       ( Stop , Stop ) -> return stop
       ( _    , _    ) -> return $ interrupt bexp1' bexp2'

reduce' (ProcInst pid chans vexps) = do
     vexps' <- mapM reduce vexps
     return $ procInst pid chans vexps'

reduce' (Hide chids bexp) = do
     bexp' <- reduce bexp
     if  TxsDefs.view bexp' == Stop
       then return stop
       else let chids' = List.nub chids `List.intersect` freeChans bexp'
            in if null chids'
                 then return bexp'
                 else return $ hide chids' bexp'

reduce' (ValueEnv venv bexp) = do
    fdefs <- IOB.getFuncDefs
    reduce $ Subst.subst venv fdefs bexp

reduce' (StAut stid ve trns) = return $ stAut stid ve trns

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
    reduce = return

-- ----------------------------------------------------------------------------------------- --
-- class: free channels

class FreeChan t
  where
    freeChans :: t -> [ChanId]      -- PvdL : result is a set, why not use Set iso List?


instance FreeChan INode
  where
    freeChans inode  =  List.nub $ freeChans' inode
      where
       freeChans' (BNbexpr (_,_) bexp)        = freeChans bexp
       freeChans' (BNparallel chids inodes)   = chids ++ concatMap freeChans' inodes
       freeChans' (BNenable inode1 _ inode2)  = freeChans' inode1 ++ freeChans' inode2
       freeChans' (BNdisable inode1 inode2)   = freeChans' inode1 ++ freeChans' inode2
       freeChans' (BNinterrupt inode1 inode2) = freeChans' inode1 ++ freeChans' inode2
       freeChans' (BNhide chids inode')       = List.nub (freeChans' inode') List.\\ chids -- use List.nub since list-difference \\ only removes first occurrence of elem

instance FreeChan BExpr
  where
    freeChans bexpr  =  List.nub $ freeChans' (TxsDefs.view bexpr)
      where
        freeChans' :: BExprView -> [ChanId]
        freeChans'  Stop                   = []
        freeChans' (ActionPref ao bexp)    = freeChans ao ++ freeChans bexp
        freeChans' (Guard _ bexp)          = freeChans bexp
        freeChans' (Choice bexps)          = concatMap freeChans bexps
        freeChans' (Parallel chids bexps)  = chids ++ concatMap freeChans bexps
        freeChans' (Enable bexp1 _ bexp2)  = freeChans bexp1 ++ freeChans bexp2
        freeChans' (Disable bexp1 bexp2)   = freeChans bexp1 ++ freeChans bexp2
        freeChans' (Interrupt bexp1 bexp2) = freeChans bexp1 ++ freeChans bexp2
        freeChans' (ProcInst _ chids _)    = chids
        freeChans' (Hide chids bexp)       = freeChans bexp List.\\ chids         
        freeChans' (ValueEnv _ bexp)       = freeChans bexp
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
stopINode = BNbexpr (Map.empty, Map.empty) stop

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
