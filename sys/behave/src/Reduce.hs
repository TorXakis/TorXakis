{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Data.List     as List
import qualified Data.Map      as Map
import qualified Data.MultiSet as MultiSet
import qualified Data.Set      as Set

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
           then             BTpref btoffs bthidvars' btpreds' <$> reduce btnext'
           else    return $ BTpref btoffs bthidvars' btpreds' btnext'

    reduce (BTtau bt) =
         BTtau <$> reduce bt

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
                                                  BNbexpr _ bexpr -> isStop bexpr
                                                  _               -> False
                                             )
                                             inodes'
             chans'  = Set.unions $ map freeChans nstops
             chids'  = chids `Set.intersection` chans'
         case (stops,nstops) of
           ( _  , [] )       -> return stopINode
           ( []  , [inode] ) -> return inode
           ( []  , inods )   -> return $ BNparallel chids' inods
           ( j:_ , i:is )    -> if Set.null chids' -- PvdL: performance/change issue - should be equal to chanIdExit? chanIdExit will always be part of intersection....
                                  then return $ BNparallel chids' (i:is)
                                  else return $ BNparallel chids' (j:i:is)

    reduce (BNenable inode1 choffs inode2) = do
         inode1' <- reduce inode1
         case inode1' of
            BNbexpr _ bexpr | isStop bexpr -> return stopINode
            _                              -> do
                                                inode2' <- reduce inode2
                                                return $ BNenable inode1' choffs inode2'

    reduce (BNdisable inode1 inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr _ bexpr1 , BNbexpr _ bexpr2 ) | isStop bexpr1 && isStop bexpr2 -> return stopINode
           ( BNbexpr _ bexpr1 , _                ) | isStop bexpr1                  -> return inode2'
           ( _                , BNbexpr _ bexpr2 ) |                  isStop bexpr2 -> return inode1'
           ( _                , _                )                                  -> return $ BNdisable inode1' inode2'

    reduce (BNinterrupt inode1 inode2) = do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
           ( BNbexpr _ bexpr1, BNbexpr _ bexpr2 ) | isStop bexpr1 && isStop bexpr2 -> return stopINode
           ( _               , _                )                                  -> return $ BNinterrupt inode1' inode2'

    reduce (BNhide chids inode) = do
         inode' <- reduce inode
         let chans' = freeChans inode'
             chids' = chids `Set.intersection` chans'
         case inode' of
           BNbexpr _ bexpr | isStop bexpr -> return stopINode
           _                              -> if Set.null chids'
                                                then return inode'
                                                else return $ BNhide chids' inode'

-- ----------------------------------------------------------------------------------------- --
-- Reduce :  BExpr

instance Reduce BExpr
  where
    reduce = reduce' . TxsDefs.view

reduce' :: BExprView -> IOB.IOB BExpr
reduce' (ActionPref (ActOffer offs hidvars c) bexp) = do
     c'  <- reduce c
     case ValExpr.view c' of
        Vconst (Cbool False) -> return stop
        _                    -> do offs' <- mapM reduce (Set.toList offs)
                                   bexp' <- reduce bexp
                                   return $ actionPref (ActOffer (Set.fromList offs') hidvars c') bexp'

reduce' (Guard c bexp) = do
     c'  <- reduce c
     case ValExpr.view c' of
        Vconst (Cbool True)  -> reduce bexp
        Vconst (Cbool False) -> return stop
        _                    -> do bexp' <- reduce bexp
                                   if isStop bexp'
                                     then return stop       -- we can drop condition
                                     else return $ guard c' bexp'

reduce' (Choice bexps) = do
     bexps' <- mapM reduce (Set.toList bexps)
     let bexps'' = filter (not . isStop) bexps'
     bexps''' <- nubMby (~=~) bexps''
     if  null bexps'''
       then return stop
       else return $ choice (Set.fromList bexps''')

reduce' (Parallel chids bexps) = do
     bexps' <- multiSetMapM reduce bexps
     let (stops,nstops) = MultiSet.partition isStop bexps'
         chans'  = Set.unions $ map freeChans (MultiSet.distinctElems nstops)
         chids'  = chids `Set.intersection` chans'
     case (MultiSet.toList stops, MultiSet.toList nstops) of
       ( _ , [] )      -> return stop
       ( [] , [bexp] ) -> return bexp
       ( [] , _      ) -> return $ parallel chids' nstops
       ( c:_ , _    )  -> if  Set.null chids'       -- PvdL: performance/change issue - should be equal to chanIdExit? chanIdExit will always be part of intersection....
                            then return $ parallel chids' nstops
                            else return $ parallel chids' (MultiSet.insert c nstops)
  where multiSetMapM :: forall a b m . (Ord b, Monad m) => (a-> m b) -> MultiSet.MultiSet a -> m (MultiSet.MultiSet b)
        multiSetMapM f a = MultiSet.fromOccurList <$> mapM f' (MultiSet.toOccurList a)
          where f' :: (a, MultiSet.Occur) -> m (b, MultiSet.Occur)
                f' (x,c) = do
                            v <- f x
                            return (v,c)

reduce' (Enable bexp1 choffs bexp2) = do
     bexp1'  <- reduce bexp1
     if isStop bexp1'
        then return stop
        else do
                bexp2'  <- reduce bexp2
                return $ enable bexp1' choffs bexp2'

reduce' (Disable bexp1 bexp2) = do
     bexp1' <- reduce bexp1
     bexp2' <- reduce bexp2
     case (isStop bexp1', isStop bexp2') of
       ( True , True ) -> return stop
       ( True , _    ) -> return bexp2'
       ( _    , True ) -> return bexp1'
       ( _    , _    ) -> return $ disable bexp1' bexp2'

reduce' (Interrupt bexp1 bexp2) = do
     bexp1' <- reduce bexp1
     bexp2' <- reduce bexp2
     case (isStop bexp1', isStop bexp2') of
       ( True , True ) -> return stop
       ( _    , _    ) -> return $ interrupt bexp1' bexp2'

reduce' (ProcInst pid chans vexps) =
     procInst pid chans <$> mapM reduce vexps

reduce' (Hide chids bexp) = do
     bexp' <- reduce bexp
     if isStop bexp'
       then return stop
       else let chids' = chids `Set.intersection` freeChans bexp'
            in if Set.null chids'
                 then return bexp'
                 else return $ hide chids' bexp'

reduce' (ValueEnv venv bexp) = do
    fdefs <- IOB.getFuncDefs
    reduce $ Subst.subst venv fdefs bexp

reduce' (StAut stid ve trns) = return $ stAut stid ve trns

-- ----------------------------------------------------------------------------------------- --

instance Reduce Offer
  where
    reduce (Offer chid choffs) =
         Offer chid <$> mapM reduce choffs


instance Reduce ChanOffer
  where
    reduce (Quest vid)   = return $ Quest vid
    reduce (Exclam vexp) = Exclam <$> reduce vexp

-- ----------------------------------------------------------------------------------------- --
-- Reduce :  VExpr

instance Reduce VExpr
  where
    reduce = return

-- ----------------------------------------------------------------------------------------- --
-- class: free channels

class FreeChan t
  where
    freeChans :: t -> Set.Set ChanId


instance FreeChan INode
  where
   freeChans (BNbexpr _ bexp)            = freeChans bexp
   freeChans (BNparallel chids inodes)   = Set.unions (chids:map freeChans inodes)
   freeChans (BNenable inode1 _ inode2)  = freeChans inode1 `Set.union` freeChans inode2
   freeChans (BNdisable inode1 inode2)   = freeChans inode1 `Set.union` freeChans inode2
   freeChans (BNinterrupt inode1 inode2) = freeChans inode1 `Set.union` freeChans inode2
   freeChans (BNhide chids inode')       = Set.difference (freeChans inode') chids

instance FreeChan BExpr
  where
    freeChans bexpr  =  freeChans' (TxsDefs.view bexpr)
      where
        freeChans' :: BExprView -> Set.Set ChanId
        freeChans' (ActionPref ao bexp)    = freeChans ao `Set.union` freeChans bexp
        freeChans' (Guard _ bexp)          = freeChans bexp
        freeChans' (Choice bexps)          = Set.unions $ map freeChans (Set.toList bexps)
        freeChans' (Parallel chids bexps)  = Set.unions (chids:map freeChans (MultiSet.distinctElems bexps))
        freeChans' (Enable bexp1 _ bexp2)  = freeChans bexp1 `Set.union` freeChans bexp2
        freeChans' (Disable bexp1 bexp2)   = freeChans bexp1 `Set.union` freeChans bexp2
        freeChans' (Interrupt bexp1 bexp2) = freeChans bexp1 `Set.union` freeChans bexp2
        freeChans' (ProcInst _ chids _)    = Set.fromList chids
        freeChans' (Hide chids bexp)       = Set.difference (freeChans bexp) chids
        freeChans' (ValueEnv _ bexp)       = freeChans bexp
        freeChans' (StAut _ _ trns)        = Set.unions (map freeChans trns)

instance FreeChan ActOffer
  where
    freeChans (ActOffer offs _ _) = Set.map chanid offs

instance FreeChan Trans
  where
    freeChans (Trans _ actoffer' _ _) = freeChans actoffer'

instance (FreeChan t) => FreeChan [t]
  where
    freeChans = Set.unions . map freeChans

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
