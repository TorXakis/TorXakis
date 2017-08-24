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
    reduce x  =  do  return $ x


-- ----------------------------------------------------------------------------------------- --
-- Reduce :  BTree


instance Reduce BTree
  where

    reduce bt  =  do

         btree1              <- nubMby (~=~) bt

         (visibleBranches, tauBranches)
                             <- return $ List.partition isPref btree1

         stableTauBranches   <- return $ [ bb
                                         | bb@(BTtau btree') <- tauBranches
                                         , isStable btree'
                                         ]
         unStableTauBranches <- return $ [ bb
                                         | bb@(BTtau btree') <- tauBranches
                                         , not $ isStable btree'
                                         ]

         visAfterTauBranches <- return $ [ bb
                                         | BTtau btree' <- unStableTauBranches
                                         , bb@(BTpref _ _ _ _) <- btree'
                                         ]
         tauAfterTauBranches <- return $ [ bb
                                         | BTtau btree' <- unStableTauBranches
                                         , bb@(BTtau _) <- btree' 
                                         ]

         visibleTree     <- mapM reduce visibleBranches
         stableTauTree   <- mapM reduce stableTauBranches
         visAfterTauTree <- mapM reduce visAfterTauBranches
         tauAfterTauTree <- mapM reduce tauAfterTauBranches

         btree2 <- nubMby (~=~) $   visibleTree
                               ++ stableTauTree
                               ++ visAfterTauTree
                               ++ tauAfterTauTree

         afterTauBranches2 <- return $ [ bb | bb@BTtau{}  <- btree2 ]
         visibleBranches2  <- return $ [ bb | bb@BTpref{} <- btree2 ]
         afterTauBranches3 <- return $ concat [ btree'
                                              | BTtau btree' <- afterTauBranches2
                                              ]
         visibleBranches3  <- return $ [ bb
                                       | bb <- visibleBranches2
                                       , bb `notElem` afterTauBranches3
                                       ]
         btree3 <- return $  afterTauBranches2
                           ++ visibleBranches3

         return $ btree3


-- ----------------------------------------------------------------------------------------- --


instance Reduce BBranch
  where

    reduce (BTpref btoffs bthidvars' btpreds' btnext')  =  do
         if  null bthidvars'
           then do reducedBtnext <- reduce btnext'
                   return $ BTpref btoffs bthidvars' btpreds' reducedBtnext
           else do return $ BTpref btoffs bthidvars' btpreds' btnext'

    reduce (BTtau bt)  =  do
         btree' <- reduce bt
         return $ BTtau btree'


-- ----------------------------------------------------------------------------------------- --
-- Reduce :  INode  --  assumption: no hidvars (?)


instance Reduce INode
  where

    reduce (BNbexpr (wenv,ivenv) bexp)  =  do
         bexp' <- reduce $ subst (Map.map cstrConst wenv) bexp
         return $ BNbexpr (Map.empty, ivenv) bexp'

    reduce (BNparallel chids inodes)  =  do
         inodes' <- mapM reduce inodes
         (stops,nstops) <- return $ List.partition ( \i -> case i of { BNbexpr _ Stop -> True
                                                                     ; _ -> False
                                                                     }
                                                   )
                                                   inodes'
         chans'  <- return $ Set.unions $ map Set.fromList $ map freeChans nstops
         chids'  <- return $ (Set.fromList (chanId_Exit:chids)) `Set.intersection` chans'
         chids'' <- return $ (Set.toList chids') \\\ [chanId_Exit]
         case (stops,nstops) of
         { ( _ , [] )          -> do  return $ stopINode
         ; ( [] , [inode] )    -> do  return $ inode
         ; ( [] , inods )      -> do  return $ BNparallel chids'' inods
         ; ( (j:_) , (i:is) )  -> do  if  Set.null $ chids'
                                        then do return $ BNparallel chids'' (i:is)
                                        else do return $ BNparallel chids'' (j:i:is)
         }
                
    reduce (BNenable inode1 choffs inode2)  =  do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
         { ( BNbexpr (_,_) Stop , BNbexpr (_,_) Stop ) -> do
                return $ stopINode
         ; ( BNbexpr (_,_) Stop , _                  ) -> do
                return $ stopINode
         ; ( _                  , _                  ) -> do
                return $ BNenable inode1' choffs inode2'
         }

    reduce (BNdisable inode1 inode2)  =  do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
         { ( BNbexpr (_,_) Stop , BNbexpr (_,_) Stop ) -> do
                return $ stopINode
         ; ( BNbexpr (_,_) Stop , _                  ) -> do
                return $ inode2'
         ; ( _                  , BNbexpr (_,_) Stop ) -> do
                return $ inode1'
         ; ( _                  , _                  ) -> do
                return $ BNdisable inode1' inode2'
         }

    reduce (BNinterrupt inode1 inode2)  =  do
         inode1' <- reduce inode1
         inode2' <- reduce inode2
         case (inode1',inode2') of
         { ( BNbexpr (_,_) Stop , BNbexpr (_,_) Stop ) -> do
                return $ stopINode
         ; ( _                  , _                  ) -> do
                return $ BNinterrupt inode1' inode2'
         }

    reduce (BNhide chids inode)  =  do
         inode' <- reduce inode
         chans' <- return $ Set.fromList $ freeChans inode'
         chids' <- return $ (Set.fromList chids) `Set.intersection` chans'
         case inode' of
         { BNbexpr (_,_) Stop -> do
                return $ stopINode
         ; _                  -> do
                if  Set.null chids'
                  then do return $ inode'
                  else do return $ BNhide (Set.toList chids') inode'
         }


-- ----------------------------------------------------------------------------------------- --
-- Reduce :  BExpr


instance Reduce BExpr
  where


    reduce Stop  =  do
         return $ Stop

    reduce (ActionPref (ActOffer offs cnrs) bexp)  =  do
         offs'  <- mapM reduce (Set.toList offs)
         offs'' <- return $ Set.fromList offs'
         cnrs'  <- mapM reduce cnrs
         cnrs'' <- return $ filter (/= (cstrConst (Cbool True))) cnrs'
         if  null cnrs''
           then do bexp' <- reduce bexp
                   return $ ActionPref (ActOffer offs'' []) bexp'
           else if (cstrConst (Cbool False)) `elem` cnrs''
                  then do return $ Stop
                  else do bexp' <- reduce bexp
                          return $ ActionPref (ActOffer offs'' cnrs'') bexp'

    reduce (Guard cnrs bexp)  =  do
         cnrs'  <- mapM reduce cnrs
         cnrs'' <- return $ filter (/= (cstrConst (Cbool True))) cnrs'
         if  null cnrs''
           then do reduce bexp
           else if (cstrConst (Cbool False)) `elem` cnrs''
                  then do return $ Stop
                  else do bexp' <- reduce bexp
                          if  bexp' == Stop
                            then return $ Stop
                            else return $ Guard cnrs'' bexp'

    reduce (Choice bexps)  =  do
         bexps'   <- mapM reduce bexps
         bexps''  <- return $ filter (/= Stop) bexps'
         bexps''' <- nubMby (~=~) bexps''
         if  null bexps'''
           then do return $ Stop
           else do return $ Choice bexps'''

    reduce (Parallel chids bexps)  =  do
         bexps' <- mapM reduce bexps
         (stops,nstops) <- return $ List.partition (\b -> b == Stop) bexps'
         chans'  <- return $ Set.unions $ map Set.fromList $ map freeChans nstops
         chids'  <- return $ (Set.fromList (chanId_Exit:chids)) `Set.intersection` chans'
         chids'' <- return $ (Set.toList chids') \\\ [chanId_Exit]
         case (stops,nstops) of
         { ( _ , [] )          -> do  return $ Stop
         ; ( [] , [bexp] )     -> do  return $ bexp
         ; ( [] , bexs )       -> do  return $ Parallel chids'' bexs
         ; ( (c:_) , (b:bs) )  -> do  if  Set.null $ chids'
                                        then do return $ Parallel chids'' (b:bs)
                                        else do return $ Parallel chids'' (c:b:bs)
         }

    reduce (Enable bexp1 choffs bexp2)  =  do
         bexp1'  <- reduce bexp1
         bexp2'  <- reduce bexp2
         case (bexp1',bexp2') of
         { ( Stop , Stop ) -> do return $ Stop
         ; ( Stop , _    ) -> do return $ Stop
         ; ( _    , _    ) -> do return $ Enable bexp1' choffs bexp2'
         }

    reduce (Disable bexp1 bexp2)  =  do
         bexp1' <- reduce bexp1
         bexp2' <- reduce bexp2
         case (bexp1',bexp2') of
         { ( Stop , Stop ) -> do return $ Stop
         ; ( Stop , _    ) -> do return $ bexp2'
         ; ( _    , Stop ) -> do return $ bexp1'
         ; ( _    , _    ) -> do return $ Disable bexp1' bexp2'
         }

    reduce (Interrupt bexp1 bexp2)  =  do
         bexp1' <- reduce bexp1
         bexp2' <- reduce bexp2
         case (bexp1',bexp2') of
         { ( Stop , Stop ) -> do return $ Stop
         ; ( _    , _    ) -> do return $ Interrupt bexp1' bexp2'
         }

    reduce (ProcInst pid chans vexps)  =  do
         vexps' <- mapM reduce vexps
         return $ ProcInst pid chans vexps'

    reduce (Hide chids bexp)  =  do
         bexp'  <- reduce bexp
         if  bexp' == Stop
           then do return $ Stop 
           else do chids' <- return $ (List.nub chids) `List.intersect` (freeChans bexp')
                   if  null chids'
                     then do return $ bexp'
                     else do return $ Hide chids' bexp'

    reduce (ValueEnv venv bexp)  =  do
         reduce $ subst venv bexp

    reduce (StAut stid ve trns)  =  do
         return $ StAut stid ve trns


-- ----------------------------------------------------------------------------------------- --


instance Reduce Offer
  where
    reduce (Offer chid choffs)  =  do
         choffs' <- mapM reduce choffs
         return $ Offer chid choffs'


instance Reduce ChanOffer
  where
    reduce (Quest vid)    =  do { return $ Quest vid }
    reduce (Exclam vexp)  =  do { vexp' <- reduce vexp ; return $ Exclam vexp' }


-- ----------------------------------------------------------------------------------------- --
-- Reduce :  VExpr


instance Reduce VExpr
  where
    reduce vexp  =  do
         if  isClosed vexp
           then do wal <- eval vexp
                   return $ cstrConst wal
           else do return $ vexp


-- ----------------------------------------------------------------------------------------- --
-- class: free channels


class FreeChan t
  where
    freeChans :: t -> [ChanId]


instance FreeChan INode
  where
    freeChans inode  =  List.nub $ freeChans' inode
    
     where
      freeChans' (BNbexpr (_,_) bexp)            = freeChans bexp
      freeChans' (BNparallel chids inodes)       = chids ++ concatMap freeChans' inodes
      freeChans' (BNenable inode1 _ inode2)      = freeChans' inode1 ++ freeChans' inode2
      freeChans' (BNdisable inode1 inode2)       = freeChans' inode1 ++ freeChans' inode2
      freeChans' (BNinterrupt inode1 inode2)     = freeChans' inode1 ++ freeChans' inode2
      freeChans' (BNhide chids inode')           = (List.nub $ freeChans' inode') List.\\ chids

instance FreeChan BExpr
  where
    freeChans bexpr  =  List.nub $ freeChans' bexpr

     where
      freeChans'  Stop                       = []
      freeChans' (ActionPref ao bexp)        = freeChans ao ++ freeChans' bexp
      freeChans' (Guard _ bexp)              = freeChans' bexp
      freeChans' (Choice bexps)              = concatMap freeChans' bexps
      freeChans' (Parallel chids bexps)      = chids ++ concatMap freeChans' bexps
      freeChans' (Enable bexp1 _ bexp2)      = freeChans' bexp1 ++ freeChans' bexp2
      freeChans' (Disable bexp1 bexp2)       = freeChans' bexp1 ++ freeChans' bexp2
      freeChans' (Interrupt bexp1 bexp2)     = freeChans' bexp1 ++ freeChans' bexp2
      freeChans' (ProcInst _ chids _)        = chids
      freeChans' (Hide chids bexp)           = freeChans bexp List.\\ chids             -- use freeChans since list-difference \\ only removes first occurrence of elem
      freeChans' (ValueEnv _ bexp)           = freeChans' bexp
      freeChans' (StAut _ _ trns)            = concatMap freeChans trns

instance FreeChan ActOffer
  where
    freeChans (ActOffer offs _)   =  map chanid $ Set.toList offs


instance FreeChan Trans
  where
    freeChans (Trans _ actoffer' _ _)  =  freeChans actoffer'


instance (FreeChan t) => FreeChan [t]
  where
    freeChans list  =  concatMap freeChans list


-- ----------------------------------------------------------------------------------------- --
-- dealing with maps: composition with domain, remove identities
-- mapCompose m1 m2 with m1 :: a->b, m2 :: b->c, gives a->c


-- mapCompose :: (Ord t) => (Map.Map t t) -> (Map.Map t t) -> (Map.Map t t)
-- mapCompose m1 m2
--   =  Map.fromList [ (a,c)
--                   | a <- Map.keys m1
--                   , b <- [Map.findWithDefault a a m1]
--                   , c <- [Map.findWithDefault b b m2]
--                   ]


-- mapRemoveIdent :: (Ord t) => (Map.Map t t) -> (Map.Map t t)
-- mapRemoveIdent m
--   =  Map.fromList [ (a,a') | (a,a') <- Map.toList m, a /= a' ]


-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --
-- helper functions


isPref :: BBranch -> Bool
isPref (BTpref _ _ _ _)  =  True
isPref (BTtau _)         =  False


{-
isTau :: BBranch -> Bool
isTau (BTpref _ _ _ _)  =  False
isTau (BTtau _)         =  True
-}

isStable :: BTree -> Bool
isStable bt  =  and $ map isPref bt


stopINode :: INode
stopINode  =  BNbexpr (Map.empty, Map.empty) Stop


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
