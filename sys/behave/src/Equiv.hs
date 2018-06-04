{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
module Equiv

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Equivalence on BTree's
--
-- ----------------------------------------------------------------------------------------- --
-- export

( Equiv (..)
, nubMby
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Set            as Set
import qualified Data.MultiSet       as MultiSet

import           BTree
import qualified EnvBTree            as IOB
import           TxsDefs
import           Variable
import           ValExpr
import           SortOf
import           ConstDefs


-- ----------------------------------------------------------------------------------------- --
-- | Equiv : equivalence
class (Eq e) => Equiv e
  where
    (~=~) :: e -> e -> IOB.IOB Bool

    x ~=~ y  =  return $ x == y


instance (Equiv t) => Equiv [t]
  where
    []     ~=~ []      =  return True
    []     ~=~ (_:_)   =  return False
    (_:_)  ~=~ []      =  return False
    (x:xs) ~=~ (y:ys)  =  do  { eq_hd <- x  ~=~ y
                              ; eq_tl <- xs ~=~ ys
                              ; return $ eq_hd && eq_tl
                              }


instance (Ord t,Equiv t) => Equiv (Set.Set t)
  where
    xs ~=~ ys  =  do
         let xs' = Set.toList xs
         let ys' = Set.toList ys
         xs_sub_ys <- sequence [ elemMby (~=~) x ys' | x <- xs' ]
         ys_sub_xs <- sequence [ elemMby (~=~) y xs' | y <- ys' ]
         return $ and xs_sub_ys && and ys_sub_xs

instance (Ord t,Equiv t) => Equiv (MultiSet.MultiSet t)
  where 
    xs ~=~ ys = do
        xs' <- msMby (~=~) xs
        ys' <- msMby (~=~) ys
        xs_sub_ys <- sequence [ elemOccurenceMby (~=~) x ys' | x <- MultiSet.toOccurList xs' ]
        ys_sub_xs <- sequence [ elemOccurenceMby (~=~) y xs' | y <- MultiSet.toOccurList ys' ]
        return $ and xs_sub_ys && and ys_sub_xs

-- ----------------------------------------------------------------------------------------- --
-- Equiv :  BTree

{-  overlapping instances

instance Equiv BTree
  where
    btree1 ~=~ btree2  =  do
         (Set.fromList btree1) ~=~ (Set.fromList btree2)

-}


-- ----------------------------------------------------------------------------------------- --
-- Equiv :  BBranch


instance Equiv BBranch
  where

    (BTpref offs1 hvars1 preds1 next1) ~=~ (BTpref offs2 hvars2 preds2 next2)  =  do
         let eq_offs = offs1 == offs2
         let eq_hvars = hvars1 == hvars2
         let eq_preds = preds1 == preds2
         eq_next <- next1 ~=~ next2
         return $ eq_offs && eq_hvars && eq_preds && eq_next

    (BTtau btree1) ~=~ (BTtau btree2)  =
         btree1 ~=~ btree2

    _ ~=~ _  =
         return False


-- ----------------------------------------------------------------------------------------- --
-- Equiv :  INode


instance Equiv INode
  where

    (BNbexpr (wenv1, ivenv1) bexp1) ~=~ (BNbexpr (wenv2, ivenv2) bexp2)  =  do
         let eq_wenv = wenv1  == wenv2
         let eq_ivenv = ivenv1 == ivenv2
         eq_bexp  <- bexp1 ~=~ bexp2
         return $ eq_wenv && eq_ivenv && eq_bexp

    (BNparallel chids1 inodes1) ~=~ (BNparallel chids2 inodes2)  =  do
         let eq_chids = chids1 == chids2
         eq_inodes <- inodes1 ~=~ inodes2
         return $ eq_chids && eq_inodes

    (BNenable inode11 choffs1 inode12) ~=~ (BNenable inode21 choffs2 inode22)  =  do
         eq_inode1 <- inode11 ~=~ inode21
         let eq_choffs = Set.fromList choffs1 == Set.fromList choffs2
         eq_inode2 <- inode12 ~=~ inode22
         return $ eq_inode1 && eq_choffs && eq_inode2

    (BNdisable inode11 inode12) ~=~ (BNdisable inode21 inode22)  =  do
         eq_inode1 <- inode11 ~=~ inode21
         eq_inode2 <- inode12 ~=~ inode22
         return $ eq_inode1 && eq_inode2

    (BNinterrupt inode11 inode12) ~=~ (BNinterrupt inode21 inode22)  =  do
         eq_inode1 <- inode11 ~=~ inode21
         eq_inode2 <- inode12 ~=~ inode22
         return $ eq_inode1 && eq_inode2

    (BNhide chids1 inode1) ~=~ (BNhide chids2 inode2)  =  do
         let eq_chids = chids1 == chids2
         eq_inode <- inode1 ~=~ inode2
         return $ eq_chids && eq_inode

    _ ~=~ _  =  return False


-- ----------------------------------------------------------------------------------------- --
-- Equiv :  ValExpr


instance (Variable v) => Equiv (ValExpr v)
  where
    vexp1 ~=~ vexp2  = return $    vexp1 == cstrConst (Cany (sortOf vexp1))
                                || vexp2 == cstrConst (Cany (sortOf vexp2))
                                || vexp1 == vexp2


-- ----------------------------------------------------------------------------------------- --
-- Equiv :  BExpr

instance Equiv BExpr
    where
        b1 ~=~ b2 = TxsDefs.view b1 ~=~ TxsDefs.view b2

instance Equiv BExprView
  where
    (ActionPref (ActOffer offs1 hidvars1 c1) bexp1) ~=~ (ActionPref (ActOffer offs2 hidvars2 c2) bexp2) = do
         eq_bexp <- bexp1 ~=~ bexp2
         return $ offs1 == offs2 && null hidvars1 && null hidvars2 && c1 == c2 && eq_bexp  -- @tretmans: why not hidvars1 == hidvars2?

    (Guard c1 bexp1) ~=~ (Guard c2 bexp2)  =  do
         eq_bexp <- bexp1 ~=~ bexp2
         return $ c1 == c2 && eq_bexp

    (Choice bexps1) ~=~ (Choice bexps2)  =
        bexps1 ~=~ bexps2

    (Parallel chids1 bexps1) ~=~ (Parallel chids2 bexps2)  =  do
         let eq_chids = chids1 == chids2
         eq_bexps <- bexps1 ~=~ bexps2
         return $ eq_chids && eq_bexps

    (Enable bexp11 choffs1 bexp12) ~=~ (Enable bexp21 choffs2 bexp22)  =  do
         eq_bexp1  <- bexp11 ~=~ bexp21
         let eq_choffs = Set.fromList choffs1 == Set.fromList choffs2
         eq_bexp2  <- bexp12 ~=~ bexp22
         return $ eq_bexp1 && eq_choffs && eq_bexp2

    (Disable bexp11 bexp12) ~=~ (Disable bexp21 bexp22)  =  do
         eq_bexp1  <- bexp11 ~=~ bexp21
         eq_bexp2  <- bexp12 ~=~ bexp22
         return $ eq_bexp1 && eq_bexp2

    (Interrupt bexp11 bexp12) ~=~ (Interrupt bexp21 bexp22)  =  do
         eq_bexp1  <- bexp11 ~=~ bexp21
         eq_bexp2  <- bexp12 ~=~ bexp22
         return $ eq_bexp1 && eq_bexp2

    (ProcInst pid1 chids1 vexps1) ~=~ (ProcInst pid2 chids2 vexps2)  =  do
         let eq_pid = pid1 == pid2
             eq_chids = chids1 == chids2
             eq_vexps = vexps1 == vexps2
         return $ eq_pid && eq_chids && eq_vexps

    (Hide chids1 bexp1) ~=~ (Hide chids2 bexp2)  =  do
         let eq_chids = chids1 == chids2
         eq_bexp  <- bexp1 ~=~ bexp2
         return $ eq_chids && eq_bexp

    (ValueEnv ve1 bexp1) ~=~ (ValueEnv ve2 bexp2)  =  do
         let eq_ve = ve1 == ve2
         eq_bexp <- bexp1 ~=~ bexp2
         return $ eq_ve && eq_bexp

    (StAut stid1 ve1 trns1) ~=~ (StAut stid2 ve2 trns2)  =  do
         let eq_stid = stid1 == stid2
         let eq_ve = ve1 == ve2
         let eq_trns = trns1 == trns2
         return $ eq_stid && eq_ve && eq_trns

    _ ~=~ _  = return False

-- | check elem of, with given monad equality
elemMby :: (Monad m) => (a -> a -> m Bool) -> a -> [a] -> m Bool
elemMby _ _ []      =  return False
elemMby eqm e (x:xs)  =  do
     isin <- e `eqm` x
     if  isin
       then return True
       else elemMby eqm e xs

-- | make list unique, with given monad equality, in monad
nubMby :: (Monad m) => (a -> a -> m Bool) -> [a] -> m [a]
nubMby _ []      =  return []
nubMby eqm (x:xs)  =  do
     double <- elemMby eqm x xs
     if  double
       then    nubMby eqm xs
       else do xs' <- nubMby eqm xs
               return $ x:xs'

-- | check tuple element occurence is within occurence list
elemOccurenceMby :: (Monad m) => (a -> a -> m Bool) -> (a,MultiSet.Occur) -> MultiSet.MultiSet a -> m Bool
elemOccurenceMby eqm t = elemOccurenceOLMby t . MultiSet.toOccurList
    where
        -- elemOccurenceOLMby :: (Monad m) => (a,MultiSet.Occur) -> [(a, MultiSet.Occur)] -> m Bool
        elemOccurenceOLMby _ [] = return False
        elemOccurenceOLMby (v,cv) ((x,cx) : xs) = do
            equal <- eqm v x
            if equal 
                then return $ cv == cx                  -- use ordered list, so only one match
                else elemOccurenceOLMby (v,cv) xs

-- | make multiSet unique under given monad equality
msMby :: (Ord a, Monad m) => (a -> a-> m Bool) -> MultiSet.MultiSet a -> m (MultiSet.MultiSet a)
msMby eqm ms = 
        let ol = MultiSet.toOccurList ms in
            MultiSet.fromOccurList <$> olMby ol
    where
        -- | make occurence list unique, with given monad equality
        -- olMby :: (Monad m) => [(a,MultiSet.Occur)] -> m [(a, MultiSet.Occur)]
        olMby []         = return []
        olMby ((v,c):xs) = do
                            (duplicateCount, xs') <- removedDuplicates (eqm v) xs
                            ol' <- olMby xs'
                            return $ (v,c+duplicateCount) : ol'
            where
                -- | removedDuplicates return number of duplicates and list without duplicates
                -- removedDuplicates :: (Monad m) => (a -> m Bool) -> [(a,MultiSet.Occur)] -> m (MultiSet.Occur, [(a, MultiSet.Occur)])
                removedDuplicates _    []           = return (0, [])
                removedDuplicates eqmv (y@(w,cw):ys) = do
                        equal <- eqmv w
                        (cd, ls) <- removedDuplicates eqmv ys
                        if equal 
                            then return (cw+cd, ls)
                            else return (cd, y:ls)