{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Utils
where

import System.Random
import qualified Data.Set  as Set

-- ----------------------------------------------------------------------------------------- --
-- set difference of two lists

(\\\) :: (Eq t) => [t] -> [t] -> [t]
(\\\) list1 list2  =  [ x | x <- list1, x `notElem` list2 ]

-- ----------------------------------------------------------------------------------------- --
-- monad manipulation

liftP2 :: Monad m => (a, m b) -> m (a, b)
liftP2 (x, my)  =  do  { y <- my; return (x,y) }

liftP3 :: Monad m => (a, b, m c) -> m (a, b, c)
liftP3 (x, y, mz)  =  do  { z <- mz; return (x,y,z) }

-- ----------------------------------------------------------------------------------------- --
-- random ordering of a list

randOrder :: [t] -> IO [t]
randOrder []    =  return []
randOrder list  =  do
     first <- randomRIO ( 0, length list -1 )
     rest  <- randOrder ( take first list ++ drop (first+1) list )
     return ( list!!first : rest )

-- ----------------------------------------------------------------------------------------- --
-- cartesian product

cartProd :: [[t]] -> [[t]]
cartProd =  foldr listProd [[]]
  where
    listProd sq acc  =  [ e:a | e <- sq, a <- acc ]

-- ----------------------------------------------------------------------------------------- --
-- generalized intersection on list of sets

intersections :: Ord a => [Set.Set a] -> Set.Set a
intersections []          =  Set.empty
intersections [s1]        =  s1
intersections (s1:s2:ss)  =  s1 `Set.intersection` intersections (s2:ss)

-- ----------------------------------------------------------------------------------------- --
-- first, second, third of three-tuple

frst :: (a,b,c) -> a
frst (x,_y,_z)  =  x
scnd :: (a,b,c) -> b
scnd (_x,y,_z)  =  y
thrd :: (a,b,c) -> c
thrd (_x,_y,z)  =  z
