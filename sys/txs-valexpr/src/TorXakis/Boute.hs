{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Boute Division and Modulo
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Define div and mod according to Boute's Euclidean definition, that is,
--  so as to satisfy the formula
--
-- @
--  (for all ((m Int) (n Int))
--    (=> (distinct n 0)
--        (let ((q (div m n)) (r (mod m n)))
--          (and (= m (+ (* n q) r))
--               (<= 0 r (- (abs n) 1))))))
-- @
--
-- Boute, Raymond T. (April 1992). 
--      The Euclidean definition of the functions div and mod. 
--      ACM Transactions on Programming Languages and Systems (TOPLAS) 
--      ACM Press. 14 (2): 127 - 144. doi:10.1145/128861.128862.
-----------------------------------------------------------------------------
module TorXakis.Boute
( TorXakis.Boute.divMod
, TorXakis.Boute.div
, TorXakis.Boute.mod
)
where
-- | operator divMod on the provided integer values.
-- 
-- @
--  (for all ((m Int) (n Int))
--    (=> (distinct n 0)
--        (let ((q (div m n)) (r (mod m n)))
--          (and (= m (+ (* n q) r))
--               (<= 0 r (- (abs n) 1))))))
-- @
divMod :: Integer -> Integer -> (Integer, Integer)
divMod _ 0 = error "divMod: division by zero"
divMod m n = if n > 0 || pm == 0
                then pdm
                else (pd+1, pm-n)
    where pdm@(pd, pm) = Prelude.divMod m n

-- | operator divide on the provided integer values.
div :: Integer -> Integer -> Integer
div m = fst . TorXakis.Boute.divMod m

-- | operator modulo on the provided integer values.
-- 
-- @
--  (for all ((m Int) (n Int)) 
--    (=> (distinct n 0)
--               (<= 0 (mod m n) (- (abs n) 1))))
-- @
mod :: Integer -> Integer -> Integer
mod m = snd . TorXakis.Boute.divMod m

