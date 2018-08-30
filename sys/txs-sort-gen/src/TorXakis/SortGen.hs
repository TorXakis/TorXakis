{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Generators associated with the Sort package
-----------------------------------------------------------------------------
module TorXakis.SortGen
( module TorXakis.NameGen
, coverEq
, coverOrd
, coverReadShow
)
where

import TorXakis.NameGen

-- | Tautology of 'Data.Eq' that covers all functions
coverEq :: Eq e => e -> Bool 
coverEq a =
    let aNEQa = a /= a in   -- local variable to prevent hlint issue
        not aNEQa

-- | Tautology of 'Data.Ord' that covers all functions
coverOrd :: Ord o => o -> Bool 
coverOrd a =
    let aGTa = a > a
        aLTa = a < a in     -- local variables to prevent hlint issues
           a <= a
        && a >= a
        && not aGTa
        && not aLTa
        && a == max a a
        && a == min a a
    
-- | Tautology of 'Text.Read' and 'Text.Show' that covers all functions (using 'Data.Eq').
coverReadShow :: (Eq a, Read a, Show a) => a -> Bool
coverReadShow val =
    [val] == (read . show) [val]     -- read . show == id