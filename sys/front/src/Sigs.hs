{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sigs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module introduces the cartesian product of Sigs for the TxsParser.
-----------------------------------------------------------------------------
module Sigs
( Sigs (..)
, Sigs.empty
, combine
, uniqueCombine
)
where

import qualified Data.Map as Map

import ChanId
import ProcId
import SortId

import FuncTable

data Sigs v = Sigs  { chan  :: [ChanId]  -- TODO: Map.Map String ChanId
                    , func  :: FuncTable v
                    , pro   :: [ProcId]  -- TODO: ProcTable
                    , sort  :: Map.Map String SortId
                    }
    
empty :: Sigs v
empty = Sigs [] FuncTable.empty [] Map.empty

combine :: Sigs v -> Sigs v -> Sigs v
combine l r = Sigs  (chan l ++ chan r)
                    (FuncTable.union (func l) (func r))
                    (pro l ++ pro r)
                    (Map.union (sort l) (sort r))
                    
uniqueCombine :: Sigs v -> Sigs v -> Sigs v 
uniqueCombine l r = let d = duplicate l r 
                    in if null d  
                        then combine l r
                        else error (unlines d)
                        
duplicate :: Sigs v -> Sigs v -> [String]
duplicate l r = 
    ["duplicate channel " ++ show c | c <- chan l, c `elem` chan r ]
    ++
    ["duplicate function " ++ f ++ show s | f <- FuncTable.names (func l), s <- FuncTable.signatures f (func l), FuncTable.member f s (func r)]
    ++
    ["duplicate procedure " ++ show p | p <- pro l, p `elem` pro r ]
    ++
    ["duplicate sort " ++ s | s <- Map.keys (sort l), Map.member s (sort r) ]