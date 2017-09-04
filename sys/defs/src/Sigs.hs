{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Sigs
( Sigs (..)
, Sigs.empty
, combine
, uniqueCombine
)
where

import qualified Data.Map        as Map

import           Control.DeepSeq
import qualified Data.Text       as T
import           GHC.Generics    (Generic)

import           ChanId
import           ProcId
import           SortId

import           FuncTable

data Sigs v = Sigs  { chan :: [ChanId]  -- TODO: Map.Map String ChanId
                    , func :: FuncTable v
                    , pro  :: [ProcId]  -- TODO: ProcTable
                    , sort :: Map.Map String SortId
                    } deriving (Show, Generic, NFData)

empty :: Sigs v
empty = Sigs [] FuncTable.empty [] Map.empty

combine :: Sigs v -> Sigs v -> Sigs v
combine l r = Sigs  (chan l ++ chan r)
                    (FuncTable.union (func l) (func r))
                    (pro l ++ pro r)
                    (Map.union (sort l) (sort r))

uniqueCombine :: Sigs v -> Sigs v -> Sigs v
uniqueCombine l r = Sigs
                    (let d = ["duplicate channel " ++ show c | c <- chan l, c `elem` chan r ] in
                        if null d then chan l ++ chan r else error (unlines d) )
                    (let d = ["duplicate function " ++ T.unpack f ++ show s | f <- FuncTable.names (func l), s <- FuncTable.signatures f (func l), FuncTable.member f s (func r)] in
                        if null d then FuncTable.union (func l) (func r) else error (unlines d) )
                    (let d = ["duplicate procedure " ++ show p | p <- pro l, p `elem` pro r ] in
                        if null d then pro l ++ pro r else error (unlines d) )
                    (let d = ["duplicate sort " ++ s | s <- Map.keys (sort l), Map.member s (sort r) ] in
                        if null d then Map.union (sort l) (sort r) else error (unlines d) )
