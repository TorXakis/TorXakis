{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncTable
-- Copyright   :  (c) TNO and Radboud University
-- License     :  Closed-style (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module introduces the context of Sort (collection of all definitions).
-----------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}
module FuncTable
( Signature (..)
, SignHandler
, Handler
, FuncTable(FuncTable)
, FuncTable.empty
, FuncTable.insert
, FuncTable.union
, FuncTable.member
, FuncTable.signatures
, FuncTable.signHandler
)
where

import qualified Data.Map as Map
import SortId
import TxsDefs

data Signature = Signature  { sortArgs  :: [SortId]
                            , sortRet   :: SortId
                            }
  deriving (Eq, Ord, Read, Show)

type Handler v = [ValExpr v] -> ValExpr v

type SignHandler v = Map.Map Signature (Handler v)

-- | Data structure for Function-like Name and Signature to Handler
newtype FuncTable v = FuncTable { toMap :: Map.Map String (SignHandler v) }

-- | empty
empty :: FuncTable v
empty = FuncTable (Map.empty)

-- | insert
-- Insert a new name, signature and handler in the table. 
-- If the name and signature pair is already present in the table, 
-- the associated handler is replaced with the supplied handler.
insert :: String -> Signature -> Handler v -> FuncTable v -> FuncTable v
insert n s h (toMap -> t) = case Map.lookup n t of
                                Nothing     ->  FuncTable (Map.insert n (Map.singleton s h) t)
                                Just m      ->  FuncTable (Map.insert n (Map.insert s h m)  t)

-- | union
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate name and signature pairs are encountered.
union :: FuncTable v -> FuncTable v -> FuncTable v
union a b = foldl insertSignHandler b (Map.toList (toMap a))
    where insertSignHandler :: FuncTable v -> (String, SignHandler v) -> FuncTable v
          insertSignHandler (toMap -> t) (n,sh) = case Map.lookup n t of
                                Nothing     ->  FuncTable (Map.insert n sh t)
                                Just m      ->  FuncTable (Map.insert n (Map.union sh m) t)


-- | member 
-- Is the name and signature pair a member of the table?
member :: String -> Signature -> FuncTable v -> Bool
member n s (toMap -> t) = case Map.lookup n t of
                                Nothing     ->  False
                                Just m      ->  Map.member s m

-- | signatures
-- Get all signatures associated with the given name in the table. 
signatures :: String -> FuncTable v -> [Signature]
signatures n (toMap -> t) = case Map.lookup n t of
                                Nothing     ->  []
                                Just m      ->  Map.keys m

-- | signHandler
-- Get signHandler associated with the given name in the table. 
signHandler :: String -> FuncTable v -> SignHandler v
signHandler n (toMap -> t) = case Map.lookup n t of
                                Nothing     ->  Map.empty
                                Just m      ->  m
