{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MonadAny
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module MonadAny (
anyM,
allM,
binaryOpM,
andM,
orM
) where

import qualified Control.Monad as Monad

anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = Monad.foldM iter False
  where
    iter soFar current =
        if soFar
        then return True
        else f current
-- anyM

allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM f = Monad.foldM iter True
  where
    iter soFar current =
        if soFar
        then f current
        else return False
-- allM

binaryOpM :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
binaryOpM f lhs rhs = do
    x <- lhs
    f x <$> rhs
-- andM

andM :: (Monad m) => m Bool -> m Bool -> m Bool
andM = binaryOpM (&&)

orM :: (Monad m) => m Bool -> m Bool -> m Bool
orM = binaryOpM (||)


