{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  UntilFixedPoint
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module UntilFixedPoint (
untilFixedPoint,
untilFixedPointM,
untilCounterOrFixedPoint,
untilCounterOrFixedPointM
) where

untilFixedPoint :: Eq t => (t -> t) -> t -> t
untilFixedPoint f value =
    let newValue = f value in
      if newValue /= value
      then untilFixedPoint f newValue
      else value
-- untilFixedPoint

untilFixedPointM :: (Monad m, Eq t) => (t -> m t) -> t -> m t
untilFixedPointM f value = do
    newValue <- f value
    if newValue /= value
    then untilFixedPointM f newValue
    else return value
-- untilFixedPointM

untilCounterOrFixedPoint :: Eq t => Int -> (t -> t) -> t -> t
untilCounterOrFixedPoint n f value =
    if n > 0
    then let newValue = f value in
           if newValue /= value
           then untilCounterOrFixedPoint (n - 1) f newValue
           else value
    else value
-- untilCounterOrFixedPoint

untilCounterOrFixedPointM :: (Monad m, Eq t) => Int -> (t -> m t) -> t -> m t
untilCounterOrFixedPointM n f value =
    if n > 0
    then do newValue <- f value
            if newValue /= value
            then untilCounterOrFixedPointM (n - 1) f newValue
            else return value
    else return value
-- untilCounterOrFixedPointM

