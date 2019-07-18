{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MapDebug
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module MapDebug (
getOrMsg,
getOrError,
getAllOrMsg,
getAllOrError
) where

import qualified Data.List as List
import qualified Data.Map as Map
import ConcatEither

getOrMsg :: Ord a => Map.Map a b -> (a -> String) -> a -> Either String b
getOrMsg m f k =
    case m Map.!? k of
      Just v -> Right v
      Nothing -> Left ("Could not find " ++ f k ++ "; maybe you meant " ++ List.intercalate " / " (map f (Map.keys m)))
-- getOrMsg

getOrError :: Ord a => Map.Map a b -> (a -> String) -> a -> b
getOrError m f k =
    case getOrMsg m f k of
      Left msg -> error msg
      Right v -> v
-- getOrError

getAllOrMsg :: Ord a => Map.Map a b -> (a -> String) -> [a] -> Either [String] [b]
getAllOrMsg m f = concatEither . map (getOrMsg m f)

getAllOrError :: Ord a => Map.Map a b -> (a -> String) -> [a] -> [b]
getAllOrError m f ks =
    case getAllOrMsg m f ks of
      Left msgs -> error (List.intercalate "\n" msgs)
      Right vs -> vs
-- getAllOrError


