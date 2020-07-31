{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ConcatEither
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module ConcatEither (
concatEither,
concatEitherList
) where

concatEither :: [Either a b] -> Either [a] [b]
concatEither [] = Right []
concatEither (Left x:xs) =
    case concatEither xs of
      Left xs' -> Left (x:xs')
      Right _ -> Left [x]
concatEither (Right x:xs) =
    case concatEither xs of
      Left xs' -> Left xs'
      Right xs' -> Right (x:xs')
-- concatEither

concatEitherList :: [Either [a] [b]] -> Either [a] [b]
concatEitherList [] = Right []
concatEitherList (Left xs:ys) =
    case concatEitherList ys of
      Left ys' -> Left (xs ++ ys')
      Right _ -> Left xs
concatEitherList (Right xs:ys) =
    case concatEitherList ys of
      Left ys' -> Left ys'
      Right ys' -> Right (xs ++ ys')
-- concatEitherList

