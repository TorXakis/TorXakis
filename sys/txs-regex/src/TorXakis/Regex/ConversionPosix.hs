{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.ConversionPosix
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Posix conversions of regular expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Regex.ConversionPosix
( 
-- * Conversion to Posix
  toPosix
)
where
import           Data.Monoid            ((<>))
import           Data.Set
import           Data.Text

import           TorXakis.Regex.Regex

-- | encode Char to Posix
-- by escaping posix special/meta characters
encodeChar :: Char -> Text
encodeChar '\\' = "\\\\"
encodeChar '|'  = "\\|"
encodeChar '?'  = "\\?"
encodeChar '+'  = "\\+"
encodeChar '*'  = "\\*"
encodeChar '.'  = "\\."
encodeChar '{'  = "\\{"
encodeChar '}'  = "\\}"
encodeChar '['  = "\\["
encodeChar ']'  = "\\]"
encodeChar '('  = "\\("
encodeChar ')'  = "\\)"
encodeChar '^'  = "\\^"
encodeChar '$'  = "\\$"
encodeChar c    = Data.Text.singleton c

-- | encode String to Posix
-- by escaping posix special/meta characters
encodeString :: Text -> Text
encodeString t | Data.Text.null t = Data.Text.pack "()"
               | otherwise        = Data.Text.concatMap encodeChar t

-- | transform Regular expression to Posix Text
toPosix :: Regex -> Text
toPosix t = pack "\\`" <> toPosixNested t <> pack "\\'"
    where
        toPosixNested :: Regex -> Text
        toPosixNested = toPosixNestedView . view

        toPosixNestedView :: RegexView -> Text
        toPosixNestedView (RegexStringLiteral l)      = encodeString l
        toPosixNestedView (RegexConcat cs)            =    Data.Text.singleton '('
                                                        <> intercalate (pack ")(") (Prelude.map toPosixNested cs)
                                                        <> Data.Text.singleton ')'
        toPosixNestedView (RegexUnion us)             = intercalate (Data.Text.singleton '|') (Prelude.map toPosixNested (Data.Set.toList us))
        toPosixNestedView (RegexLoop r l (Just u))    =    Data.Text.singleton '('
                                                        <> toPosixNested r
                                                        <> Data.Text.singleton ')'
                                                        <> Data.Text.singleton '{'
                                                        <> pack (show l)
                                                        <> Data.Text.singleton ','
                                                        <> pack (show u)
                                                        <> Data.Text.singleton '}'
        toPosixNestedView (RegexLoop r l Nothing)     =    Data.Text.singleton '('
                                                        <> toPosixNested r
                                                        <> Data.Text.singleton ')'
                                                        <> Data.Text.singleton '{'
                                                        <> pack (show l)
                                                        <> Data.Text.singleton ','
                                                        <> Data.Text.singleton '}'
        toPosixNestedView (RegexRange '-' ']')      =      Data.Text.pack "[].-\\-]"
        toPosixNestedView (RegexRange '-' u)        =      Data.Text.pack "[.-"
                                                        <> Data.Text.singleton u
                                                        <> Data.Text.pack "-]"
        toPosixNestedView (RegexRange ']' u)        =      Data.Text.pack "[]^-"
                                                        <> Data.Text.singleton u
                                                        <> Data.Text.singleton ']'
        toPosixNestedView (RegexRange '^' u)        =      Data.Text.pack "[_-"
                                                        <> Data.Text.singleton u
                                                        <> Data.Text.pack "^]"
        toPosixNestedView (RegexRange l ']')        =      Data.Text.pack "[]"
                                                        <> Data.Text.singleton l
                                                        <> Data.Text.pack "-\\]"
        toPosixNestedView (RegexRange l u)          =      Data.Text.singleton '['
                                                        <> Data.Text.singleton l
                                                        <> Data.Text.singleton '-'
                                                        <> Data.Text.singleton u
                                                        <> Data.Text.singleton ']'
