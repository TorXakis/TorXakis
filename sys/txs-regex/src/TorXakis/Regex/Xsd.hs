{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.Xsd
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Xsd conversions of regular expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Regex.Xsd
( 
-- * Conversion to Xsd
  toXsd
, fromXsd
)
where
import           Data.Set
import           Data.Text

import           TorXakis.Regex
import           TorXakis.Regex.CharRepr
import           TorXakis.Regex.RegexFromXsdHappy

-- | encode Char to Xsd
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
encodeChar '-'  = "\\-"         -- only needed in charGroup context (i.e. with [ ]), yet always allowed to escape
encodeChar '^'  = "\\^"         -- only needed in charGroup context (i.e. with [ ]), yet always allowed to escape
encodeChar c    = Data.Text.singleton c

-- | transform Regular expression to Xsd Text
toXsd :: Regex -> Text
toXsd = toXsdView . viewCharRepr
    where 
        toXsdView :: CharRepr -> Text
        toXsdView  RegexEmpty                 = Data.Text.pack "()"
        toXsdView (RegexCharLiteral c)        = encodeChar c
        toXsdView (RegexConcat cs)            =    Data.Text.singleton '('
                                                <> intercalate (pack ")(") (Prelude.map toXsdView cs)
                                                <> Data.Text.singleton ')'
        toXsdView (RegexUnion us)             =    intercalate (Data.Text.singleton '|') (Prelude.map toXsdView (Data.Set.toList us))
        toXsdView (RegexLoop r l (Just u))    =    Data.Text.singleton '('
                                                <> toXsdView r
                                                <> Data.Text.singleton ')'
                                                <> Data.Text.singleton '{'
                                                <> pack (show l)
                                                <> Data.Text.singleton ','
                                                <> pack (show u)
                                                <> Data.Text.singleton '}'
        toXsdView (RegexLoop r l Nothing)     =    Data.Text.singleton '('
                                                <> toXsdView r
                                                <> Data.Text.singleton ')'
                                                <> Data.Text.singleton '{'
                                                <> pack (show l)
                                                <> Data.Text.singleton ','
                                                <> Data.Text.singleton '}'
        toXsdView (RegexRange l u)            =    Data.Text.singleton '['
                                                <> encodeChar l
                                                <> Data.Text.singleton '-'
                                                <> encodeChar u
                                                <> Data.Text.singleton ']'
