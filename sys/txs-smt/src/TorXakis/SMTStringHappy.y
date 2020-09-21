{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell preamble

{
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.SMTStringHappy
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse SMT String.
-----------------------------------------------------------------------------
module TorXakis.SMTStringHappy
( smtStringParser
)
where

import TorXakis.SMTStringAlex (Token(..), smtStringLexer)

import Numeric
import Data.Char
import Data.Tuple
}

-- ----------------------------------------------------------------------------------------- --
--  happy preamble

%name happySmtString       StringValue
%tokentype { Token }
%error { parseError }

-- ----------------------------------------------------------------------------------------- --
-- tokens

%token
    "\""           { Tquotes            pos }
    escSequence    { TescSequence       pos  $$ }
    escUSequence   { TescUSequence      pos  $$ }
    escXSequence   { TescXSequence      pos  $$ }
    char           { Tchar              pos  $$ }

%% ----------------------------------------------------------------------------------------- --

-- ----------------------------------------------------------------------------------------- --
-- happy grammar for SMT string according to smtlib 2.5 standard

StringValue :: { String }
            :
                {
                    ""
                }
            |   CharValue StringValue
                {
                    $1 ++ $2
                }

CharValue   :: { String }
            : escSequence
                {
                    case (tail $1) of
                        {   "\\"    -> "\\"
                        ;   "a"     -> "\a"
                        ;   "b"     -> "\b"
                        ;   "e"     -> "\x1B"
                        ;   "f"     -> "\f"
                        ;   "n"     -> "\n"
                        ;   "r"     -> "\r"
                        ;   "t"     -> "\t"
                        ;   "v"     -> "\v"
                        }
                }
            | escUSequence
                {
                    [chr (fst (head (readHex (drop 3 (init $1)))))]
                }
            | escXSequence
                {
                    [chr (fst (head (readHex (drop 2 $1))))]
                }
            | "\"" "\""
                {
                    "\""
                }
            | char
                {
                    $1
                }

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{
-- ----------------------------------------------------------------------------------------- --
-- error handling

parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- | Smt String Parser.
smtStringParser :: [Token] -> String
smtStringParser = happySmtString
}
-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble