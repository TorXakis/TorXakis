-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2017 TNO and Radboud University
-- See LICENSE at root directory of this repository.
{
-----------------------------------------------------------------------------
-- |
-- Module      :  ValueAlex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tokenize string for value.
-----------------------------------------------------------------------------
module TorXakis.Value.ValueAlex
( Token(..)
, valueLexer
)

where

}

-- ----------------------------------------------------------------------------------------- --

%wrapper "posn"
   
$digit              = [0-9]                              -- digits
$nameStartChar      = [A-Za-z_]
$nameChar           = [A-Za-z_0-9\-]

tokens :-                                          -- Each right-hand side has type
                                                   -- :: AlexPosn -> String -> Token

   $white+                   ;                     -- white space

   \(                                               { tok ( \p _s -> Topenpar p ) }
   \)                                               { tok ( \p _s -> Tclosepar p ) }
   \,                                               { tok ( \p _s -> Tcomma p ) }

   True                                             { tok ( \p _s -> Tbool p True ) }
   False                                            { tok ( \p _s -> Tbool p False ) }

   $nameStartChar $nameChar*                        { tok ( \p s -> Tname p s ) }

   \-? $digit+                                      { tok ( \p s -> Tint p (read s) ) }
   \' ( .
      | \\$digit{1,3}
      | \\[A-Z]{2}[A-Z1-4]?
      | \\[\\abfnrtv']
      ) \'                                          { tok ( \p c -> Tchar p (read c) ) }
   \" ([^\"\\]|\\.)* \"                             { tok ( \p s -> Tstring p (read s) ) }

-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s

-- | Data structure for Value Tokens.
data  Token  = Tname        AlexPosn  String
             | Tbool        AlexPosn  Bool
             | Tint         AlexPosn  Integer
             | Tchar        AlexPosn  Char
             | Tstring      AlexPosn  String
             | Topenpar     AlexPosn
             | Tclosepar    AlexPosn
             | Tcomma       AlexPosn
   deriving (Eq,Show)

-- | Value lexer.
valueLexer :: String -> [Token]
valueLexer = alexScanTokens
}

-- ----------------------------------------------------------------------------------------- --