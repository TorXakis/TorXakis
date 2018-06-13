-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2017 TNO and Radboud University
-- See LICENSE at root directory of this repository.
{
-----------------------------------------------------------------------------
-- |
-- Module      :  ConstAlex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tokenize Value response.
-----------------------------------------------------------------------------
module ConstAlex
( Token(..)
, constLexer
)

where

}

-- ----------------------------------------------------------------------------------------- --

%wrapper "posn"
   
$digit              = [0-9]                              -- digits
$alpha              = [a-zA-Z]                           -- alphabetic characters
$nameCharStart      = [ $alpha ]
$nameChar           = [ $alpha $digit ]

tokens :-                                          -- Each right-hand side has type
                                                   -- :: AlexPosn -> String -> Token

   $white+                   ;                     -- white space

   \(                                    { tok ( \p _s -> Topenpar p ) }
   \)                                    { tok ( \p _s -> Tclosepar p ) }
   \,                                    { tok ( \p _s -> Tcomma p ) }
   
   True                                  { tok ( \p _s -> Tbool p True ) }
   False                                 { tok ( \p _s -> Tbool p False ) }
   
   $nameCharStart $nameChar*             { tok ( \p s -> Tname p s ) }
   
   $digit+                               { tok ( \p s -> Tinteger p (read s) ) }
   \" ([^\"]|\"\")* \"                   { tok ( \p s -> Tstring p s ) }
                        -- All characters can occur: either as escapeChar \xdd or as nonEscapeChar

-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s

-- | Data structure for Value Tokens.
data  Token  =  Tname             AlexPosn  String
              | Tbool             AlexPosn  Bool
              | Tinteger          AlexPosn  Integer
              | Tstring           AlexPosn  String
              | Topenpar          AlexPosn
              | Tclosepar         AlexPosn
              | Tcomma            AlexPosn
   deriving (Eq,Show)

-- | Value lexer.
constLexer :: String -> [Token]
constLexer = alexScanTokens
}

-- ----------------------------------------------------------------------------------------- --