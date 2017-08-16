-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2017 TNO and Radboud University
-- See LICENSE at root directory of this repository.
{
-----------------------------------------------------------------------------
-- |
-- Module      :  SMTAlex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tokenize SMT response.
-----------------------------------------------------------------------------
module SMTAlex
( Token(..)                  -- exporting
, smtLexer                   -- txsLexer :: String -> [Token]
)

where

}

-- ----------------------------------------------------------------------------------------- --

%wrapper "posn"
   
$digit              = [0-9]                              -- digits
$alphaCap           = [A-Z]                              -- large alphabetic characters
$alpha              = [a-zA-Z]                           -- alphabetic characters
$symbol             = [ \~ ! @ \$ \% \^ & \* _ \- \+ = \< \> \. \? \/ ]                -- special symbol characters - smt-lib v2.5 page 22
$nameCharStart      = [ $alpha $symbol ]
$nameChar           = [ $alpha $digit $symbol ]

tokens :-                                          -- Each right-hand side has type
                                                   -- :: AlexPosn -> String -> Token

   $white+                   ;                     -- white space

   \(                                    { tok ( \p s -> Topenpar p ) }
   \)                                    { tok ( \p s -> Tclosepar p ) }
   \-                                    { tok ( \p s -> Tminus p ) }
   
   let                                   { tok ( \p s -> Tlet p ) }
   
   true                                  { tok ( \p s -> Tbool p True ) }
   false                                 { tok ( \p s -> Tbool p False ) }
   
   $nameCharStart $nameChar*             { tok ( \p s -> Tname p s ) }
   
   $digit+                               { tok ( \p s -> Tinteger p (read s) ) }
   \" ([^\"]|\"\")* \"                   { tok ( \p s -> Tstring p s ) }
                        -- All characters can occur: either as escapeChar \xdd or as nonEscapeChar

-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s

-- | Data structure for Smt Tokens.
data  Token  =  Tname             AlexPosn  String
              | Tbool             AlexPosn  Bool
              | Tinteger          AlexPosn  Integer
              | Tstring           AlexPosn  String
              | Tlet              AlexPosn
              | Topenpar          AlexPosn
              | Tclosepar         AlexPosn
              | Tminus            AlexPosn
   deriving (Eq,Show)

-- | Smt lexer.
smtLexer :: String -> [Token]
smtLexer = alexScanTokens
}

-- ----------------------------------------------------------------------------------------- --