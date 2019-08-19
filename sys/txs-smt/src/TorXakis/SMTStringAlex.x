-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2017 TNO and Radboud University
-- See LICENSE at root directory of this repository.
{
-----------------------------------------------------------------------------
-- |
-- Module      :  SMTStringAlex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tokenize SMT String.
-----------------------------------------------------------------------------

module SMTStringAlex
( Token(..)                  -- exporting
, smtStringLexer             -- txsStringLexer :: String -> [Token]
)

where

}

-- ----------------------------------------------------------------------------------------- --
-- alex grammar for SMT string according to smtlib 2.5 standard

%wrapper "posn"

$escchar    = [\\abefnrtv]
$hexdig     = [0-9A-Fa-f]   

@escSequence     = \\ ($escchar | x $hexdig{2})

tokens :-                                          -- Each right-hand side has type
                                                   -- :: AlexPosn -> String -> Token

   \"                                    { tok ( \p _s -> Tquotes p ) }
   @escSequence                          { tok ( \p s -> TescSequence p s) }
   $printable                            { tok ( \p s -> Tchar p s ) }
      
-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s

-- | Data structure for Smt String Tokens.
data  Token  =  Tquotes           AlexPosn
              | TescSequence      AlexPosn  String
              | Tchar             AlexPosn  String
   deriving (Eq,Show)

-- | Smt String lexer.
smtStringLexer :: String -> [Token]
smtStringLexer = alexScanTokens
}

-- ----------------------------------------------------------------------------------------- --