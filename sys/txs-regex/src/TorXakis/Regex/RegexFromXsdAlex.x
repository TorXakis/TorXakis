-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2017 TNO and Radboud University
-- See LICENSE at root directory of this repository.

-- ----------------------------------------------------------------------------------------- --
{
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Regex.RegexFromXsdAlex
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Tokenize regular expressions according to the XSD standard.
-- See http://www.w3.org/TR/xmlschema11-2/#regexs
-----------------------------------------------------------------------------
module TorXakis.Regex.RegexFromXsdAlex
( Token(..)
, regexFromXsdLexer
)

where

}
-- ----------------------------------------------------------------------------------------- --
%wrapper "posn"
   
$digit        = [0-9]                              -- digits
$quantifier   = [\?\*\+]
$formatEsc    = [nrt]
$charEsc      = [\.\\\?\*\+\{\}\(\)\|\[\]\-\^]
        -- UTF8 - extended ascii 256 characters
        -- from \x00 till \xFF
$normal       = [\x00-\xff] # $charEsc
$singleNoEsc  = [\x00-\xff] # [\\\[\]]
                    
                    
tokens :-                                          -- Each right-hand side has type
                                                   -- :: AlexPosn -> String -> Token

   \,                        { tok ( \p _ -> Tcomma p ) }
   \.                        { tok ( \p _ -> Tdot p ) }
   \-                        { tok ( \p _ -> Tdash p ) }
   \(                        { tok ( \p _ -> Tbracketopen p ) }
   \)                        { tok ( \p _ -> Tbracketclose p ) }
   \{                        { tok ( \p _ -> Tcurlybracketopen p ) }
   \}                        { tok ( \p _ -> Tcurlybracketclose p ) }
   \[                        { tok ( \p _ -> Tsquarebracketopen p ) }
   \]                        { tok ( \p _ -> Tsquarebracketclose p ) }
   \\                        { tok ( \p _ -> Tesc p ) }
   \|                        { tok ( \p _ -> Tunion p ) }
   \^                        { tok ( \p _ -> Ttop p ) }
   $digit                    { tok ( \p s -> Tdigit p (fromSingleCharStringToChar s) ) }
   $quantifier               { tok ( \p s -> Tquantifier p (fromSingleCharStringToChar s) ) }
   $formatEsc                { tok ( \p s -> Tformatesc p (fromSingleCharStringToChar s) ) }
   $normal                   { tok ( \p s -> Tnormal p (fromSingleCharStringToChar s) ) }

-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s

-- | Tokens for regular expressions.
data  Token  = Tcomma AlexPosn
             | Tdot AlexPosn
             | Tdash AlexPosn
             | Tbracketopen AlexPosn
             | Tbracketclose AlexPosn
             | Tcurlybracketopen AlexPosn
             | Tcurlybracketclose AlexPosn
             | Tsquarebracketopen AlexPosn
             | Tsquarebracketclose AlexPosn
             | Tesc AlexPosn
             | Tunion AlexPosn
             | Ttop AlexPosn
             | Tdigit AlexPosn Char
             | Tquantifier AlexPosn Char
             | Tformatesc AlexPosn Char
             | Tnormal AlexPosn Char
   deriving (Eq, Show)

-- | Lexer for regular expressions.
regexFromXsdLexer :: String -> [Token]
regexFromXsdLexer = alexScanTokens

fromSingleCharStringToChar :: String -> Char
fromSingleCharStringToChar [c] = c
fromSingleCharStringToChar s   = error ("String is unexpectedly not a single character but '" ++ s ++ "'")

}
-- ----------------------------------------------------------------------------------------- --
