-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2016 TNO and Radboud University
-- See license.txt

-- ----------------------------------------------------------------------------------------- --
{
module RegexAlex
( Token(..)                  -- exporting
, AlexPosn(..)               -- Alex type for Position
, regexLexer                 -- regexLexer :: String -> [Token]
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

   \,                        { tok ( \p s -> Tcomma p ) }
   \.                        { tok ( \p s -> Tdot p ) }
   \-                        { tok ( \p s -> Tdash p ) }
   \(                        { tok ( \p s -> Tbracketopen p ) }
   \)                        { tok ( \p s -> Tbracketclose p ) }
   \{                        { tok ( \p s -> Tcurlybracketopen p ) }
   \}                        { tok ( \p s -> Tcurlybracketclose p ) }
   \[                        { tok ( \p s -> Tsquarebracketopen p ) }
   \]                        { tok ( \p s -> Tsquarebracketclose p ) }
   \\                        { tok ( \p s -> Tesc p ) }
   \|                        { tok ( \p s -> Tunion p ) }
   \^                        { tok ( \p s -> Ttop p ) }
   $digit                    { tok ( \p s -> Tdigit p s ) }
   $quantifier               { tok ( \p s -> Tquantifier p s ) }
   $formatEsc                { tok ( \p s -> Tformatesc p s ) }
   $normal                   { tok ( \p s -> Tnormal p s ) }

-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s

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
             | Tdigit AlexPosn String
             | Tquantifier AlexPosn String
             | Tformatesc AlexPosn String
             | Tnormal AlexPosn String
   deriving (Eq,Show)

regexLexer :: String -> [Token]
regexLexer = alexScanTokens
}
-- ----------------------------------------------------------------------------------------- --
