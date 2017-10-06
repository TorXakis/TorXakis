-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2017 TNO and Radboud University
-- See LICENSE at root directory of this repository.

-- ----------------------------------------------------------------------------------------- --
{
module RegexAlex
( Token(..)                  -- exporting
, AlexPosn(..)               -- Alex type for Position
, regexLexer                 -- regexLexer :: String -> [Token]
)

where

import Data.Text (Text)
import qualified Data.Text as T
  
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

   \,                        { tok ( \p _s -> Tcomma p ) }
   \.                        { tok ( \p _s -> Tdot p ) }
   \-                        { tok ( \p _s -> Tdash p ) }
   \(                        { tok ( \p _s -> Tbracketopen p ) }
   \)                        { tok ( \p _s -> Tbracketclose p ) }
   \{                        { tok ( \p _s -> Tcurlybracketopen p ) }
   \}                        { tok ( \p _s -> Tcurlybracketclose p ) }
   \[                        { tok ( \p _s -> Tsquarebracketopen p ) }
   \]                        { tok ( \p _s -> Tsquarebracketclose p ) }
   \\                        { tok ( \p _s -> Tesc p ) }
   \|                        { tok ( \p _s -> Tunion p ) }
   \^                        { tok ( \p _s -> Ttop p ) }
   $digit                    { tokT ( \p s -> Tdigit p s ) }
   $quantifier               { tokT ( \p s -> Tquantifier p s ) }
   $formatEsc                { tokT ( \p s -> Tformatesc p s ) }
   $normal                   { tokT ( \p s -> Tnormal p s ) }

-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s

tokT f p s = f p (T.pack s)

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
             | Tdigit AlexPosn Text
             | Tquantifier AlexPosn Text
             | Tformatesc AlexPosn Text
             | Tnormal AlexPosn Text
   deriving (Eq, Show)

regexLexer :: String -> [Token]
regexLexer = alexScanTokens
}
-- ----------------------------------------------------------------------------------------- --
