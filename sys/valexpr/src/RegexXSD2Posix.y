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
-- Module      :  RegexXSD2Posix
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Transcribe regular expression between different representations.
-- 
-- For more info on
--  * XSD representation see http://www.w3.org/TR/xmlschema11-2/#regexs
--  * Posix reprentation see https://wiki.haskell.org/Regular_expressions#regex-posix
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
  
module RegexXSD2Posix
( xsd2posix )
where

import RegexAlex                        -- importing
                                        -- data Token(..), AlexPosn(..)
                                        -- regexLexer :: String --> [Token]
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
}
    
-- ----------------------------------------------------------------------------------------- --
--  happy preamble
%name regexPosixParser       RegExp

%tokentype { Token }
%error { parseError }

-- ----------------------------------------------------------------------------------------- --
-- tokens

%token
   ","                       { Tcomma                pos }
   "."                       { Tdot                  pos }
   "-"                       { Tdash                 pos }
   "("                       { Tbracketopen          pos }
   ")"                       { Tbracketclose         pos }
   "{"                       { Tcurlybracketopen     pos }
   "}"                       { Tcurlybracketclose    pos }
   "["                       { Tsquarebracketopen    pos }
   "]"                       { Tsquarebracketclose   pos }
   "\\"                      { Tesc                  pos }
   "|"                       { Tunion                pos }
   "^"                       { Ttop                  pos }
   digit                     { Tdigit                pos $$ }
   quantifier                { Tquantifier           pos $$ }
   formatEsc                 { Tformatesc            pos $$ }
   normal                    { Tnormal               pos $$ }

%% ----------------------------------------------------------------------------------------- --

-- ----------------------------------------------------------------------------------------- --
-- happy grammar for syntax and attributes

-- See https://www.haskell.org/happy/doc/html/sec-sequences.html
-- The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules

Digits      :: { Text }
            : digit
                { $1 }
            | Digits digit
                { $1 <> $2 }

                
            -- everything that is included in ~[.\\?*+{}()|[\]]
            -- include CommaChar | DashChar | TopChar | DigitChar | FormatEscChar | NormalChar                                
Normal      :: { Text }
            : ","
                { "," }
            | "-"
                { "-" }
            | "^"
                { "^" }
            | digit
                { $1 }
            | formatEsc
                { $1 }
            | normal
                { $1 }

                -- everything that is included in ~[\\[\]]
SingleCharNoEsc :: { Text }
                : ","
                    { "," }
                | "."
                    { "." }
                | "-"
                    { "-" }
                | "("
                    { "(" }
                | ")"
                    { ")" }
                | "{"
                    { "{" }
                | "}"
                    { "}" }
                | "|"
                    { "|" }
                | "^"
                    { "^" }
                | digit
                    { $1 }
                | quantifier
                    { $1 }
                | formatEsc
                    { $1 }
                | normal
                    { $1 }

                    -- everything that is included in [\.\\\?\*\+\{\}\(\)\[\]\|\-\^]
SingleCharEscChar   :: { Text }
                    : "."
                        { "." }
                    | "\\"
                        { "\\" }
                    | quantifier
                        { $1 }
                    | "{"
                        { "{" }
                    | "}"
                        { "}" }
                    | "("
                        { "(" }
                    | ")"
                        { ")" }
                    | "["
                        { "[" }
                    | "]"
                        { "]" }
                    | "|"
                        { "|" }
                    | "-"
                        { "-" }
                    | "^"
                        { "^" }
                    
RegExp  :: { Text }
        : RegExp1
            { "^"<> $1 <> "$" }
            
RegExp1 :: { Text }
        : Branches
            { $1 }

Branches    :: { Text }
            : Branches "|" Branch 
                { $1 <> "|" <> $3 }
            | Branch
                { $1 }
                
Branch  :: { Text }
        : {- empty -}
            { "" } 
        | NeBranch
            { $1 }
            
                
NeBranch    :: { Text }
            : NeBranch Piece
               { $1 <> $2 }
            | Piece
                { $1 }
                
Piece       :: { Text }
            : Atom
                { $1 }
            | Atom Quantifier
                { $1 <> $2 }
            | Atom "{" Quantity "}"
                { $1 <> "{" <> $3 <> "}" }
                
Quantifier  :: { Text }
            : quantifier
                { $1 }
                    
Quantity    :: { Text }
            : Digits "," Digits                  -- QuantRange
                { $1 <> "," <> $3 }
            | Digits ","                         -- QuantMin
                { $1 <> ","}
            | Digits                             -- QuantExact
                { $1  }

Atom    :: { Text }
        : "(" RegExp1 ")"                        -- Precedence
            { "(" <> $2 <> ")" }
        | Normal
            { $1 }
        | NormalSingleCharEsc
            { $1 }
        | "."                      -- wildcardEsc
            -- UTF-8 has 256 characters from \x00 till \xFF                    
            -- exclude \n == \xA
            --         \r == \xD
            { "[^\r\n]" }     
        | CharClass
            { $1 }
     -- | charClassEsc   
                
CharClass   :: { Text }
            : "[" CharGroup "]"        -- charClassExpr
                { "[" <> $2 <> "]" }

NormalSingleCharEsc :: { Text } 
                    : "\\" formatEsc
                        { "\\" <> $2 }
                    | "\\" SingleCharEscChar
                        { "\\" <> $2 }
                
CharGroup   :: { Text }
            : PosCharGroup          -- simplified from ( posCharGroup | negCharGroup ) ( DashChar charClassExpr )?
                { $1 }

PosCharGroup    :: { Text }                
                : PosCharGroup CharGroupPart
                    { $1 <> $2 }
                | CharGroupPart
                    { $1 }

CharGroupPart   :: { Text }              -- range like a-z -> treated as just 3 characters (thus 'a' '-' 'z'), 
                                           -- correct range interpretation is still done by Posix 
                                           -- known: issue [x-\]] results in other interpretation of CharClass
                : SingleChar
                    { $1 }
                --  | charClassEsc   

SingleChar  :: { Text } 
            : SingleCharNoEsc
                { $1 }
            | SingleCharEsc
                { $1 }
                
SingleCharEsc   :: { Text } 
                : "\\" formatEsc
                    { case $2 of
                        "n"     -> "\n" 
                        "r"     -> "\r"
                        "t"     -> "\t"
                    }
                | "\\" SingleCharEscChar
                    { $2 }                  -- https://en.wikibooks.org/wiki/Regular_Expressions/POSIX-Extended_Regular_Expressions :  backslash escapes are not allowed
-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{

-- | Transcribe regular expression in XSD to Posix representation.
xsd2posix :: Text -> Text
xsd2posix = regexPosixParser . regexLexer . T.unpack

-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
}