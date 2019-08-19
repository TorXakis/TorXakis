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
-- Module      :  RegexXSD2SMT
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Transcribe Regular Expression from XSD into SMT format.
-- 
-- For more info on
--  * XSD representation see http://www.w3.org/TR/xmlschema11-2/#regexs
--  * SMT reprentation see http://cvc4.cs.stanford.edu/wiki/Strings#Symbolic_Regular_Expression
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}  
module RegexXSD2SMT
( xsd2smt
)
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Foldable

import RegexAlex                        -- importing
                                        -- data Token(..), AlexPosn(..)
                                        -- regexLexer :: String --> [Token]
import SMTString
}
    
-- ----------------------------------------------------------------------------------------- --
--  happy preamble
%name regexSMTParser       RegExp           -- regexSMTParser       :: [Token] -> String

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

Digits  :: { Text }
        : digit
            { $1 }
        | Digits digit
            { $1 <> $2 }
            
Normal  :: { Text }
        -- everything that is included in ~[.\\?*+{}()|[\]]
        -- include CommaChar | DashChar | TopChar | DigitChar | FormatEscChar | NormalChar                
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
            { stringToSMT $1 }

SingleCharNoEsc :: { Text }
                -- everything that is included in ~[\\[\]]
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
                    { stringToSMT $1 }


SingleCharEscChar   :: { Text }
                    -- everything that is included in [\.\\\?\*\+\{\}\(\)\[\]\|\-\^]
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
        : Branches
            { case $1 of
               [x] -> x
               list -> "(re.union " <> fold list <> ") "
            }

Branches    :: { [Text] }
            : Branches "|" Branch 
                { $1 ++ [$3] }
            | Branch
                { [$1] }
                
Branch  :: { Text }
        : 
            { "(str.to.re \"\") " } 
        | NeBranch
            { case $1 of
               [x] -> x
               list -> "(re.++ " <> fold list <> ") "
            }
            
NeBranch    :: { [Text] }
            : NeBranch Piece
                { $1 ++ [$2] }
            | Piece
                { [$1] }
                
Piece   :: { Text }
        : Atom
            { $1 }
        | Atom Quantifier
            { "(" <> $2 <> " " <> $1 <> ") "}
        | Atom "{" Quantity "}"
            { "(re.loop " <> $1 <> $3 <> ") " }
                
Quantifier  :: { Text }
            : quantifier
                { case $1 of
                    "*" -> "re.*"
                    "+" -> "re.+"
                    "?" -> "re.opt"
                }
                    
Quantity    :: { Text }
            : Digits "," Digits                  -- QuantRange
                { $1 <> " " <> $3 }
            | Digits ","                         -- QuantMin
                { $1 }
            | Digits                             -- QuantExact
                { $1 <> " " <> $1 }

Atom    :: { Text }
        : "(" RegExp ")"                        -- Precedence
            { $2 }
        | Normal
            { "(str.to.re \"" <> $1 <> "\") " }
        | CharClass
            { $1 }
            
CharClass   :: { Text }
            : "[" CharGroup "]"        -- charClassExpr
                { $2 }
            | SingleCharEsc
                { "(str.to.re \"" <> $1 <> "\") " }
            | "."                      -- wildcardEsc
                -- UTF8 - extended ascii 256 characters
                -- from \x00 till \xFF                    
                -- exclude \n == \xA
                --         \r == \xD
                { "(re.union (re.range \"\\x00\" \"\\x09\") (re.range \"\\x0B\" \"\\x0C\") (re.range \"\\x0E\" \"\\xFF\") ) " }     
            --  | charClassEsc   
                
CharGroup   :: { Text }
            : PosCharGroup          -- simplified from ( posCharGroup | negCharGroup ) ( DashChar charClassExpr )?
                { case $1 of
                   [x] -> x
                   list -> "(re.union " <> fold list <> ") "
                }

PosCharGroup    :: { [Text] }                
                : PosCharGroup CharGroupPart 
                    { $1 ++ $2 }
                | CharGroupPart
                    { $1 }

CharGroupPart   :: { [Text] } 
                : SingleChar "-" SingleChar
                    { ["(re.range \"" <> $1 <> "\" \"" <> $3 <> "\") "] }
                | SingleChar "-"
                    { ["(str.to.re \"" <> $1 <> "\") ",
                       "(str.to.re \"-\") "] }
                | SingleChar
                    { ["(str.to.re \"" <> $1 <> "\") "] }
                --  | charClassEsc   

SingleChar  :: { Text } 
            : SingleCharNoEsc              
                {  $1 }
            | SingleCharEsc
                { $1 }
            
SingleCharEsc   :: { Text } 
                : "\\" formatEsc
                    { "\\" <> $2 }
                | "\\" SingleCharEscChar
                    { if $2=="\\" then "\\\\" else $2 }
-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{

-- | Transcribe regular expression in XSD to SmtLib representation.
xsd2smt :: Text -> Text
xsd2smt = regexSMTParser . regexLexer . T.unpack

-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
}
