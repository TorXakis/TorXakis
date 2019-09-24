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
-- Module      :  TorXakis.Regex.RegexFromXsdHappy
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Read Rgex from XSD format into internal representation.
--
-- For more info on
-- XSD representation see http://www.w3.org/TR/xmlschema11-2/#regexs
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Regex.RegexFromXsdHappy
( fromXsd )
where

import           Data.Either
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid

import           TorXakis.Error
import           TorXakis.Regex
import           TorXakis.Regex.RegexFromXsdAlex (Token(..), regexFromXsdLexer)
}

-- ----------------------------------------------------------------------------------------- --
--  happy preamble

%name regexParser       RegExp
%tokentype { Token }
%error { parseError }
%monad { Either Error }

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
                { T.singleton $1 }
            | Digits digit
                { $1 <> T.singleton $2 }

            -- everything that is included in ~[.\\?*+{}()|[\]]
            -- include CommaChar | DashChar | TopChar | DigitChar | FormatEscChar | NormalChar
Normal      :: { Char }
            : ","
                { ',' }
            | "-"
                { '-' }
            | "^"
                { '^' }
            | digit
                { $1 }
            | formatEsc
                { $1 }
            | normal
                { $1 }

                -- everything that is included in ~[\\[\]]
SingleCharNoEsc :: { Char }
                : ","
                    { ',' }
                | "."
                    { '.' }
                | "-"
                    { '-' }
                | "("
                    { '(' }
                | ")"
                    { ')' }
                | "{"
                    { '{' }
                | "}"
                    { '}' }
                | "|"
                    { '|' }
                | "^"
                    { '^' }
                | digit
                    { $1 }
                | quantifier
                    { $1 }
                | formatEsc
                    { $1 }
                | normal
                    { $1 }

                    -- everything that is included in [\.\\\?\*\+\{\}\(\)\[\]\|\-\^]
SingleCharEscChar   :: { Char }
                    : "."
                        { '.' }
                    | "\\"
                        { '\\' }
                    | quantifier
                        { $1 }
                    | "{"
                        { '{' }
                    | "}"
                        { '}' }
                    | "("
                        { '(' }
                    | ")"
                        { ')' }
                    | "["
                        { '[' }
                    | "]"
                        { ']' }
                    | "|"
                        { '|' }
                    | "-"
                        { '-' }
                    | "^"
                        { '^' }

RegExp  :: { Regex }
        : RegExp1
            { $1 }

RegExp1 :: { Regex }
        : Branches
            {% mkRegexUnion $1 }

Branches    :: { [Regex] }
            : Branches "|" Branch
                { ($3:$1) }
            | Branch
                { [$1] }

Branch  :: { Regex }
        : {- empty -}
            { mkRegexEmpty }
        | NeBranch
            { mkRegexConcat $1 }


NeBranch    :: { [Regex] }
            : NeBranch Piece
               { $1 ++ [$2] }
            | Piece
                { [$1] }

Piece       :: { Regex }
            : Atom
                { $1 }
            | Atom Quantifier
                { $2 $1 }
            | Atom "{" Quantity "}"
                {% uncurry (mkRegexLoop $1) $3 }

Quantifier  :: { Regex -> Regex }
            : quantifier
                { case $1 of
                    '?' -> mkRegexOptional
                    '*' -> mkRegexKleeneStar
                    '+' -> mkRegexKleeneCross
                }

Quantity    :: { (Integer, Maybe Integer) }
            : Digits "," Digits                  -- QuantRange
                { ( read (T.unpack $1), Just (read (T.unpack $3)) ) }
            | Digits ","                         -- QuantMin
                { ( read (T.unpack $1), Nothing )}
            | Digits                             -- QuantExact
                { let b = read (T.unpack $1) in (b , Just b)  }

Atom    :: { Regex }
        : "(" RegExp ")"                        -- Precedence
            { $2 }
        | Normal
            {% mkRegexCharLiteral $1 }
        | CharClass
            { $1 }

CharClass   :: { Regex }
            : "[" CharGroup "]"        -- charClassExpr
                { $2 }
            | SingleCharEsc
                {% mkRegexCharLiteral $1 }
            | "."                      -- wildcardEsc
                -- UTF8 - extended ascii 256 characters
                -- from \x00 till \xFF
                -- exclude \n == \xA
                --         \r == \xD
                { mkRegexDot }
            --  | charClassEsc

CharGroup   :: { Regex }
            : PosCharGroup          -- simplified from ( posCharGroup | negCharGroup ) ( DashChar charClassExpr )?
                {% mkRegexUnion $1 }

PosCharGroup    :: { [Regex] }
                : PosCharGroup CharGroupPart
                    { ($2:$1) }
                | CharGroupPart
                    { [$1] }

CharGroupPart   :: { Regex }
                : SingleChar "-" SingleChar
                    {% mkRegexRange $1 $3 }
                | SingleChar "-"
                    {% case partitionEithers [ mkRegexCharLiteral $1
                                             , mkRegexCharLiteral '-'
                                             ] of
                                    ([], cs) -> mkRegexUnion cs
                                    (es, _)  -> error ("mkRegexCharLiteral failed unexpectedly with " ++ show es)
                    }
                | SingleChar
                    {% mkRegexCharLiteral $1 }
                --  | charClassEsc

SingleChar  :: { Char }
            : SingleCharNoEsc
                { $1 }
            | SingleCharEsc
                { $1 }

SingleCharEsc   :: { Char }
                : "\\" formatEsc
                    { case $2 of
                        'n'     -> '\n'
                        'r'     -> '\r'
                        't'     -> '\t'
                    }
                | "\\" SingleCharEscChar
                    { $2 }
-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{

-- | Transcribe regular expression in XSD to internal Regex representation.
fromXsd :: Text -> Either Error Regex
fromXsd = regexParser . regexFromXsdLexer . T.unpack

-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> Either Error a
parseError t = Left $ Error ("Parse Error on token " ++ show t)

noerror = ()

-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
}