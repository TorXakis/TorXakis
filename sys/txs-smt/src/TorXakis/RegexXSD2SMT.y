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
-- Module      :  TorXakis.RegexXSD2SMT
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
module TorXakis.RegexXSD2SMT
( xsdToSmt
)
where

import TorXakis.RegexAlex
import TorXakis.SmtLanguage
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

Digits  :: { SmtString }
        : digit
            { fromString $1 }
        | Digits digit
            { append $1 (fromString $2) }

Normal  :: { SmtString }
        -- everything that is included in ~[.\\?*+{}()|[\]]
        -- include CommaChar | DashChar | TopChar | DigitChar | FormatEscChar | NormalChar
        : ","
            { smtStringLiteral "," }
        | "-"
            { smtStringLiteral "-" }
        | "^"
            { smtStringLiteral "^" }
        | digit
            { smtStringLiteral $1 }
        | formatEsc
            { smtStringLiteral $1 }
        | normal
            { smtStringLiteral $1 }

SingleCharNoEsc :: { SmtString }
                -- everything that is included in ~[\\[\]]
                : ","
                    { smtStringLiteral "," }
                | "."
                    { smtStringLiteral "." }
                | "-"
                    { smtStringLiteral "-" }
                | "("
                    { smtStringLiteral "(" }
                | ")"
                    { smtStringLiteral ")" }
                | "{"
                    { smtStringLiteral "{" }
                | "}"
                    { smtStringLiteral "}" }
                | "|"
                    { smtStringLiteral "|" }
                | "^"
                    { smtStringLiteral "^" }
                | digit
                    { smtStringLiteral $1 }
                | quantifier
                    { smtStringLiteral $1 }
                | formatEsc
                    { smtStringLiteral $1 }
                | normal
                    { smtStringLiteral $1 }


SingleCharEscChar   :: { SmtString }
                    -- everything that is included in [\.\\\?\*\+\{\}\(\)\[\]\|\-\^]
                    : "."
                        { smtStringLiteral "." }
                    | "\\"
                        { smtStringLiteral "\\" }
                    | quantifier
                        { smtStringLiteral $1 }
                    | "{"
                        { smtStringLiteral "{" }
                    | "}"
                        { smtStringLiteral "}" }
                    | "("
                        { smtStringLiteral "(" }
                    | ")"
                        { smtStringLiteral ")" }
                    | "["
                        { smtStringLiteral "[" }
                    | "]"
                        { smtStringLiteral "]" }
                    | "|"
                        { smtStringLiteral "|" }
                    | "-"
                        { smtStringLiteral "-" }
                    | "^"
                        { smtStringLiteral "^" }

RegExp  :: { SmtString }
        : Branches
            { smtRegExpUnion $1 }

Branches    :: { [SmtString] }
            : Branches "|" Branch
                { $1 ++ [$3] }
            | Branch
                { [$1] }

Branch  :: { SmtString }
        :
            { smtRegExpConcat [] }
        | NeBranch
            { smtRegExpConcat $1
            }

NeBranch    :: { [SmtString] }
            : NeBranch Piece
                { $1 ++ [$2] }
            | Piece
                { [$1] }

Piece   :: { SmtString }
        : Atom
            { $1 }
        | Atom Quantifier
            { $2 $1 }
        | Atom "{" Quantity "}"
            { uncurry (smtRegExpLoop $1) $3 }

Quantifier  :: { SmtString -> SmtString }
            : quantifier
                { case $1 of
                    "*" -> smtRegExpKleeneStar
                    "+" -> smtRegExpKleeneCross
                    "?" -> smtRegExpOptional
                }

Quantity    :: { (Integer, Maybe Integer) }
            : Digits "," Digits                  -- QuantRange
                { ( read $1, Just (read $3) ) }
            | Digits ","                         -- QuantMin
                { ( read $1, Nothing ) }
            | Digits                             -- QuantExact
                { let b = read $1 in (b,Just b) }

Atom    :: { Text }
        : "(" RegExp ")"                        -- Precedence
            { $2 }
        | Normal
            { "(str.to.re " <> $1 <> ") " }
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
                    { ["(re.range " <> $1 <> " " <> $3 <> ") "] }
                | SingleChar "-"
                    { ["(str.to.re " <> $1 <> ") ",
                       "(str.to.re \"-\") "] }
                | SingleChar
                    { ["(str.to.re " <> $1 <> ") "] }
                --  | charClassEsc

SingleChar  :: { SmtString }
            : SingleCharNoEsc
                { $1 }
            | SingleCharEsc
                { $1 }

SingleCharEsc   :: { SmtString }
                : "\\" formatEsc
                    { "\\" <> $2 }
                | "\\" SingleCharEscChar
                    { if $2=="\\" then "\\\\" else $2 }
-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{

-- | Transcribe regular expression in XSD to SmtLib representation.
xsdToSmt :: Text -> SmtString
xsdToSmt = regexSMTParser . regexLexer . T.unpack

-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
}
