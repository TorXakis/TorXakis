{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell preamble
{
module RegexPosixHappy

where

import RegexAlex                        -- importing
                                        -- data Token(..), AlexPosn(..)
                                        -- regexLexer :: String --> [Token]
}
    
-- ----------------------------------------------------------------------------------------- --
--  happy preamble
%name regexPosixParser       RegExp           -- regexPosixParser       :: [Token] -> String

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

Digits      -- :: { String }
                : digit
                    { $1 }
                | Digits digit
                    { $1 ++ $2 }
                    
Normal      -- :: { String }
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
                    { $1 }

SingleCharNoEsc -- :: { String }
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
                    { $1 }


SingleCharEscChar   -- :: { String }
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
                    
RegExp  -- :: { String }
            : RegExp1
                { "^"++ $1 ++ "$" }
                
RegExp1 -- :: {String}
            : Branches
                { $1 }

Branches   -- :: { String }
            : Branches "|" Branch 
                { $1 ++ "|" ++ $3 }
            | Branch
                { $1 }
                
Branch  -- :: { String }
            : {- empty -}
                { "" } 
            | NeBranch
                { $1 }
                
                
NeBranch -- :: { String }
            : NeBranch Piece
               { $1 ++ $2 }
            | Piece
                { $1 }
                
Piece   -- :: { String }
            : Atom
                { $1 }
            | Atom Quantifier
                { $1 ++ $2 }
            | Atom "{" Quantity "}"
                { $1 ++ "{" ++ $3 ++ "}" }
                
Quantifier -- :: { String }
               : quantifier
                    { $1 }
                    
Quantity    -- :: { String }
                : Digits "," Digits                  -- QuantRange
                    { $1 ++ "," ++ $3 }
                | Digits ","                         -- QuantMin
                    { $1 ++ ","}
                | Digits                             -- QuantExact
                    { $1  }

Atom        -- :: { String }
                : "(" RegExp1 ")"                        -- Precedence
                    { "(" ++ $2 ++ ")" }
                | Normal
                    { $1 }
                | NormalSingleCharEsc
                    { $1 }
                | "."                      -- wildcardEsc
                    -- UTF-8 has 256 characters from \x00 till \xFF                    
                    -- exclude \n == \xA
                    --         \r == \xD
                    { "[^\r\n]" }     
             --  | charClassEsc   
                 | CharClass
                    { $1 }
                    
CharClass   --  :: { String }
                 : "[" CharGroup "]"        -- charClassExpr
                    { "[" ++ $2 ++ "]" }

NormalSingleCharEsc -- :: { String } 
                      : "\\" formatEsc
                        { "\\" ++ $2 }
                      | "\\" SingleCharEscChar
                        { "\\" ++ $2 }
                
CharGroup   -- :: { String }
                : PosCharGroup          -- simplified from ( posCharGroup | negCharGroup ) ( DashChar charClassExpr )?
                    { $1 }

PosCharGroup   -- :: { String }                
                : PosCharGroup CharGroupPart
                    { $1 ++ $2 }
                | CharGroupPart
                    { $1 }

CharGroupPart -- :: { [String] }            -- range like a-z -> treated as just 3 characters (thus 'a' '-' 'z'), 
                                            -- correct range interpretation is still done by Posix 
                                            -- known: issue [x-\]] results in other interpretation of CharClass
                  : SingleChar
                    { $1 }
                --  | charClassEsc   

SingleChar    -- :: { String } 
                  : SingleCharNoEsc
                    { $1 }
                  | SingleCharEsc
                    { $1 }
                    
SingleCharEsc -- :: { String } 
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
-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
}

