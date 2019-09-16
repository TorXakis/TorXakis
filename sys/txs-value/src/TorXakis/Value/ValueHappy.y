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
-- Module      :  ValueHappy
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse Value.
-----------------------------------------------------------------------------
module TorXakis.Value.ValueHappy
( ParseValue(..)
, valueParser
)
where
import TorXakis.Value.ValueAlex (Token(..), valueLexer)
import Data.Text (Text)
import qualified Data.Text as T
}
-- ----------------------------------------------------------------------------------------- --
--  happy preamble

%name      happyValue       Value
%tokentype { Token }
%error     { parseError }

-- ----------------------------------------------------------------------------------------- --
-- tokens

%token
    "("           { Topenpar   pos }
    ")"           { Tclosepar  pos }
    ","           { Tcomma     pos }
    name          { Tname      pos  $$ }
    bool          { Tbool      pos  $$ }
    integer       { Tint       pos  $$ }
--  char          { Tchar      pos  $$ }
    string        { Tstring    pos  $$ }

%% ----------------------------------------------------------------------------------------- --

-- ----------------------------------------------------------------------------------------- --
-- happy grammar for syntax and attributes

-- https://www.haskell.org/happy/doc/html/sec-sequences.html 
-- The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules; 

Arguments :: { [ParseValue] }
            : 
                { [] }
            | "(" ")"
                { [] }
            | "(" Values ")"
                { $2 }

Values :: { [ParseValue] }
          : Value
              { [$1] }
          | Values "," Value 
              { $1 ++ [ $3 ] }

Value :: { ParseValue }
         : bool
             { Pbool $1 }
         | integer
             { Pint $1 }
--       | char
--           { Pchar ( (init .tail) $1 ) }
         | string
             { Pstring ( (init . tail) $1 ) }
         | name Arguments
             { Pcstr (T.pack $1) $2 }

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{
-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- | Data structure for Parse Value.
data  ParseValue = Pbool   Bool
                 | Pint    Integer
--               | Pchar   String
                 | Pstring String
                 | Pcstr   Text [ParseValue]
     deriving (Eq,Ord,Read,Show)

-- | Value Parser.
valueParser :: [Token] -> ParseValue
valueParser = happyValue
}
-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
