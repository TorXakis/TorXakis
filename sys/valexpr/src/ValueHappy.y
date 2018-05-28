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
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse Value response.
-----------------------------------------------------------------------------
module ValueHappy
( valueParser
, Value(..)
)
where
import ValueAlex (Token(..), valueLexer)
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
    "("           { Topenpar          pos }
    ")"           { Tclosepar         pos }
    ","           { Tcomma            pos }
    name          { Tname             pos  $$ }
    bool          { Tbool             pos  $$ }
    integer       { Tinteger          pos  $$ }
    string        { Tstring           pos  $$ }


%% ----------------------------------------------------------------------------------------- --

-- ----------------------------------------------------------------------------------------- --
-- happy grammar for syntax and attributes

-- https://www.haskell.org/happy/doc/html/sec-sequences.html 
-- The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules; 

            
Values  :: { [Value] }
        : 
            { [] }
        | Value
            { [$1] }
        | Values "," Value 
            { $1 ++ [ $3 ] }

                
Value   :: { Value }
        : bool
            { ValueBool $1 }
        | integer
            { ValueInt $1 }
        | string
            { ValueString $ T.pack (init (tail $1)) }
        | name "(" Values ")"
            { ValueADT (T.pack $1) $3 }

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{
-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- | Data structure for Values.
data  Value = ValueADT    Text [Value]
            | ValueBool   Bool
            | ValueInt    Integer
            | ValueString Text
     deriving (Eq,Ord,Read,Show)

-- | Value Parser.
valueParser :: [Token] -> Value
valueParser = happyValue
}
-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
