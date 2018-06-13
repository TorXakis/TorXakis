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
-- Module      :  ConstHappy
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse Value response.
-----------------------------------------------------------------------------
module ConstHappy
( ParseConst(..)
, constParser
)
where
import ConstAlex (Token(..), constLexer)
import Data.Text (Text)
import qualified Data.Text as T
}
-- ----------------------------------------------------------------------------------------- --
--  happy preamble

%name      happyConst       Const
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

            
Consts  :: { [ParseConst] }
        : 
            { [] }
        | Const
            { [$1] }
        | Consts "," Const 
            { $1 ++ [ $3 ] }

                
Const   :: { ParseConst }
        : bool
            { Pbool $1 }
        | integer
            { Pint $1 }
        | string
            { Pstring $ T.pack (init (tail $1)) }
        | name "(" Consts ")"
            { Pcstr (T.pack $1) $3 }

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{
-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- | Data structure for Parse Const.
data  ParseConst = Pbool   Bool
                 | Pint    Integer
                 | Pstring Text
                 | Pcstr   Text [ParseConst]
     deriving (Eq,Ord,Read,Show)

-- | Constant Value Parser.
constParser :: [Token] -> ParseConst
constParser = happyConst
}
-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
