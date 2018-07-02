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
-- Module      :  ConstantHappy
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse Value response.
-----------------------------------------------------------------------------
module ConstantHappy
( ParseConstant(..)
, constantParser
)
where
import ConstantAlex (Token(..), constantLexer)
import Data.Text (Text)
import qualified Data.Text as T
}
-- ----------------------------------------------------------------------------------------- --
--  happy preamble

%name      happyConstant       Constant
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

            
Constants :: { [ParseConstant] }
          : 
              { [] }
          | Constant
              { [$1] }
          | Constants "," Constant 
              { $1 ++ [ $3 ] }

                
Constant :: { ParseConstant }
         : bool
             { Pbool $1 }
         | integer
             { Pint $1 }
         | string
             { Pstring $ T.pack (init (tail $1)) }
         | name "(" Constants ")"
             { Pcstr (T.pack $1) $3 }

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{
-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- | Data structure for Parse Constant.
data  ParseConstant = Pbool   Bool
                    | Pint    Integer
                    | Pstring Text
                    | Pcstr   Text [ParseConstant]
     deriving (Eq,Ord,Read,Show)

-- | Constant Value Parser.
constantParser :: [Token] -> ParseConstant
constantParser = happyConstant
}
-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
