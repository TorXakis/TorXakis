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
-- Module      :  SMTHappy
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse SMT response.
-----------------------------------------------------------------------------
module SMTHappy
( smtParser
, SMTValue(..)
)
where
import SMTAlex (Token(..), smtLexer)
import Data.Text (Text)
import qualified Data.Text as T
import qualified SMTString as SMTString    -- Parse SMT string according to smtlib 2.5 standard
                                        
import qualified Data.Map    as Map
import Data.String.Utils

import Text.Regex.TDFA
}
    
-- ----------------------------------------------------------------------------------------- --
--  happy preamble

%name happySmt       ValueResponse
%tokentype { Token }
%error { parseError }

%attributetype          { MyAttributes a }
%attribute parseVal     { a }
%attribute bind         { Map.Map String SMTValue }

-- ----------------------------------------------------------------------------------------- --
-- tokens

%token
    "-"           { Tminus            pos }
    "("           { Topenpar          pos }
    ")"           { Tclosepar         pos }
    "let"         { Tlet              pos }
    name          { Tname             pos  $$ }
    bool          { Tbool             pos  $$ }
    integer       { Tinteger          pos  $$ }
    string        { Tstring           pos  $$ }


%% ----------------------------------------------------------------------------------------- --

-- ----------------------------------------------------------------------------------------- --
-- happy grammar for syntax and attributes

-- https://www.haskell.org/happy/doc/html/sec-sequences.html 
-- The only reason we used left recursion is that Happy is more efficient at parsing left-recursive rules; 

ValueResponse  -- :: { Map.Map String SMTValue }
            : "(" ValuationPairs ")"
                {
                    $2.bind = Map.empty
                ;   $$ = $2
                }

ValuationPairs -- :: { Map.Map String SMTValue }
            :  ValuationPair
                {  
                    $1.bind = $$.bind
                ;   $$ = Map.singleton (fst $1) (snd $1)
                }
            |  ValuationPairs ValuationPair
                {  
                    $1.bind = $$.bind
                ;   $2.bind = $$.bind
                ;   $$ = Map.insert (fst $2) (snd $2) $1
                }
            
ValuationPair -- :: { (String, SMTValue) }
            : "("  name RuleValue ")"
                {
                    $3.bind = Map.empty
                ;   $$ = ($2, $3)
                }
            
RuleValues  -- :: { [SMTValue] }
            : RuleValue
                {   
                    $1.bind = $$.bind
                ;   $$ = [$1]
                }
            | RuleValues RuleValue 
                {  
                    $1.bind = $$.bind
                ;   $2.bind = $$.bind
                ;   $$ = $1 ++ [ $2 ]
                }

RuleValue  -- :: { SMTValue }
              : RuleExpression
                {  
                    $1.bind = $$.bind
                ;   $$ = $1
                }
              | "(" "let" "(" VarBindings ")" RuleValue ")"
                {
                    $4.bind = $$.bind
                ;   $6.bind = Map.union (Map.fromList $4) $$.bind
                ;   $$ = $6
                }
            
VarBindings  -- :: { [( var, SMTValue)] }
             : VarBinding
                {
                    $1.bind = $$.bind
                ;   $$ = [$1]
                }
             | VarBindings VarBinding
                {
                    $1.bind = $$.bind
                ;   $2.bind = $$.bind
                ;   $$ = $1 ++ [$2]
                }

VarBinding   -- :: { ( name, SMTValue) }
             : "(" name RuleExpression ")"
                {   
                    $3.bind = $$.bind
                ;   $$ = ($2, $3)
                }
                
RuleExpression -- :: { SMTValue }
                : "(" "-" RuleExpression ")"
                  { 
                     $3.bind = $$.bind
                  ;  $$ = case $3 of
                            { SMTInt i -> SMTInt (-1*i)
                            ; _        -> error "SMT unexpected format"
                            }
                  }
                | name
                  {  
                     $$ = case Map.lookup $1 $$.bind of 
                             { Nothing  -> if ($1 =~ cstrRegex) then
                                               SMTConstructor (T.pack $1) []
                                           else
                                               error ("SMT var " ++ $1 ++ " not declared")
                             ; Just val -> val
                             }
                  }
                | bool
                  {
                    $$ = SMTBool $1 
                  }
                | integer
                  {
                    $$ = SMTInt $1
                  }
                | string
                  {
                    $$ = SMTString $ SMTString.stringFromSMT (T.pack (init (tail $1)))
                  }
                | "(" name RuleValues ")"
                  {
                    $3.bind = $$.bind
                  ; $$ = if ($2 =~ cstrRegex) then
                            SMTConstructor (T.pack $2) $3
                         else
                            error ("SMT: " ++ $2 ++ " is not a constructor name.")
                  }

-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble
{
-- ----------------------------------------------------------------------------------------- --
-- error handling
parseError :: [Token] -> a
parseError _ = error "Parse Error"

noerror = ()

-- | Data structure for SMTValues.
data  SMTValue       = SMTConstructor Text [SMTValue]
                     | SMTBool Bool
                     | SMTInt Integer
                     | SMTString Text
     deriving (Eq,Ord,Read,Show)

-- | Smt Parser.
smtParser :: [Token] -> Map.Map String SMTValue
smtParser = happySmt

cstrRegex :: String
cstrRegex = "[A-Z][A-Za-z0-9_$]*"
}
-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble
