{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Language
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Description of the TorXakis Language and its elements.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
module TorXakis.Language
( -- * TorXakis representation
  TxsString (..)
, toString
  -- * TorXakis Elements
  -- ** Comment
, txsCommentLine
, txsNestedComments
, txsCommentStart
, txsCommentEnd
 -- ** Case Sensitivity
, txsCaseSensitive
 -- ** Predefined Keywords and Operators
, txsPredefinedSorts
, txsKeywords
, isTxsKeyword
, txsOperators
, isTxsReserved
 -- ** Identifiers
, regexTxsIdentifier
, regexTxsIdentifierHead
, regexTxsIdentifierTail
, satisfyTxsIdentifier
, satisfyTxsIdentifierHead
, satisfyTxsIdentifierTail
 -- ** TorXakis Function / Operator names
, regexTxsFuncOperator
, satisfyTxsFuncOperator
 -- * Implicit TorXakis Elements
, txsNameConstructor
, txsNameIsConstructor
, prefixIsConstructor
, txsNameField
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.List
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import           Text.Regex.TDFA

-- | The data type that represents content in the TorXakis language.
-- The content can be output of e.g. a pretty printer or input for a parser.
newtype TxsString = TxsString { toText :: T.Text }
    deriving (Eq, Ord, Read, Generic, NFData, Data)

-- | To String conversion
toString :: TxsString -> String
toString = T.unpack . toText

instance Show TxsString where
    show (TxsString x) = T.unpack x

-- | Comment till end of Line
txsCommentLine :: TxsString
txsCommentLine = TxsString (T.pack "--")

-- | TorXakis is case sensitive
txsCaseSensitive :: Bool
txsCaseSensitive = True

-- | TorXakis supports nested Comments
txsNestedComments :: Bool
txsNestedComments = True

-- | Start / Open Comment
txsCommentStart :: TxsString
txsCommentStart = TxsString (T.pack "{-")

-- | End / Close Comment
txsCommentEnd :: TxsString
txsCommentEnd = TxsString (T.pack "-}")

-- | TorXakis Predefined Sorts
txsPredefinedSorts :: [TxsString]
txsPredefinedSorts = map (TxsString . T.pack) [ "Int", "Bool", "Char", "String", "Regex" ]

-- | TorXakis Keywords
txsKeywords :: Set.Set TxsString
txsKeywords = Set.fromList $
                  txsPredefinedSorts ++
                  map (TxsString . T.pack) [ "TYPEDEF", "CONSTDEF", "FUNCDEF", "PROCDEF", "MODELDEF"
                                           , "CHANDEF", "PURPDEF", "CNECTDEF", "STAUTDEF", "MAPPERDEF"
                                           , "ENDDEF"                                           -- Definitions
                                           , "LET", "IN", "NI"
                                           , "IF", "THEN", "ELSE", "FI"                         -- ValExpr
                                           , "STOP", "EXIT"
                                           , "ISTEP", "HIDE", "ACCEPT"                          -- PROCDEF terms
                                           , "QSTEP"
                                           , "GOAL", "HIT", "MISS"                              -- GOALDEF terms
                                           , "CHAN", "OUT", "BEHAVIOUR", "SYNC"                 -- MODELDEF terms
                                           , "CLIENTSOCK", "SERVERSOCK", "ENCODE", "DECODE"
                                           , "HOST", "PORT"                                     -- CNECTDEF terms
                                           , "STATE", "VAR", "INIT", "TRANS"                    -- STAUTDEF terms
                                           ]
{- What about
      "toString"
    , "fromString"
    , "toXML"
    , "fromXML"
    , "takeWhile"
    , "takeWhileNot"
    , "dropWhile"
    , "dropWhileNot"
  ? -}

-- | Is string a torxakis keyword?
isTxsKeyword :: String -> Bool
isTxsKeyword s = TxsString (T.pack s) `Set.member` txsKeywords

-- | Is string a reserved name/operator?
isTxsReserved :: String -> Bool
isTxsReserved s =      (toString txsCommentLine `isPrefixOf` s)
                    || isTxsKeyword s

-- | TorXakis predefined operators
-- TODO: should we name each operator and document the operator (instead of the wiki?)
--       and even explicitly distinguish some operators mapped onto same symbol (e.g. "|" and "->")
-- TODO: most operators can be overloaded for other usages (e.g. a == b for a <> b can be defined),
--       so the operator name is not reserved, only a particular signature is reserved!
txsOperators :: [TxsString]
txsOperators = map (TxsString . T.pack) [ "::="                     -- XDEF
                                        , "[", "]", "(", ")"        -- Process Def, Process instantiation, function instantiation/call
                                        , ",", ";", "#", "::"       -- Sort
                                        , "|", "{", "}"             -- ADT
                                        , ":="                      -- STAUTDEF assignment (TODO: why differs from LET assignment?)
                                        
                                        , "'", "\"", "`"            -- constant char, string and regex
                                                            -- val expression
                                        , "==", "<>", "/\\", "\\/", "\\|/", "=>"    -- boolean
                                        , "+", "-", "*", "/", "%"                   -- integer (TODO: what about "^"?)
                                        , "<", "<=", ">=", ">"                      -- comparison: integer -> bool
                                        , "++"                                      -- concat string
                                        , "="                                       -- Let assignment
                                                            -- process
                                        , "?", "!"                        -- Communication (TODO: what about "[[" and "]]"?) (also uses "|")
                                        , ">->", ">>>"                    -- Sequence
                                        , "[>>", "[><"                    -- Disable /Interrupt
                                        , "##"                            -- Choice
                                        , "||", "|||"                     -- Parallel (TODO: what about "|[" and "]|"?)
                                        , "=>>"                           -- Guard
                                                            -- cnectdef
                                        , "<-", "->"                      -- CNECTDEF (note: STAUTDEF also uses "->" )
                                        ]
-- TODO: what about &#<digit>+;   ?
--       to accept control characters in TorXakis (only defined inside ', " and ` context)

-- | Regular expression to which a TorXakis identifier adheres
-- includes text boundaries:
-- see http://hackage.haskell.org/package/regex-tdfa-1.2.3.1/docs/Text-Regex-TDFA.html
regexTxsIdentifier :: String
regexTxsIdentifier = "\\`" ++ regexTxsIdentifierHead ++ regexTxsIdentifierTail ++ "*\\'"

-- | Regular expression to which the first/head character of a TorXakis identifier adheres
regexTxsIdentifierHead :: String
regexTxsIdentifierHead = "[A-Za-z_]"

-- | Regular expression to which a character in the tail of a TorXakis identifier adheres
regexTxsIdentifierTail :: String
regexTxsIdentifierTail = "[A-Za-z_0-9-]"

-- | Is string a TorXakis identifier?
satisfyTxsIdentifier :: String -> Bool
satisfyTxsIdentifier s = s =~ regexTxsIdentifier

-- | Is character a first/head character of a TorXakis identifier?
satisfyTxsIdentifierHead :: Char -> Bool
satisfyTxsIdentifierHead c = [c] =~ regexTxsIdentifierHead

-- | Is character a character in the tail of a TorXakis identifier?
satisfyTxsIdentifierTail :: Char -> Bool
satisfyTxsIdentifierTail c = [c] =~ regexTxsIdentifierTail

-- | Regular expression to which a TorXakis function / operator adheres
-- includes text boundaries:
-- see http://hackage.haskell.org/package/regex-tdfa-1.2.3.1/docs/Text-Regex-TDFA.html
regexTxsFuncOperator :: String
regexTxsFuncOperator = "\\`" ++ "[A-Za-z_][A-Za-z_0-9-]*|[-+*^=<>%/\\|@&]+" ++ "\\'"

-- | Is string a TorXakis function/operator name?
satisfyTxsFuncOperator :: String -> Bool
satisfyTxsFuncOperator s = s =~ regexTxsFuncOperator

-- TODO: What about operators like
--      communication ?! 
--      others        :#$.~

-- | Implicit function name for constructor
txsNameConstructor :: String -> TxsString
txsNameConstructor nm = TxsString (T.pack nm)

-- | Implicit function name for is-made-by-constructor
txsNameIsConstructor :: String -> TxsString
txsNameIsConstructor nm = TxsString (T.pack (toString prefixIsConstructor ++ nm))

-- | Prefix of is-made-by-constructor
prefixIsConstructor :: TxsString
prefixIsConstructor = TxsString (T.pack "is")

-- | Implicit function Name of field access
-- TODO: add additional parameters for ADT name and constructor name?
--       with additional parameters, one could allow multiple fields with the same name in a single ADT....
txsNameField :: String -> TxsString
txsNameField nm = TxsString (T.pack nm)