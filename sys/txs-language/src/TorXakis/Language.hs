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
  -- * TxsString operators
, TorXakis.Language.empty
, TorXakis.Language.length
, TorXakis.Language.replicate
, intercalate
, TorXakis.Language.concat
, TorXakis.Language.lines
, append
, indent
  -- * TorXakis Elements
  -- ** Comment
, txsCommentLine
, txsNestedComments
, txsCommentStart
, txsCommentEnd
  -- ** Case Sensitivity
, txsCaseSensitive

  -- ** Spacing
, txsSpace
, txsNewLine

  -- ** Predefined Sorts
, txsPredefinedSorts

, txsBoolean
, txsInteger
, txsCharacter
, txsString
, txsRegularExpression

  -- ** Predefined Operators
, txsOperators

, txsOperatorDef
, txsCloseScopeDef
, txsSortDef
, txsOperatorOfSort
, txsSeparatorElements
, txsSeparatorLists
, txsSeparatorConstructors
, txsOpenScopeConstructor
, txsCloseScopeConstructor

, txsOpenScopeValExpr
, txsCloseScopeValExpr

  -- ** Predefined keywords
, txsKeywords
, isTxsKeyword
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
import           Data.Hashable       (Hashable(hashWithSalt))
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import           Text.Regex.TDFA

-- | The data type that represents content in the TorXakis language.
-- The content can be output of e.g. a pretty printer or input for a parser.
newtype TxsString = TxsString { -- | To Text conversion
                                toText :: T.Text
                              }
    deriving (Eq, Ord, Read, Generic, NFData, Data)

instance Hashable TxsString where
    hashWithSalt s = hashWithSalt s . toText

-- | To String conversion
toString :: TxsString -> String
toString = T.unpack . toText

-- | The empty 'TorXakis.TxsString'.
empty :: TxsString
empty = TxsString (T.empty)

-- | Returns the number of characters in a 'TorXakis.TxsString'. Subject to fusion.
length :: TxsString -> Int
length = T.length . toText

-- | Returns a 'TorXakis.TxsString' consisting of the input 'TorXakis.TxsString' repeated the given number of times.
replicate :: Int       -- | ^ number of repeat
          -> TxsString -- | ^ input 'TorXakis.TxsString' that will be repeated
          -> TxsString
replicate n t = TxsString (T.replicate n (toText t))

-- |  The intercalate function takes a 'TorXakis.TxsString' and
-- a list of 'TorXakis.TxsString's and concatenates the list after interspersing the first argument between each element of the list.
intercalate :: TxsString -> [TxsString] -> TxsString
intercalate t ls = TxsString (T.intercalate (toText t) (map toText ls))

-- | Concatenate a list of 'TorXakis.TxsString's.
concat :: [TxsString] -> TxsString
concat ls = TxsString (T.concat (map toText ls))

-- | Breaks a 'TorXakis.TxsString' up into a list of 'TorXakis.TxsString's at newline Characters. The resulting 'TorXakis.TxsString's do not contain newlines.
lines :: TxsString -> [TxsString]
lines t = map TxsString (T.lines (toText t))

-- | Appends one 'TorXakis.TxsString' to the other by copying both of them into a new 'TorXakis.TxsString'. Subject to fusion.
append :: TxsString -> TxsString -> TxsString
append a b = TxsString (T.append (toText a) (toText b))

-- | indentation
indent :: TxsString -> TxsString -> TxsString
indent i t = intercalate (append txsNewLine i) (TorXakis.Language.lines t)

instance Show TxsString where
    show (TxsString x) = T.unpack x

-- | TorXakis Predefined Sorts
txsPredefinedSorts :: [TxsString]
txsPredefinedSorts = [ txsBoolean
                     , txsInteger
                     , txsCharacter
                     , txsString
                     , txsRegularExpression
                     ]

-- | Boolean in TorXakis
txsBoolean :: TxsString
txsBoolean = TxsString (T.pack "Bool")

-- | Integer in TorXakis
txsInteger :: TxsString
txsInteger = TxsString (T.pack "Int")

-- | Character in TorXakis
txsCharacter :: TxsString
txsCharacter = TxsString (T.pack "Char")

-- | String in TorXakis
txsString :: TxsString
txsString = TxsString (T.pack "String")

-- | Regular Expression in TorXakis
txsRegularExpression :: TxsString
txsRegularExpression = TxsString (T.pack "Regex")

-- | Comment till end of Line
txsCommentLine :: TxsString
txsCommentLine = TxsString (T.pack "--")

-- | TorXakis supports nested Comments
txsNestedComments :: Bool
txsNestedComments = True

-- | Start / Open Comment
txsCommentStart :: TxsString
txsCommentStart = TxsString (T.pack "{-")

-- | End / Close Comment
txsCommentEnd :: TxsString
txsCommentEnd = TxsString (T.pack "-}")

-- | TorXakis is case sensitive
txsCaseSensitive :: Bool
txsCaseSensitive = True

-- | A space in TorXakis
txsSpace :: TxsString
txsSpace = TxsString (T.singleton ' ')

-- | A newline in TorXakis
txsNewLine :: TxsString
txsNewLine = TxsString (T.singleton '\n')

-- | Close scope of definition
txsCloseScopeDef :: TxsString
txsCloseScopeDef = TxsString (T.pack "ENDDEF")

-- | Definition of Sort in TorXakis.
txsSortDef :: TxsString
txsSortDef = TxsString (T.pack "TYPEDEF")

-- | operator define in TorXakis.
txsOperatorDef :: TxsString
txsOperatorDef = TxsString (T.pack "::=")       -- TODO: Is txsOpenScopeDef a better name?

-- | operator of Sort in TorXakis.
txsOperatorOfSort :: TxsString
txsOperatorOfSort = TxsString (T.pack "::")

-- | Separator of Elements in TorXakis.
txsSeparatorElements :: TxsString
txsSeparatorElements = TxsString (T.singleton ',')

-- | Separator of Lists in TorXakis.
txsSeparatorLists :: TxsString
txsSeparatorLists = TxsString (T.singleton ';')

-- | Separator of Constructors in ADT in  TorXakis.
txsSeparatorConstructors :: TxsString
txsSeparatorConstructors = TxsString (T.singleton '|')

-- | Open scope of Constructor in TorXakis.
txsOpenScopeConstructor :: TxsString
txsOpenScopeConstructor = TxsString (T.singleton '{')

-- | Close scope of Constructor in TorXakis.
txsCloseScopeConstructor :: TxsString
txsCloseScopeConstructor = TxsString (T.singleton '}')

-- | Open scope of ValExpr in TorXakis.
txsOpenScopeValExpr :: TxsString
txsOpenScopeValExpr = TxsString (T.singleton '(')

-- | Close scope of ValExpr in TorXakis.
txsCloseScopeValExpr :: TxsString
txsCloseScopeValExpr = TxsString (T.singleton ')')

-- | TorXakis Keywords
txsKeywords :: Set.Set TxsString
txsKeywords = Set.fromList $
                  txsPredefinedSorts ++
                  [ txsCloseScopeDef
                  , txsSortDef
                  ] ++
                  map (TxsString . T.pack) [ "CONSTDEF", "FUNCDEF", "PROCDEF", "MODELDEF"
                                           , "CHANDEF", "PURPDEF", "CNECTDEF", "STAUTDEF", "MAPPERDEF" -- Definitions
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
{- TODO: What about
      "toString"
    , "fromString"
    , "toXML"
    , "fromXML"
    , "takeWhile"
    , "takeWhileNot"
    , "dropWhile"
    , "dropWhileNot"
  ? -}

-- | Is 'Data.Text' a torxakis keyword?
isTxsKeyword :: T.Text -> Bool
isTxsKeyword t = TxsString t `Set.member` txsKeywords

-- | Is 'Data.Text' a reserved name/operator?
isTxsReserved :: T.Text -> Bool
isTxsReserved t =      (toText txsCommentLine `T.isPrefixOf` t)
                    || isTxsKeyword t

-- | TorXakis predefined operators
-- TODO: should we name each operator and document the operator (instead of the wiki?)
--       and even explicitly distinguish some operators mapped onto same symbol (e.g. "|" and "->")
-- TODO: most operators can be overloaded for other usages (e.g. a == b for a <> b can be defined),
--       so the operator name is not reserved, only a particular signature is reserved!
txsOperators :: [TxsString]
txsOperators = [ txsOperatorDef
               , txsOperatorOfSort
               , txsSeparatorElements
               , txsSeparatorLists
               , txsSeparatorConstructors
               , txsOpenScopeConstructor
               , txsCloseScopeConstructor
               ]
                ++ map (TxsString . T.pack) [ "[", "]", "(", ")"        -- Process Def, Process instantiation, function instantiation/call
                                            , "#"                       -- Sort
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
-- see [regex-tdfa (latest)](http://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html) or
--     [regex-tdfa (version 1.2.3.1)](http://hackage.haskell.org/package/regex-tdfa-1.2.3.1/docs/Text-Regex-TDFA.html)
--
-- note ^ and $ are line boundaries, 
-- so "\na" and "a\n" matches "^[A-Z_a-z][A-Z_a-z0-9-]*$".
-- we need entire text boundaries!
regexTxsIdentifier :: String
regexTxsIdentifier = "\\`" ++ regexTxsIdentifierHead ++ regexTxsIdentifierTail ++ "*\\'"

-- | Regular expression to which the first/head character of a TorXakis identifier adheres
regexTxsIdentifierHead :: String
regexTxsIdentifierHead = "[A-Za-z_]"

-- | Regular expression to which a character in the tail of a TorXakis identifier adheres
regexTxsIdentifierTail :: String
regexTxsIdentifierTail = "[A-Za-z_0-9-]"

-- | Is 'Data.Text' a TorXakis identifier?
satisfyTxsIdentifier :: T.Text -> Bool
satisfyTxsIdentifier t = (T.unpack t) =~ regexTxsIdentifier

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

-- | Is 'Data.Text' a TorXakis function/operator name?
satisfyTxsFuncOperator :: T.Text -> Bool
satisfyTxsFuncOperator t = (T.unpack t) =~ regexTxsFuncOperator

-- TODO: What about operators like
--      communication ?!
--      others        :#$.~

-- | Implicit function name for constructor
txsNameConstructor :: T.Text -> TxsString
txsNameConstructor nm = TxsString nm

-- | Implicit function name for is-made-by-constructor
txsNameIsConstructor :: T.Text -> TxsString
txsNameIsConstructor nm = append prefixIsConstructor (TxsString nm)

-- | Prefix of is-made-by-constructor
prefixIsConstructor :: TxsString
prefixIsConstructor = TxsString (T.pack "is")

-- | Implicit function Name of field access
-- TODO: add additional parameters for ADT name and constructor name?
--       with additional parameters, one could allow multiple fields with the same name in a single ADT....
txsNameField :: T.Text -> TxsString
txsNameField nm = TxsString nm