{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.SmtLanguage
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Description of the Smt Language and its elements.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
module TorXakis.SmtLanguage
( -- * Smt representation
  SmtString
, fromString
, fromText
, toString
, toText
  -- * SmtString operators
, TorXakis.SmtLanguage.empty
, TorXakis.SmtLanguage.singleton
, TorXakis.SmtLanguage.length
, TorXakis.SmtLanguage.intercalate
, TorXakis.SmtLanguage.concat
, append
  -- * Smt Commands
, smtExit
, smtCheckSat
, smtPush
, smtPop
, smtGetInfo
, smtGetValues
, smtDeclareDatatypes
, smtDeclareDatatype
, smtDeclareConstructor
, smtDeclareField
 -- * Smt Sorts
, smtBoolean
, smtInteger
, smtString
 -- ** Smt Values
, smtTrue
, smtFalse
, smtIntegerLiteral
, smtStringLiteral
, smtTextLiteral
, smtRegExpUnion
, smtRegExpConcat
, smtRegExpOptional
, smtRegExpKleeneStar
, smtRegExpKleeneCross
, smtRegExpLoop
)
where
import           Control.DeepSeq     (NFData)
import           Data.Char           (ord)
import           Data.Data           (Data)
import           Data.List
import           Data.Hashable       (Hashable(hashWithSalt))
import qualified Data.Text as T
import           GHC.Generics        (Generic)
import           Text.Printf

import           TorXakis.Error

-- | The data type that represents content in the Smt language.
-- The content can be output of e.g. a pretty printer or input for an smt solver.
newtype SmtString = SmtString { -- | To Text conversion
                                toText :: T.Text
                              }
    deriving (Eq, Ord, Read, Generic, NFData, Data)

instance Show SmtString where
    show (SmtString x) = T.unpack x

instance Hashable SmtString where
    hashWithSalt s = hashWithSalt s . toText

-- | From Text constructor
fromText :: T.Text -> SmtString
fromText = SmtString

-- | From String constructor
fromString :: String -> SmtString
fromString = SmtString . T.pack

-- | To String conversion
toString :: SmtString -> String
toString = T.unpack . toText

-- | The empty 'SmtString'.
empty :: SmtString
empty = SmtString T.empty

-- | The singleton constructor taking a `Char`.
singleton :: Char -> SmtString
singleton = SmtString . T.singleton

-- | Returns the number of characters in a 'SmtString'. Subject to fusion.
length :: SmtString -> Int
length = T.length . toText

-- |  The intercalate function takes a 'SmtString' and
-- a list of 'SmtString's and concatenates the list after interspersing the first argument between each element of the list.
intercalate :: SmtString -> [SmtString] -> SmtString
intercalate t ls = SmtString (T.intercalate (toText t) (map toText ls))

-- | Concatenate a list of 'SmtString's.
concat :: [SmtString] -> SmtString
concat ls = SmtString (T.concat (map toText ls))

-- | Appends one 'SmtString' to the other by copying both of them into a new 'SmtString'. Subject to fusion.
append :: SmtString -> SmtString -> SmtString
append a b = SmtString (T.append (toText a) (toText b))

-- | Smt Boolean
smtBoolean :: SmtString
smtBoolean = fromString "Bool"

-- | Smt Integer
smtInteger :: SmtString
smtInteger = fromString "Int"

-- | Smt String
smtString :: SmtString
smtString = fromString "String"

-- | Smt Boolean Value True
smtTrue :: SmtString
smtTrue = fromString "true"

-- | Smt Boolean Value False
smtFalse :: SmtString
smtFalse = fromString "false"

-- | integer literal to smt
smtIntegerLiteral :: Integer -> SmtString
smtIntegerLiteral n | n < 0 = TorXakis.SmtLanguage.concat [ fromString "(- " 
                                                          , fromString (show (abs n))
                                                          , singleton ')'
                                                          ]
smtIntegerLiteral n = fromString (show (abs n))

-- | String Literal To Smt
--   Encode Haskell String to SMT.
--
--   According to smt-lib-version 2.5 standard (http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf),
--   quote and escape characters are escaped.
--   
--   Furthermore, prevent CVC4 Parse Error "Extended/unprintable characters are not part of SMT-LIB, and they must be encoded as escape sequences"
smtStringLiteral :: String -> SmtString
smtStringLiteral t = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat ( singleton '"' : Data.List.map charToSmt t ) )
                                                 ( singleton '"' )
    where
        charToSmt :: Char -> SmtString
        charToSmt '"'                              = fromString "\"\""
        charToSmt '\\'                             = fromString "\\\\"
        charToSmt c  | ord c < 32 || ord c >= 127  = fromString $ printf "\\x%02x" (ord c)
        charToSmt c                                = TorXakis.SmtLanguage.singleton c

-- | Text Literal To Smt
--   Encode Haskell Text to SMT.
--
--   According to smt-lib-version 2.5 standard (http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf),
--   quote and escape characters are escaped.
--   
--   Furthermore, prevent CVC4 Parse Error "Extended/unprintable characters are not part of SMT-LIB, and they must be encoded as escape sequences"
smtTextLiteral :: T.Text -> SmtString
smtTextLiteral = smtStringLiteral . T.unpack

-- | The smt Regular Expression Union
smtRegExpUnion :: [SmtString] -> SmtString
smtRegExpUnion = smtListOperator (fromString "re.union") (fromString "re.nostr")

-- | The smt Regular Expression Concatenation
smtRegExpConcat :: [SmtString] -> SmtString
smtRegExpConcat = smtListOperator (fromString "re.++") (fromString "re.nostr")

-- | The smt Regular Expression optional (?)
smtRegExpOptional :: SmtString -> SmtString
smtRegExpOptional = smtUnaryOperator (fromString "re.opt")

-- | The smt Regular Expression Kleene Star (*)
smtRegExpKleeneStar :: SmtString -> SmtString
smtRegExpKleeneStar = smtUnaryOperator (fromString "re.*")

-- | The smt Regular Expression Kleene Cross (+)
smtRegExpKleeneCross :: SmtString -> SmtString
smtRegExpKleeneCross = smtUnaryOperator (fromString "re.+")

-- | The smt Regular Expression Loop
-- a regular expression that contains at least l repetitions of r and at most u repetitions of r.
-- preconditions: lowerbound >= 0
--                when present: upperbound >= lowerbound
smtRegExpLoop :: SmtString     -- ^ regular expression
              -> Integer       -- ^ lower bound
              -> Maybe Integer -- ^ possible upperbound
              -> Either Error SmtString
smtRegExpLoop _ l _        | l < 0 = Left $ Error ("precondition violation: lowerbound (" ++ show l ++ ") is negative.")
smtRegExpLoop _ l (Just u) | u < l = Left $ Error ("precondition violation: upperbound (" ++ show u ++ ") is smaller than lowerbound (" ++ show l ++ ").")
smtRegExpLoop r l Nothing          = Right $ TorXakis.SmtLanguage.concat [ fromString "(re.loop "
                                                                         , r
                                                                         , singleton ' '
                                                                         , smtIntegerLiteral l
                                                                         , singleton ')'
                                                                         ]
smtRegExpLoop r l (Just u)         = Right $ TorXakis.SmtLanguage.concat [ fromString "(re.loop "
                                                                         , r
                                                                         , singleton ' '
                                                                         , smtIntegerLiteral l
                                                                         , singleton ' '
                                                                         , smtIntegerLiteral u
                                                                         , singleton ')'
                                                                         ]

-- | The Smt Exit command
smtExit :: SmtString
smtExit = fromString "(exit)"

-- | The Smt Check Sat command
smtCheckSat :: SmtString
smtCheckSat = fromString "(check-sat)"

-- | The Smt Push command
smtPush :: SmtString
smtPush = fromString "(push 1)"

-- | The Smt Pop command
smtPop :: SmtString
smtPop = fromString "(pop 1)"

-- | The Smt Get Info command on topic
smtGetInfo :: String -- ^ topic
           -> SmtString
smtGetInfo topic = TorXakis.SmtLanguage.concat [ fromString "(get-info : "
                                               , fromString topic
                                               , singleton ')'
                                               ]

-- | The Smt Get Values command for a list of variables
smtGetValues :: [SmtString] -- ^ variables
             -> SmtString
smtGetValues vs = TorXakis.SmtLanguage.concat [ fromString "(get-value ("
                                              , TorXakis.SmtLanguage.intercalate (singleton ' ') vs
                                              , fromString "))"
                                              ]

-- | The Smt Declare Datatypes command with a list of Datatypes
-- Note: data type declarations can be recursive
smtDeclareDatatypes :: [SmtString] -- ^ list of DataType declarations
                    -> SmtString
smtDeclareDatatypes ds = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat (fromString "(declare-datatypes () (" : ds) )
                                                     ( fromString "))" )

-- | An Smt Declare Datatype element
smtDeclareDatatype :: SmtString -- ^ Datatype name
                   -> [SmtString] -- ^ Constructors
                   -> SmtString
smtDeclareDatatype nm cs = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat (singleton '(' : nm : cs) )
                                                       ( singleton ')' )

-- | An Smt Declare Constructor element
smtDeclareConstructor :: SmtString -- ^ Constructor name
                      -> [SmtString] -- ^ Fields
                      -> SmtString
smtDeclareConstructor nm [] = TorXakis.SmtLanguage.concat [ singleton '('
                                                          , nm
                                                          , singleton ')'
                                                          ]
smtDeclareConstructor nm fs = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat (singleton '(' : nm : fs) )
                                                          ( singleton ')' )

-- | An Smt Declare Field element
smtDeclareField :: SmtString -- ^ Field name
                -> SmtString -- ^ Field sort
                -> SmtString
smtDeclareField nm s = TorXakis.SmtLanguage.concat [ singleton '('
                                                   , nm
                                                   , singleton ' '
                                                   , s
                                                   , singleton ')'
                                                   ]

-- | smtUnaryOperator
smtUnaryOperator :: SmtString -> SmtString -> SmtString
smtUnaryOperator op s = TorXakis.SmtLanguage.concat [ singleton '('
                                                    , op
                                                    , singleton ' '
                                                    , s
                                                    , singleton ')'
                                                    ]

-- | smtListOperator
smtListOperator :: SmtString   -- ^ operator
                -> SmtString   -- ^ result for empty lists
                -> [SmtString] -- ^ list of operator arguments
                -> SmtString
smtListOperator _  e []  = e
smtListOperator _  _ [x] = x
smtListOperator op _ ls  = TorXakis.SmtLanguage.concat [ singleton '('
                                                       , TorXakis.SmtLanguage.intercalate (singleton ' ') ( op : ls )
                                                       , singleton ')'
                                                       ]
