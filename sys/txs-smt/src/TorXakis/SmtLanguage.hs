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
, smtAssert
, smtCheckSat
, smtPush
, smtPop
, smtGetInfo
, smtDeclareVariable
, smtGetValues
, smtDeclareFunctions
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
, smtRegexLiteral
)
where
import           Control.DeepSeq     (NFData)
import           Data.Char           (ord)
import           Data.Data           (Data)
import           Data.List
import           Data.Hashable       (Hashable(hashWithSalt))
import qualified Data.Set
import qualified Data.Text
import           GHC.Generics        (Generic)
import           Text.Printf

import           TorXakis.Regex
import           TorXakis.Regex.StringRepr

-- | The data type that represents content in the Smt language.
-- The content can be output of e.g. a pretty printer or input for an smt solver.
newtype SmtString = SmtString { -- | To Text conversion
                                toText :: Data.Text.Text
                              }
    deriving (Eq, Ord, Read, Generic, NFData, Data)

instance Show SmtString where
    show (SmtString x) = Data.Text.unpack x

instance Hashable SmtString where
    hashWithSalt s = hashWithSalt s . toText

-- | From Text constructor
fromText :: Data.Text.Text -> SmtString
fromText = SmtString

-- | From String constructor
fromString :: String -> SmtString
fromString = SmtString . Data.Text.pack

-- | To String conversion
toString :: SmtString -> String
toString = Data.Text.unpack . toText

-- | The empty 'SmtString'.
empty :: SmtString
empty = SmtString Data.Text.empty

-- | The singleton constructor taking a 'Data.Char'.
singleton :: Char -> SmtString
singleton = SmtString . Data.Text.singleton

-- | Returns the number of characters in a 'SmtString'. Subject to fusion.
length :: SmtString -> Int
length = Data.Text.length . toText

-- |  The intercalate function takes a 'SmtString' and
-- a list of 'SmtString's and concatenates the list after interspersing the first argument between each element of the list.
intercalate :: SmtString -> [SmtString] -> SmtString
intercalate t ls = SmtString (Data.Text.intercalate (toText t) (map toText ls))

-- | Concatenate a list of 'SmtString's.
concat :: [SmtString] -> SmtString
concat ls = SmtString (Data.Text.concat (map toText ls))

-- | Appends one 'SmtString' to the other by copying both of them into a new 'SmtString'. Subject to fusion.
append :: SmtString -> SmtString -> SmtString
append a b = SmtString (Data.Text.append (toText a) (toText b))

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
--   Encode Haskell String to SMData.Text.
--
--   According to smt-lib-version 2.5 standard (http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf),
--   quote and escape characters are escaped.
--
--   Furthermore, prevent CVC4 Parse Error "Extended/unprintable characters are not part of SMT-LIB, and they must be encoded as escape sequences"
smtStringLiteral :: String -> SmtString
smtStringLiteral s = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat ( singleton '"' : Data.List.map charToSmt s) )
                                                 ( singleton '"' )

-- | Text Literal To Smt
--   Encode Haskell Text to 'SmtString'.
smtTextLiteral :: Data.Text.Text -> SmtString
smtTextLiteral = smtStringLiteral . Data.Text.unpack


-- | Char Literal To Smt
--   Encode Char String to 'SmtString'.
smtCharLiteral :: Char -> SmtString
smtCharLiteral c = TorXakis.SmtLanguage.concat [ singleton '"'
                                               , charToSmt c
                                               , singleton '"'
                                               ]

-- encode single char (no context)
charToSmt :: Char -> SmtString
charToSmt '"'                              = fromString "\"\""
charToSmt '\\'                             = fromString "\\\\"
charToSmt c  | ord c < 32 || ord c >= 127  = fromString $ printf "\\x%02x" (ord c)
charToSmt c                                = TorXakis.SmtLanguage.singleton c

-- | Regex Literal To Smt
--   Encode 'Regex' to 'SmtString'.
smtRegexLiteral :: Regex -> SmtString
smtRegexLiteral = smtRegexViewLiteral . viewStringRepr
    where
        -- all generated regexes contain brackets
        -- (regexes like re.nostr and re.allchar are not used)
        -- so intercalate with a space is NOT needed in case of concatenation
        smtRegexViewLiteral :: StringRepr -> SmtString
        smtRegexViewLiteral (RegexStringLiteral t)      = TorXakis.SmtLanguage.concat [ fromString "(str.to.re "
                                                                                      , smtTextLiteral t
                                                                                      , singleton ')'
                                                                                      ]
        smtRegexViewLiteral (RegexConcat cs)            = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat ( fromString "(re.++ " : map smtRegexViewLiteral cs ) )
                                                                                      ( singleton ')' )
        smtRegexViewLiteral (RegexUnion us)             =  TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat ( fromString "(re.union " : map smtRegexViewLiteral (Data.Set.toList us) ) )
                                                                                      ( singleton ')' )
        -- optimized translation for not rewritten nested loops (invariant d /=0 and a>1)
        -- for b is Infinite, only constraint when c == 0 => split into two case 0 and {1,d}
        smtRegexViewLiteral (RegexLoop (RegexLoop r a Nothing) 0 _) = TorXakis.SmtLanguage.concat [ fromString "(re.union (str.to.re \"\") (re.loop "
                                                                                                  , smtRegexViewLiteral r
                                                                                                  , singleton ' '
                                                                                                  , smtIntegerLiteral a
                                                                                                  , fromString "))"
                                                                                                  ]
        -- for b is Finite (invariant b/=0) , constraint (b-a) >= a-1 statisfied but c == 0 => split into two case 0 and {1,d}
        smtRegexViewLiteral (RegexLoop (RegexLoop r a (Just b)) 0 Nothing) | b-a >= a-1 = TorXakis.SmtLanguage.concat [ fromString "(re.union (str.to.re \"\") (re.loop "
                                                                                                                      , smtRegexViewLiteral r
                                                                                                                      , singleton ' '
                                                                                                                      , smtIntegerLiteral a
                                                                                                                      , fromString "))"
                                                                                                                      ]
        smtRegexViewLiteral (RegexLoop (RegexLoop r a (Just b)) 0 (Just d)) | b-a >= a-1 = TorXakis.SmtLanguage.concat [ fromString "(re.union (str.to.re \"\") (re.loop "
                                                                                                                       , smtRegexViewLiteral r
                                                                                                                       , singleton ' '
                                                                                                                       , smtIntegerLiteral a
                                                                                                                       , singleton ' '
                                                                                                                       , smtIntegerLiteral (b*d)
                                                                                                                       , fromString "))"
                                                                                                                       ]
        smtRegexViewLiteral (RegexLoop r l Nothing)     = TorXakis.SmtLanguage.concat [ fromString "(re.loop "
                                                                                      , smtRegexViewLiteral r
                                                                                      , singleton ' '
                                                                                      , smtIntegerLiteral l
                                                                                      , singleton ')'
                                                                                      ]
        smtRegexViewLiteral (RegexLoop r l (Just u))    = TorXakis.SmtLanguage.concat [ fromString "(re.loop "
                                                                                      , smtRegexViewLiteral r
                                                                                      , singleton ' '
                                                                                      , smtIntegerLiteral l
                                                                                      , singleton ' '
                                                                                      , smtIntegerLiteral u
                                                                                      , singleton ')'
                                                                                      ]
        smtRegexViewLiteral (RegexRange l u)            = TorXakis.SmtLanguage.concat [ fromString "(re.range "
                                                                                      , smtCharLiteral l
                                                                                      , singleton ' '
                                                                                      , smtCharLiteral u
                                                                                      , singleton ')'
                                                                                      ]

-- | The Smt Exit command
smtExit :: SmtString
smtExit = fromString "(exit)"

-- | The Smt Assert command with assertion
smtAssert :: SmtString -- ^ topic
           -> SmtString
smtAssert boolExpr = TorXakis.SmtLanguage.concat [ fromString "(assert "
                                                 , boolExpr
                                                 , singleton ')'
                                                 ]

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
smtGetInfo topic = TorXakis.SmtLanguage.concat [ fromString "(get-info :"
                                               , fromString topic
                                               , singleton ')'
                                               ]

-- | The Smt Declare Variable command for a variable with a sort
smtDeclareVariable :: SmtString -- ^ variable
                   -> SmtString -- ^ sort
                   -> SmtString
smtDeclareVariable nm srt = TorXakis.SmtLanguage.concat [ fromString "(declare-fun "
                                                        , nm
                                                        , fromString "() "
                                                        , srt
                                                        , TorXakis.SmtLanguage.singleton ')'
                                                        ]

-- | The Smt Get Values command for a list of variables
-- TODO: replace by get-model? (see issue https://github.com/TorXakis/TorXakis/issues/932)
smtGetValues :: [SmtString] -- ^ variables
             -> SmtString
smtGetValues vs = TorXakis.SmtLanguage.concat [ fromString "(get-value ("
                                              , TorXakis.SmtLanguage.intercalate (singleton ' ') vs
                                              , fromString "))"
                                              ]

-- | The Smt Declare Functions command with a list of Tuples containing the function header and body
-- Note: function declarations can be recursive
smtDeclareFunctions :: [(SmtString,SmtString)] -- ^ list of tuples containing the header and body of the functions
                    -> SmtString
smtDeclareFunctions fs = let (hs,bs) = unzip fs in  -- for analyzability added \n
                            TorXakis.SmtLanguage.concat [ fromString "(define-funs-rec (\n"
                                                        , TorXakis.SmtLanguage.intercalate (TorXakis.SmtLanguage.singleton '\n') hs --, TorXakis.SmtLanguage.concat hs
                                                        , fromString "\n)(\n"
                                                        , TorXakis.SmtLanguage.intercalate (TorXakis.SmtLanguage.singleton '\n') bs      -- bodys can be just `1` or `true`, so separation is needed
                                                        , fromString "\n))"
                                                        ]

-- | The Smt Declare Datatypes command with a list of Datatypes
-- Note: data type declarations can be recursive
smtDeclareDatatypes :: [SmtString] -- ^ list of DataType declarations
                    -> SmtString
smtDeclareDatatypes ds = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat (fromString "(declare-datatypes () (" : ds) )
                                                     ( fromString "))" )

-- | An Smt Declare Datatype element
smtDeclareDatatype :: SmtString   -- ^ Datatype name
                   -> [SmtString] -- ^ Constructors
                   -> SmtString
smtDeclareDatatype nm cs = TorXakis.SmtLanguage.append ( TorXakis.SmtLanguage.concat (singleton '\n' : singleton '(' : nm : cs) )  -- \n for analyzability
                                                       ( singleton ')' )

-- | An Smt Declare Constructor element
smtDeclareConstructor :: SmtString   -- ^ Constructor name
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
