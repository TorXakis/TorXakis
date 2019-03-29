{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.PrettyPrint.TorXakis
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- PrettyPrinter for TorXakis output
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.PrettyPrint
( -- * Pretty Print Options
  Options (..)
  -- * Pretty Print class for TorXakis
, PrettyPrint (..)
, prettyPrintSortContext
  -- * Helper Functions
, separator
  -- dependencies, yet part of interface
, TxsString
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           GHC.Generics        (Generic)
import           Prelude             hiding (concat, length, replicate)
import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext

-- | The data type that represents the options for pretty printing.
data Options = Options { -- | May a definition cover multiple lines?
                         multiline :: Bool
                         -- | Should a definition be short? E.g. by combining of arguments of the same sort.
                       , short     :: Bool
                       }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- | separator based on option
separator :: Options -> TxsString
separator Options{multiline = m} = if m then txsNewLine
                                        else txsSpace

-- | Enables pretty printing in a common way.
class PrettyPrint c a where
    prettyPrint :: Options -> c -> a -> TxsString

---------------------------------------------------------
-- Sort
---------------------------------------------------------
-- | Pretty Printer for 'TorXakis.SortContext'.
prettyPrintSortContext :: SortContext c => Options -> c -> TxsString
prettyPrintSortContext o ctx = intercalate txsNewLine (map (prettyPrint o ctx) (elemsADT ctx))

instance PrettyPrint a Sort where
    prettyPrint _ _ SortBool     = txsBoolean
    prettyPrint _ _ SortInt      = txsInteger
    prettyPrint _ _ SortChar     = txsCharacter
    prettyPrint _ _ SortString   = txsString
    prettyPrint _ _ SortRegex    = txsRegularExpression
    prettyPrint _ _ (SortADT a)  = (TxsString . TorXakis.Name.toText . toName) a

instance PrettyPrint c FieldDef where
    prettyPrint o c fd = concat [ TxsString (TorXakis.Name.toText (fieldName fd))
                                , txsSpace
                                , txsOperatorOfSort
                                , txsSpace
                                , prettyPrint o c (sort fd)
                                ]

instance PrettyPrint c ConstructorDef where
    prettyPrint o c cv = append cName
                                (case (short o, elemsField cv) of
                                    (True, [])   -> empty
                                    (True, x:xs) -> concat [ txsSpace
                                                           , txsOpenScopeConstructor
                                                           , txsSpace
                                                           , TxsString (TorXakis.Name.toText (fieldName x))
                                                           , shorten (sort x) xs
                                                           , wsField
                                                           , txsCloseScopeConstructor
                                                           ]
                                    _            -> concat [ txsSpace
                                                           , txsOpenScopeConstructor
                                                           , txsSpace
                                                           , intercalate (concat [wsField, txsSeparatorLists, txsSpace])
                                                                         (map (prettyPrint o c) (elemsField cv))
                                                           , wsField
                                                           , txsCloseScopeConstructor
                                                           ]
                                )
        where cName :: TxsString
              cName = TxsString (TorXakis.Name.toText (constructorName cv))
              
              wsField :: TxsString
              wsField = if multiline o then append txsNewLine (replicate (1+length cName) txsSpace)
                                       else txsSpace

              shorten :: Sort -> [FieldDef] -> TxsString
              shorten s []                   = addSort s
              shorten s (x:xs) | sort x == s = concat [ txsSeparatorElements
                                                      , txsSpace
                                                      , TxsString (TorXakis.Name.toText (fieldName x))
                                                      , shorten s xs
                                                      ]
              shorten s (x:xs)               = concat [ addSort s
                                                      , wsField
                                                      , txsSeparatorLists
                                                      , txsSpace
                                                      , TxsString (TorXakis.Name.toText (fieldName x))
                                                      , shorten (sort x) xs
                                                      ]

              addSort :: Sort -> TxsString
              addSort s = concat [ txsSpace
                                 , txsOperatorOfSort
                                 , txsSpace
                                 , prettyPrint o c s
                                 ]

instance PrettyPrint c ADTDef where
    prettyPrint o c av = concat [ defLine
                                , offsetFirst
                                , intercalate (concat [wsConstructor, txsSeparatorConstructors, txsSpace])
                                              (map (prettyPrint o c) (elemsConstructor av))
                                , separator o
                                , txsKeywordCloseScopeDef
                                ]
        where defLine :: TxsString
              defLine = concat [ txsKeywordSortDef
                               , txsSpace
                               , TxsString (TorXakis.Name.toText (adtName av))
                               , txsSpace
                               , txsOperatorDef
                               ]
              wsConstructor :: TxsString
              wsConstructor = if multiline o then append txsNewLine (replicate (length defLine) txsSpace)
                                             else txsSpace
              offsetFirst :: TxsString
              offsetFirst   = if multiline o then replicate (1+length txsSeparatorConstructors) txsSpace
                                             else txsSpace

---------------------------------------------------------
-- ValExpr
---------------------------------------------------------