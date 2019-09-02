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
, prettyPrintFuncContext
  -- * Helper Functions
, separator
  -- dependencies, yet part of interface
, TxsString
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           GHC.Generics        (Generic)
import           Prelude             hiding (concat, length, replicate)

import qualified TorXakis.ContextVar
import           TorXakis.FuncContext
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.FunctionName
import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.RefByIndex
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var
import           TorXakis.VarContext

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
    prettyPrint _ _ (SortADT a)  = (fromText . TorXakis.Name.toText . toName) a

instance PrettyPrint c FieldDef where
    prettyPrint o c fd = concat [ fromText (TorXakis.Name.toText (fieldName fd))
                                , txsSpace
                                , txsOperatorOfSort
                                , txsSpace
                                , prettyPrint o c (TorXakis.Sort.sort fd)
                                ]

instance PrettyPrint c ConstructorDef where
    prettyPrint o c cv = append cName
                                (case (short o, elemsField cv) of
                                    (True, [])   -> empty
                                    (True, x:xs) -> concat [ txsSpace
                                                           , txsOpenScopeConstructor
                                                           , txsSpace
                                                           , fromText (TorXakis.Name.toText (fieldName x))
                                                           , shorten (TorXakis.Sort.sort x) xs
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
              cName = fromText (TorXakis.Name.toText (constructorName cv))
              
              wsField :: TxsString
              wsField = if multiline o then append txsNewLine (replicate (2+ length cName + length txsOpenScopeConstructor) txsSpace)
                                       else txsSpace

              shorten :: Sort -> [FieldDef] -> TxsString
              shorten s []                                 = addSort s
              shorten s (x:xs) | TorXakis.Sort.sort x == s = concat [ txsSeparatorElements
                                                                    , txsSpace
                                                                    , fromText (TorXakis.Name.toText (fieldName x))
                                                                    , shorten s xs
                                                                    ]
              shorten s (x:xs)                             = concat [ addSort s
                                                                    , wsField
                                                                    , txsSeparatorLists
                                                                    , txsSpace
                                                                    , fromText (TorXakis.Name.toText (fieldName x))
                                                                    , shorten (TorXakis.Sort.sort x) xs
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
                                , indent wsConstructor $
                                         intercalate (concat [separator o, txsSeparatorConstructors, txsSpace])
                                                     (map (prettyPrint o c) (elemsConstructor av))
                                , separator o
                                , txsKeywordCloseScopeDef
                                ]
        where defLine :: TxsString
              defLine = concat [ txsKeywordSortDef
                               , txsSpace
                               , fromText (TorXakis.Name.toText (adtName av))
                               , txsSpace
                               , txsOperatorDef
                               ]
              wsConstructor :: TxsString
              wsConstructor = if multiline o then replicate (length defLine) txsSpace
                                             else empty
              offsetFirst :: TxsString
              offsetFirst   = if multiline o then replicate (1+length txsSeparatorConstructors) txsSpace
                                             else txsSpace

-----------------------------------------------------------------------
-- Variable
-------------------------------------------------------------------------
instance PrettyPrint c VarDef where
    prettyPrint o c v = concat [ fromText (TorXakis.Name.toText (name v))
                               , txsSpace
                               , txsOperatorOfSort
                               , txsSpace
                               , prettyPrint o c (TorXakis.Var.sort v)
                               ]

instance PrettyPrint c VarsDecl where
    prettyPrint o c vs = concat [ txsOpenScopeArguments
                                , txsSpace
                                , intercalate sepParam $ map (prettyPrint o c) (toList vs)
                                , close
                                ]
        where sepParam = if multiline o then concat [ txsNewLine, txsSeparatorLists, txsSpace]
                                        else append txsSeparatorLists txsSpace
              close = if multiline o then append txsNewLine txsCloseScopeArguments
                                     else txsCloseScopeArguments

---------------------------------------------------------
-- ValExpr
---------------------------------------------------------
-- | Generic Pretty Printer for all instance of 'TorXakis.FuncContext'.
prettyPrintFuncContext :: FuncContext c => Options -> c -> TxsString
prettyPrintFuncContext o fc =
    concat [ prettyPrintSortContext o fc
           , txsNewLine
           , intercalate txsNewLine (map (prettyPrint o fc) (elemsFunc fc))
           ]

instance VarContext c => PrettyPrint c ValExpression where
  prettyPrint o c = prettyPrint o c . view

instance VarContext c => PrettyPrint c ValExpressionView where
  prettyPrint _ ctx (Vconst c)          = fromText (valueToText ctx c)
  prettyPrint _ ctx (Vvar v)            = case lookupVar (toName v) ctx of
                                            Nothing     -> error ("Pretty Print accessor refers to undefined var " ++ show v)
                                            Just vDef   -> fromText (TorXakis.Name.toText (name vDef))
  prettyPrint o ctx (Vequal a b)        = infixOperator o ctx txsOperatorEqual [a,b]
  prettyPrint o ctx (Vite c tb fb)      = concat [ txsKeywordIf
                                                 , txsSpace
                                                 , indent (replicate (1 + length txsKeywordIf) txsSpace)
                                                          (prettyPrint o ctx c)
                                                 , separator o
                                                 , txsKeywordThen
                                                 , txsSpace
                                                 , indent (replicate (1 + length txsKeywordThen) txsSpace)
                                                          (prettyPrint o ctx tb)
                                                 , separator o
                                                 , txsKeywordElse
                                                 , txsSpace
                                                 , indent (replicate (1 + length txsKeywordElse) txsSpace)
                                                          (prettyPrint o ctx fb)
                                                 , separator o
                                                 , txsKeywordFi
                                                 ]
  prettyPrint o ctx (Vfunc r vs)        = funcInst o ctx (fromText (TorXakis.FunctionName.toText (TorXakis.FuncSignature.funcName (toFuncSignature r)))) vs
  prettyPrint o ctx (Vpredef r vs)      = funcInst o ctx (fromText (TorXakis.FunctionName.toText (TorXakis.FuncSignature.funcName (toFuncSignature r)))) vs
  prettyPrint o ctx (Vnot x)            = funcInst o ctx txsFunctionNot [x]
  prettyPrint o ctx (Vand s)            = infixOperator o ctx txsOperatorAnd (Set.toList s)
  prettyPrint o ctx (Vdivide t n)       = infixOperator o ctx txsOperatorDivide [t,n]
  prettyPrint o ctx (Vmodulo t n)       = infixOperator o ctx txsOperatorModulo [t,n]
  prettyPrint o ctx (Vsum m)            = occuranceOperator o ctx txsOperatorPlus txsOperatorTimes (Map.toList m)
  prettyPrint o ctx (Vproduct m)        = occuranceOperator o ctx txsOperatorTimes txsOperatorPower (Map.toList m)
  prettyPrint o ctx (Vgez v)            = case mkConst ctx (Cint 0) of
                                            Left e     -> error ("prettyPrint: Unable to make zero in GEZ " ++ show e)
                                            Right zero -> infixOperator o ctx txsOperatorLessEqual [zero,v]
  prettyPrint o ctx (Vlength s)         = funcInst o ctx txsFunctionLength [s]
  prettyPrint o ctx (Vat s p)           = funcInst o ctx txsFunctionAt [s,p]
  prettyPrint o ctx (Vconcat vs)        = infixOperator o ctx txsOperatorConcat vs
  prettyPrint o ctx (Vstrinre s r)      = funcInst o ctx txsFunctionStringInRegex [s,r]
  prettyPrint o ctx (Vcstr a c vs)      = case lookupADT (toName a) ctx of
                                            Nothing     -> error ("Pretty Print accessor refers to undefined adt " ++ show a)
                                            Just aDef   -> case lookupConstructor (toName c) aDef of
                                                                Nothing     -> error ("Pretty Print accessor refers to undefined constructor " ++ show c)
                                                                Just cDef   -> funcInst o ctx (txsFuncNameConstructor cDef) vs
  prettyPrint o ctx (Viscstr a c v)     = case lookupADT (toName a) ctx of
                                            Nothing     -> error ("Pretty Print accessor refers to undefined adt " ++ show a)
                                            Just aDef   -> case lookupConstructor (toName c) aDef of
                                                                Nothing     -> error ("Pretty Print accessor refers to undefined constructor " ++ show c)
                                                                Just cDef   -> funcInst o ctx (txsFuncNameIsConstructor cDef) [v]
  prettyPrint o ctx (Vaccess a c p v)   = case lookupADT (toName a) ctx of
                                            Nothing     -> error ("Pretty Print accessor refers to undefined adt " ++ show a)
                                            Just aDef   -> case lookupConstructor (toName c) aDef of
                                                                Nothing     -> error ("Pretty Print accessor refers to undefined constructor " ++ show c)
                                                                Just cDef   -> let field = elemsField cDef !! toIndex p in
                                                                                    funcInst o ctx (txsFuncNameField field) [v]

-- | Helper function since func and predef both are function Instantations in TorXakis
funcInst :: VarContext c => Options -> c -> TxsString -> [ValExpression] -> TxsString
funcInst o ctx txsName vs =
    let offset = if multiline o then concat [ txsNewLine
                                              , replicate (1 + length txsName) txsSpace
                                              ]
                                else empty
      in
        concat [ txsName
               , txsSpace
               , txsOpenScopeArguments
               , txsSpace
               , intercalate (concat [offset, txsSeparatorElements, txsSpace])
                               (map (indent ( replicate (length txsName + length txsSeparatorElements + 2) txsSpace ) . prettyPrint o ctx) vs)
               , offset
               , txsCloseScopeArguments
               ]

infixOperator :: VarContext c => Options -> c -> TxsString -> [ValExpression] -> TxsString
infixOperator o ctx txsName vs =
        -- TODO: assert length txsSpace == 1
        let offset = if multiline o then replicate (length (append txsName txsSpace)) txsSpace
                                    else empty
          in
            concat [ offset
                   , txsOpenScopeValExpr
                   , txsSpace
                   , intercalate (concat [ separator o
                                         , offset
                                         , txsCloseScopeValExpr
                                         , separator o
                                         , txsName
                                         , txsSpace
                                         , txsOpenScopeValExpr
                                         , txsSpace
                                        ])
                                   (map (indent (replicate (length (concat [ txsName, txsSpace, txsOpenScopeValExpr, txsSpace])) txsSpace) . prettyPrint o ctx) vs)
                   , separator o
                   , offset
                   , txsCloseScopeValExpr
                   ]

occuranceOperator :: VarContext c => Options -> c -> TxsString -> TxsString -> [(ValExpression, Integer)] -> TxsString
occuranceOperator o ctx txsOp1 txsOp2 occuranceList =
        let offset1 = if multiline o then replicate (length txsOp1 + 1) txsSpace
                                     else empty
            offset2 = replicate (length txsOp1 + length txsOpenScopeValExpr + 2) txsSpace
          in
            concat [ offset1
                   , txsOpenScopeValExpr
                   , txsSpace
                   , intercalate (concat [ separator o
                                         , offset1
                                         , txsCloseScopeValExpr
                                         , separator o
                                         , txsOp1
                                         , txsSpace
                                         , txsOpenScopeValExpr
                                         , txsSpace
                                        ])
                                   (map (indent offset2 . tupleToText) occuranceList)
                   , separator o
                   , offset1
                   , txsCloseScopeValExpr
                   ]
    where
        tupleToText :: (ValExpression, Integer) -> TxsString
        tupleToText (v,  1) = prettyPrint o ctx v
        tupleToText (v,  p) = case mkConst ctx (Cint p) of
                                   Left e   -> error ("prettyPrint: Unable to make p in tupleToText: " ++ show e)
                                   Right cP -> infixOperator o ctx txsOp2 [v, cP]

instance SortContext c => PrettyPrint c FuncDef where
    prettyPrint o c fd = 
        concat [ txsKeywordFuncDef
               , txsSpace
               , fromText (TorXakis.FunctionName.toText (TorXakis.FuncDef.funcName fd))
               , separator o
               , indent (replicate 3 txsSpace) (prettyPrint o c (paramDefs fd))
               , txsSpace
               , txsOperatorOfSort
               , txsSpace
               , prettyPrint o c (getSort vctx (body fd))
               , separator o
               , txsOperatorDef
               , separator o
               , indent (replicate 3 txsSpace) (prettyPrint o vctx (body fd))
               , separator o
               , txsKeywordCloseScopeDef
               ]
        where
            toVarContext :: [VarDef] -> TorXakis.ContextVar.ContextVar
            toVarContext vs = case addVars vs (TorXakis.ContextVar.fromSortContext c) of
                                       Left e     -> error ("toVarContext is unable to make new context: " ++ show e)
                                       Right nctx -> nctx
            vctx :: TorXakis.ContextVar.ContextVar
            vctx = toVarContext $ toList (paramDefs fd)