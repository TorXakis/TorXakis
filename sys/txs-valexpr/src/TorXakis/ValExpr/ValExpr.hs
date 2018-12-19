{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ValExpr
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions for Value Expressions.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ValExpr.ValExpr
( -- * Value Expression
  ValExpressionView (..)
, ValExpression (..)
, ValExprView
, ValExpr
  -- * Evaluate
, eval
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.HashMap        as HashMap
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           GHC.Generics        (Generic)


import           TorXakis.FreeVars
import           TorXakis.FuncSignature
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Value
import           TorXakis.VarDef

-- | ValExpressionView: the public view of value expression 'ValExpression'
data ValExpressionView v = Vconst    Value
                         | Vvar      v                                                                        -- Sort is stored to prevent lookup in context
                         -- generic
                         | Vequal    (ValExpression v)
                                     (ValExpression v)
                         | Vite      (ValExpression v)
                                     (ValExpression v)
                                     (ValExpression v)
                         | Vfunc     FuncSignature [ValExpression v]
                         | Vpredef   FuncSignature [ValExpression v]
                         -- Boolean
                         | Vnot      (ValExpression v)
                         | Vand      (Set.Set (ValExpression v))
                         -- Int
                         | Vdivide   (ValExpression v)
                                     (ValExpression v)
                         | Vmodulo   (ValExpression v)
                                     (ValExpression v)
                         | Vsum      (Map.Map (ValExpression v) Integer)
                         | Vproduct  (Map.Map (ValExpression v) Integer)
                         | Vgez      (ValExpression v)
                         -- String
                         | Vlength   (ValExpression v)
                         | Vat       (ValExpression v)
                                     (ValExpression v)
                         | Vconcat   [ValExpression v]
                         -- Regex
                         | Vstrinre  (ValExpression v)
                                     (ValExpression v)
                         -- ADT
                         | Vcstr     (RefByName ADTDef) (RefByName ConstructorDef) [ValExpression v]
                         | Viscstr   (RefByName ADTDef) (RefByName ConstructorDef) (ValExpression v)
                         | Vaccess   (RefByName ADTDef) (RefByName ConstructorDef) Sort Int (ValExpression v)       -- Sort is stored to prevent lookup in context
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | ValExpression: value expression
--
-- 1. User can't directly construct ValExpression (such that invariants will always hold)
--
-- 2. User can still pattern match on ValExpression using 'ValExpressionView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype ValExpression v = ValExpression { -- | View on value expression.
                                          view :: ValExpressionView v
                                        }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

  -- | type synonym ValExpr
type ValExpr = ValExpression MinimalVarDef

-- | type synonym ValExprView
type ValExprView = ValExpressionView MinimalVarDef

-- | Evaluate the provided value expression.
-- Either the Right Constant Value is returned or a (Left) error message.
eval :: Show v => ValExpression v -> Either String Value
eval = evalView . view

evalView :: Show v => ValExpressionView v -> Either String Value
evalView (Vconst v) = Right v
evalView x          = Left $ "Value Expression is not a constant value " ++ show x

-- | SortOf instance
instance VarDef v => HasSort (ValExpression v) where
  getSort = getSort . view

instance VarDef v => HasSort (ValExpressionView v) where
    getSort (Vconst val)                                   = getSort val
    getSort (Vvar v)                                       = getSort v
    getSort  Vequal { }                                    = SortBool
    getSort (Vite _cond vexp1 _vexp2)                      = getSort vexp1
    getSort  Vnot { }                                      = SortBool
    getSort  Vand { }                                      = SortBool
    getSort  Vdivide { }                                   = SortInt
    getSort  Vmodulo { }                                   = SortInt
    getSort  Vsum { }                                      = SortInt
    getSort  Vproduct { }                                  = SortInt
    getSort  Vgez { }                                      = SortBool
    getSort  Vlength { }                                   = SortInt
    getSort  Vat { }                                       = SortString
    getSort  Vconcat { }                                   = SortString
    getSort  Vstrinre { }                                  = SortBool
    getSort (Vcstr a _c _vexps)                            = SortADT a
    getSort  Viscstr { }                                   = SortBool
    getSort (Vaccess _a _c s _p _vexps)                    = s
    getSort (Vfunc fs _vexps)                              = returnSort fs
    getSort (Vpredef fs _vexps)                            = returnSort fs

instance VarDef v => FreeVars ValExpression v where
    freeVars = freeVars . view

instance VarDef v => FreeVars ValExpressionView v where
    freeVars  Vconst{}             = Set.empty
    freeVars (Vvar v)              = Set.singleton v
    freeVars (Vequal v1 v2)        = Set.unions $ map freeVars [v1, v2]
    freeVars (Vite c t f)          = Set.unions $ map freeVars [c, t, f]
    freeVars (Vfunc _ as)          = Set.unions $ map freeVars as
    freeVars (Vpredef _ as)        = Set.unions $ map freeVars as
    freeVars (Vnot v)              = freeVars v
    freeVars (Vand s)              = Set.unions $ map freeVars (Set.toList s)
    freeVars (Vdivide t n)         = Set.unions $ map freeVars [t, n]
    freeVars (Vmodulo t n)         = Set.unions $ map freeVars [t, n]
    freeVars (Vsum m)              = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vproduct m)          = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vgez v)              = freeVars v
    freeVars (Vlength v)           = freeVars v
    freeVars (Vat s p)             = Set.unions $ map freeVars [s, p]
    freeVars (Vconcat vs)          = Set.unions $ map freeVars vs
    freeVars (Vstrinre s r)        = Set.unions $ map freeVars [s, r]
    freeVars (Vcstr _ _ vs)        = Set.unions $ map freeVars vs
    freeVars (Viscstr _ _ v)       = freeVars v
    freeVars (Vaccess _ _ _ _ v)   = freeVars v

-- Pretty Print 
instance (SortContext c, VarDef v) => PrettyPrint c (ValExpression v) where
  prettyPrint o c = prettyPrint o c . view

instance (SortContext c, VarDef v) => PrettyPrint c (ValExpressionView v) where
  prettyPrint _ ctx (Vconst c)          = TxsString (valueToText ctx c)
  prettyPrint _ _   (Vvar v)            = TxsString (TorXakis.Name.toText (getName v))
  prettyPrint o ctx (Vequal a b)        = infixOperator o ctx (T.pack "==") [a,b]
  prettyPrint o ctx (Vite c tb fb)      = TxsString (T.concat [ T.pack "IF "
                                                              , indent (T.pack "   ") (TorXakis.PrettyPrint.TorXakis.toText (prettyPrint o ctx c))
                                                              , separator o
                                                              , T.pack "THEN "
                                                              , indent (T.pack "     ") (TorXakis.PrettyPrint.TorXakis.toText (prettyPrint o ctx tb))
                                                              , separator o
                                                              , T.pack "ELSE "
                                                              , indent (T.pack "     ") (TorXakis.PrettyPrint.TorXakis.toText (prettyPrint o ctx fb))
                                                              , separator o
                                                              , T.pack "FI"
                                                              ])
  prettyPrint o ctx (Vfunc fs vs)       = funcInst o ctx (TorXakis.Name.toText (funcName fs)) vs
  prettyPrint o ctx (Vpredef fs vs)     = funcInst o ctx (TorXakis.Name.toText (funcName fs)) vs
  prettyPrint o ctx (Vnot x)            = funcInst o ctx (T.pack "not") [x]
  prettyPrint o ctx (Vand s)            = infixOperator o ctx (T.pack "/\\") (Set.toList s)
  prettyPrint o ctx (Vdivide t n)       = infixOperator o ctx (T.pack "/") [t,n]
  prettyPrint o ctx (Vmodulo t n)       = infixOperator o ctx (T.pack "%") [t,n]
  prettyPrint o ctx (Vsum m)            = occuranceOperator o ctx (T.pack "+") (T.pack "*") (Map.toList m)
  prettyPrint o ctx (Vproduct m)        = occuranceOperator o ctx (T.pack "*") (T.pack "^") (Map.toList m)
  prettyPrint o ctx (Vgez v)            = infixOperator o ctx (T.pack "<=") [ValExpression (Vconst (Cint 0)),v]
  prettyPrint o ctx (Vlength s)         = funcInst o ctx (T.pack "len") [s]
  prettyPrint o ctx (Vat s p)           = funcInst o ctx (T.pack "at") [s,p]
  prettyPrint o ctx (Vconcat vs)        = infixOperator o ctx (T.pack "++") vs
  prettyPrint o ctx (Vstrinre s r)      = funcInst o ctx (T.pack "strinre") [s,r]
  prettyPrint o ctx (Vcstr _ c vs)      = funcInst o ctx (TorXakis.Name.toText (TorXakis.Name.toName c)) vs
  prettyPrint o ctx (Viscstr _ c v)     = funcInst o ctx (T.append (T.pack "is") (TorXakis.Name.toText (TorXakis.Name.toName c))) [v]
  prettyPrint o ctx (Vaccess a c _ p v) = case HashMap.lookup a (adtDefs ctx) of
                                            Nothing     -> error ("Pretty Print accessor refers to undefined adt " ++ show a)
                                            Just aDef   -> case HashMap.lookup c (constructors (viewADTDef aDef)) of
                                                                Nothing     -> error ("Pretty Print accessor refers to undefined constructor " ++ show c)
                                                                Just cDef   -> let field = fields (viewConstructorDef cDef) !! p in
                                                                                    funcInst o ctx (TorXakis.Name.toText (fieldName field)) [v]

-- | Helper function since func and predef both are function Instantations in TorXakis
funcInst :: (SortContext c, VarDef v) => Options -> c -> T.Text -> [ValExpression v] -> TxsString
funcInst o ctx fName vs = TxsString (T.concat [ fName
                                              , T.pack " ( "
                                              , T.intercalate (if multiline o 
                                                                  then T.concat [ T.singleton '\n'
                                                                                , T.replicate (T.length fName + 1) (T.singleton ' ')
                                                                                , T.pack ", "
                                                                                ]
                                                                  else T.pack ", ")
                                                              (map (indent ( T.replicate (T.length fName + 3) (T.singleton ' ') ) . TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o ctx) vs)
                                              , if multiline o 
                                                   then T.append (T.singleton '\n') (T.replicate (T.length fName + 1) (T.singleton ' '))
                                                   else T.singleton ' '
                                              , T.pack ")"
                                              ])

infixOperator :: (SortContext c, VarDef v) => Options -> c -> T.Text -> [ValExpression v] -> TxsString
infixOperator o ctx oName vs = 
        let offset = if multiline o then T.replicate (T.length oName + 1) (T.singleton ' ')
                                    else T.empty
          in
            TxsString (T.concat [ offset 
                                , T.pack "( "
                                , T.intercalate (T.concat [ separator o
                                                          , offset
                                                          , T.singleton ')'
                                                          , separator o
                                                          , oName
                                                          , T.pack " ( "
                                                         ])
                                                (map (indent (T.replicate (T.length oName + 3) (T.singleton ' ')) . TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o ctx) vs)
                                , separator o
                                , offset
                                , T.singleton ')'
                                ])

occuranceOperator :: (SortContext c, VarDef v) => Options -> c -> T.Text -> T.Text -> [(ValExpression v, Integer)] -> TxsString
occuranceOperator o ctx op1 op2 occuranceList =
        let offset1 = if multiline o then T.replicate (T.length op1 + 1) (T.singleton ' ')
                                     else T.empty
          in
            TxsString (T.concat [ offset1
                                , T.pack "( "
                                , T.intercalate (T.concat [ separator o
                                                          , offset1
                                                          , T.singleton ')'
                                                          , separator o
                                                          , op1
                                                          , T.pack " ( "
                                                         ])
                                                (map (indent offset2 . tupleToText) occuranceList)
                                , separator o
                                , offset1
                                , T.singleton ')'
                                ])
    where
        -- offset1 + "( "
        offset2 :: T.Text
        offset2 = T.replicate (T.length op1 + 3) (T.singleton ' ')
        
        tupleToText :: (VarDef v) => (ValExpression v, Integer) -> T.Text
        tupleToText (v,  1) = TorXakis.PrettyPrint.TorXakis.toText (prettyPrint o ctx v)
        tupleToText (v,  p) = indent offset2 (TorXakis.PrettyPrint.TorXakis.toText (infixOperator o ctx op2 [v, ValExpression (Vconst (Cint p))]))
