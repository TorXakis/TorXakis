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
  -- * Evaluate
, eval
)
where
import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import           TorXakis.Error
import           TorXakis.FreeVars
import           TorXakis.FuncSignature
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.Value
import           TorXakis.VarContext
import           TorXakis.VarDef

-- | ValExpressionView: the public view of value expression 'ValExpression'
data ValExpressionView = Vconst    Value
                       | Vvar      (RefByName VarDef)
                       -- generic
                       | Vequal    ValExpression
                                   ValExpression
                       | Vite      ValExpression
                                   ValExpression
                                   ValExpression
                       | Vfunc     FuncSignature [ValExpression]
                       | Vpredef   FuncSignature [ValExpression]
                       -- Boolean
                       | Vnot      ValExpression
                       | Vand      (Set.Set ValExpression)
                       -- Int
                       | Vdivide   ValExpression
                                   ValExpression
                       | Vmodulo   ValExpression
                                   ValExpression
                       | Vsum      (Map.Map ValExpression Integer)
                       | Vproduct  (Map.Map ValExpression Integer)
                       | Vgez      ValExpression
                       -- String
                       | Vlength   ValExpression
                       | Vat       ValExpression
                                   ValExpression
                       | Vconcat   [ValExpression]
                       -- Regex
                       | Vstrinre  ValExpression
                                   ValExpression
                       -- ADT
                       | Vcstr     (RefByName ADTDef) (RefByName ConstructorDef) [ValExpression]
                       | Viscstr   (RefByName ADTDef) (RefByName ConstructorDef) ValExpression
                       | Vaccess   (RefByName ADTDef) (RefByName ConstructorDef) Int ValExpression
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | ValExpression: value expression
--
-- 1. User can't directly construct ValExpression (such that invariants will always hold)
--
-- 2. User can still pattern match on ValExpression using 'ValExpressionView'
--
-- 3. Overhead at run-time is zero. See https://wiki.haskell.org/Performance/Data_types#Newtypes
newtype ValExpression = ValExpression { -- | View on value expression.
                                          view :: ValExpressionView
                                        }
  deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Evaluate the provided value expression.
-- Either the Right Constant Value is returned or a (Left) error message.
eval :: ValExpression -> Either Error Value
eval = evalView . view

evalView :: ValExpressionView -> Either Error Value
evalView (Vconst v) = Right v
evalView x          = Left $ Error ("Value Expression is not a constant value " ++ show x)

-- | SortOf instance
instance VarContext a => HasSort a ValExpression where
  getSort c = getSort c . TorXakis.ValExpr.ValExpr.view

instance VarContext a => HasSort a ValExpressionView where
    getSort ctx (Vconst val)              = getSort ctx val
    getSort ctx (Vvar r)                  = case lookupVar ctx r of
                                               Nothing -> error ("getSort: VarDef not found in context " ++ show r)
                                               Just v  -> getSort ctx v
    getSort _    Vequal { }               = SortBool
    getSort ctx (Vite _cond vexp1 _vexp2) = getSort ctx vexp1
    getSort _    Vnot { }                 = SortBool
    getSort _    Vand { }                 = SortBool
    getSort _    Vdivide { }              = SortInt
    getSort _    Vmodulo { }              = SortInt
    getSort _    Vsum { }                 = SortInt
    getSort _    Vproduct { }             = SortInt
    getSort _    Vgez { }                 = SortBool
    getSort _    Vlength { }              = SortInt
    getSort _    Vat { }                  = SortString
    getSort _    Vconcat { }              = SortString
    getSort _    Vstrinre { }             = SortBool
    getSort _   (Vcstr a _c _vexps)       = SortADT a
    getSort _    Viscstr { }              = SortBool
    getSort ctx (Vaccess a c p _vexps)    = case lookupADT ctx a of
                                               Nothing   -> error ("getSort: ADTDef not found in context " ++ show a)
                                               Just aDef -> case lookupConstructor aDef c of
                                                               Nothing   -> error ("getSort: Constructor not found in ADTDef " ++ show c)
                                                               Just cDef -> getSort ctx ( fields cDef !! p )
    getSort _   (Vfunc fs _vexps)         = returnSort fs
    getSort _   (Vpredef fs _vexps)       = returnSort fs

instance FreeVars ValExpression where
    freeVars = freeVars . view

instance FreeVars ValExpressionView where
    freeVars  Vconst{}         = Set.empty
    freeVars (Vvar v)          = Set.singleton v
    freeVars (Vequal v1 v2)    = Set.unions $ map freeVars [v1, v2]
    freeVars (Vite c t f)      = Set.unions $ map freeVars [c, t, f]
    freeVars (Vfunc _ as)      = Set.unions $ map freeVars as
    freeVars (Vpredef _ as)    = Set.unions $ map freeVars as
    freeVars (Vnot v)          = freeVars v
    freeVars (Vand s)          = Set.unions $ map freeVars (Set.toList s)
    freeVars (Vdivide t n)     = Set.unions $ map freeVars [t, n]
    freeVars (Vmodulo t n)     = Set.unions $ map freeVars [t, n]
    freeVars (Vsum m)          = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vproduct m)      = Set.unions $ map freeVars (Map.keys m)
    freeVars (Vgez v)          = freeVars v
    freeVars (Vlength v)       = freeVars v
    freeVars (Vat s p)         = Set.unions $ map freeVars [s, p]
    freeVars (Vconcat vs)      = Set.unions $ map freeVars vs
    freeVars (Vstrinre s r)    = Set.unions $ map freeVars [s, r]
    freeVars (Vcstr _ _ vs)    = Set.unions $ map freeVars vs
    freeVars (Viscstr _ _ v)   = freeVars v
    freeVars (Vaccess _ _ _ v) = freeVars v

-- Pretty Print 
instance SortContext c => PrettyPrint c ValExpression where
  prettyPrint o c = prettyPrint o c . view

instance SortContext c => PrettyPrint c ValExpressionView where
  prettyPrint _ ctx (Vconst c)          = TxsString (valueToText ctx c)
  prettyPrint _ _   (Vvar v)            = TxsString (TorXakis.Name.toText (toName v))
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
  prettyPrint o ctx (Vaccess a c p v)   = case lookupADT ctx a of
                                            Nothing     -> error ("Pretty Print accessor refers to undefined adt " ++ show a)
                                            Just aDef   -> case lookupConstructor aDef c of
                                                                Nothing     -> error ("Pretty Print accessor refers to undefined constructor " ++ show c)
                                                                Just cDef   -> let field = fields cDef !! p in
                                                                                    funcInst o ctx (TorXakis.Name.toText (fieldName field)) [v]

-- | Helper function since func and predef both are function Instantations in TorXakis
funcInst :: SortContext c => Options -> c -> T.Text -> [ValExpression] -> TxsString
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

infixOperator :: SortContext c => Options -> c -> T.Text -> [ValExpression] -> TxsString
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

occuranceOperator :: SortContext c => Options -> c -> T.Text -> T.Text -> [(ValExpression, Integer)] -> TxsString
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
        
        tupleToText :: (ValExpression, Integer) -> T.Text
        tupleToText (v,  1) = TorXakis.PrettyPrint.TorXakis.toText (prettyPrint o ctx v)
        tupleToText (v,  p) = indent offset2 (TorXakis.PrettyPrint.TorXakis.toText (infixOperator o ctx op2 [v, ValExpression (Vconst (Cint p))]))
