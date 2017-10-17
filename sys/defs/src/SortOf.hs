{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- | SortOf for TxsDefs
module SortOf

where

import           BehExprDefs
import           ConstDefs
import           CstrId
import           FuncId
import           SortId
import           ValExprDefs
import           Variable
import           VarId

-- * standard sorts

sortId_Bool :: SortId
sortId_Bool = SortId "Bool" 101

sortId_Int :: SortId
sortId_Int = SortId "Int" 102

sortId_String :: SortId
sortId_String = SortId "String" 104

sortId_Regex :: SortId
sortId_Regex = SortId "Regex" 105

-- | Value expression, etc. :  sortOf
class SortOf s where
  sortOf :: s -> SortId

instance SortOf ChanOffer where
  sortOf (Quest (VarId _nm _uid vs)) =  vs
  sortOf (Exclam vexp)               =  sortOf vexp

sortIdError :: SortId
sortIdError = SortId "_Error" (-1)

instance (Variable v) => SortOf (ValExpr v) where
  sortOf vexp = let s = sortOf' vexp in
    if s == sortIdError
    then sortId_String
    else s

sortOf' :: (Variable v) => ValExpr v -> SortId
sortOf' (view -> Vfunc (FuncId _nm _uid _fa fs) _vexps) = fs
sortOf' (view -> Vcstr (CstrId _nm _uid _ca cs) _vexps) = cs
sortOf' (view -> Viscstr _ _)                           = sortId_Bool
sortOf' (view -> Vaccess (CstrId _nm _uid ca _cs) p _vexps) = ca!!p
sortOf' (view -> Vconst con)                            =  sortOf con
sortOf' (view -> Vvar v)                                =  vsort v
sortOf' (view -> Vite _cond vexp1 vexp2)                =  -- if the LHS is an error (Verror), we want to yield the type of the RHS which might be no error
                                                             let sort' = sortOf' vexp1 in
                                                             if sort' == sortIdError
                                                               then sortOf' vexp2
                                                               else sort'
sortOf' (view -> Vequal { })                            =  sortId_Bool
sortOf' (view -> Vnot { })                              =  sortId_Bool
sortOf' (view -> Vand { })                              =  sortId_Bool
sortOf' (view -> Vsum { })                              =  sortId_Int
sortOf' (view -> Vproduct { })                          =  sortId_Int
sortOf' (view -> Vmodulo { })                           =  sortId_Int
sortOf' (view -> Vdivide { })                           =  sortId_Int
sortOf' (view -> Vgez { })                              =  sortId_Bool
sortOf' (view -> Vlength { })                           =  sortId_Int
sortOf' (view -> Vat { })                               =  sortId_String
sortOf' (view -> Vconcat { })                           =  sortId_String
sortOf' (view -> Vstrinre { })                          =  sortId_Bool
sortOf' (view -> Vpredef _kd (FuncId _nm _uid _fa fs) _vexps)  =  fs
sortOf' (view -> Vpredef{})                             = error "sortOf': Unexpected Ident with Vpredef"
sortOf' (view -> Vany srt)                              =  srt
sortOf' (view -> Verror _str)                           =  sortIdError
sortOf' _                                               = error "sortOf': All items must be in view"

instance SortOf Const where
  sortOf (Cbool _b)                        = sortId_Bool
  sortOf (Cint _i)                         = sortId_Int
  sortOf (Cstring _s)                      = sortId_String
  sortOf (Cstr (CstrId _nm _uid _ca cs) _) = cs
  sortOf _                                 = error "Unexpect SortOf - Const"


instance SortOf VarId  where
  sortOf (VarId _nm _unid srt)                    = srt
