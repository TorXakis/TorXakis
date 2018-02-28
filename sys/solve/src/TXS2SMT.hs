{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TXS2SMT
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Translate TorXakis definitions, declarations, and assertions into SMT.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module TXS2SMT
( basicDefinitionsSMT

, adtDefsToSMT
, funcdefsToSMT

, declarationsToSMT
, assertionsToSMT

, valexprToSMT
)
where

import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict     as Map
import           Data.Monoid
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T

import           ConstDefs
import           FreeMonoidX
import           FuncDef
import           FuncId
import           Name
import           RegexXSD2SMT
import           SMTString
import           Sort
import           ValExpr
import           Variable
import           VarId

toFieldName :: Ref (ADTDef Sort) -> Ref (ConstructorDef Sort) -> Int -> Text
toFieldName aRef cRef field  = toCstrName aRef cRef <> "$" <> (T.pack . show) field

toIsCstrName :: Ref (ADTDef Sort) -> Ref (ConstructorDef Sort) -> Text
toIsCstrName aRef cRef  =  "is-" <> toCstrName aRef cRef

toCstrName :: Ref (ADTDef Sort) -> Ref (ConstructorDef Sort) -> Text
toCstrName aRef cRef  =  toRefName aRef <> "$" <> toRefName cRef

toSortName :: Sort -> Text
toSortName SortError   = error "Error is not defined in SMT"
toSortName SortBool    = "Bool"
toSortName SortInt     = "Int"
toSortName SortChar    = error "Char is not yet supported"
toSortName SortString  = "String"
toSortName SortRegex   = error "Regex is not defined in SMT"
toSortName (SortADT r) = toRefName r

toRefName :: Ref a -> Text
toRefName = toText . toName

toADTName :: Ref (ADTDef Sort) -> Text
toADTName = toRefName

toFuncName :: FuncId -> Text
toFuncName funcId  =  T.concat ["f", (T.pack . show) (FuncId.unid funcId), "$", toText $ FuncId.name funcId]

-- ----------------------------------------------------------------------------------------- --
-- basic definitions for SMT
-- native Torxakis functions that are not natively supported in SMT
-- ----------------------------------------------------------------------------------------- --
basicDefinitionsSMT :: Text
basicDefinitionsSMT = ""

-- | convert sort definitions to SMT type declarations (as multiple lines of commands)
adtDefsToSMT :: HMap.HashMap (Ref (ADTDef Sort)) (ADTDef Sort) -> (Text, HMap.HashMap Text (Ref (ADTDef Sort), Ref (ConstructorDef Sort)))
adtDefsToSMT adtMap
    | HMap.null adtMap = ("", HMap.empty)
    | otherwise       = ("(declare-datatypes () (\n"
                        <> T.concat (map
                            (\(r,d) -> "    (" <> toADTName r <> adtDefToSMT r d <> ")\n" )
                            $ HMap.toList adtMap)
                        <> ") )\n",
                        HMap.fromList $ concatMap (\(r,d) -> map (\k -> (toCstrName r k, (r,k))) $ HMap.keys . cDefsToMap $ constructors d)
                                                 $ HMap.toList adtMap
                        )
    where
        -- convert the given constructor to a SMT constructor declaration
        adtDefToSMT :: Ref (ADTDef Sort) -> ADTDef Sort -> Text
        adtDefToSMT adtRf adtDef = 
            T.concat $ map (\(r,d) -> " (" <> toCstrName adtRf r
                                      <> cstrFieldsToSMT adtRf r (fields d)
                                      <> ")" )
                           $ HMap.toList . cDefsToMap $ constructors adtDef

        -- convert the given constructor fields to a SMT constructor declaration
        cstrFieldsToSMT :: Ref (ADTDef Sort)
                        -> Ref (ConstructorDef Sort)
                        -> FieldDefs Sort
                        -> Text
        cstrFieldsToSMT adtRf cRf fDefs =
            case nrOfFieldDefs fDefs of
                0  -> ""
                _   ->  " (" <> T.intercalate ") ("
                        ( map (\(d,p) -> toFieldName adtRf cRf p <> " " <> toSortName (Sort.sort d))
                              (zip (fDefsToList fDefs) [0..]) )
                        <> ")"


-- | Convert function definitions to SMT type declarations (as multiple lines
-- of commands).
funcdefsToSMT :: Map.Map FuncId (FuncDef VarId) -> Text
funcdefsToSMT fdefs =
    toTxs (map toDT (Map.toList fdefs))
  where
    toTxs :: [(Text ,Text)] -> Text
    toTxs [] = ""
    toTxs l = let (lD,lT) = unzip l in
                "(define-funs-rec\n  (\n    " <> T.intercalate "\n    " lD <> "\n  )\n  (\n    " <> T.intercalate "\n    " lT <> "\n  )\n)\n"

    toDT :: (FuncId, FuncDef VarId) -> (Text, Text)
    toDT (funcId, FuncDef vs expr)  = ("(" <> toFuncName funcId
                                           <> "("
                                           <> T.intercalate
                                                " "
                                                (map (\v -> "("
                                                            <> toText (vname v)
                                                            <> " "
                                                            <> toSortName (varsort v)
                                                            <> ")")
                                                     vs)
                                           <> ") "
                                           <> toSortName (funcsort funcId)
                                           <> ")"
                                      , valexprToSMT expr
                                      )
-- ----------------------------------------------------------------------------------------- --
-- assertions to SMT
-- ----------------------------------------------------------------------------------------- --
assertionsToSMT :: (Variable v) => [ValExpr v] -> Text
assertionsToSMT assertions =
        T.intercalate "\n" (map assertionToSMT assertions)
    where
        assertionToSMT :: (Variable v) => ValExpr v -> Text
        assertionToSMT expr = "(assert " <> valexprToSMT expr <> ")"


integer2smt :: Integer -> Text
integer2smt n | n < 0 = "(- " <> (T.pack . show) (abs n) <> ")"
integer2smt n = (T.pack . show) n
-- ----------------------------------------------------------------------------------------- --
-- constToSMT: translate a const to a SMT constraint
-- ----------------------------------------------------------------------------------------- --
constToSMT :: Const -> Text
constToSMT (Cbool b) = if b
                        then "true"
                        else "false"
constToSMT (Cint n) = integer2smt n
constToSMT (Cstring s)  =  "\"" <> stringToSMT s <> "\""
constToSMT (Cregex r)  =  xsd2smt r
constToSMT (Cstr aRef cRef []) =         toCstrName aRef cRef
constToSMT (Cstr aRef cRef args') = "(" <> toCstrName aRef cRef <> " " <> T.intercalate " " (map constToSMT args') <> ")"
constToSMT x = error ("Illegal input constToSMT - " <> show x)

-- ----------------------------------------------------------------------------------------- --
-- valexprToSMT: translate a ValExpr to a SMT constraint
-- ----------------------------------------------------------------------------------------- --
valexprToSMT :: (Variable v) => ValExpr v -> Text
valexprToSMT (view -> Vfunc funcId [])    =        toFuncName funcId
valexprToSMT (view -> Vfunc funcId args') = "(" <> toFuncName funcId <> " " <> T.intercalate " " (map valexprToSMT args') <> ")"

valexprToSMT (view -> Vcstr aRef cRef [])    =        toCstrName aRef cRef
valexprToSMT (view -> Vcstr aRef cRef args') = "(" <> toCstrName aRef cRef <> " " <> T.intercalate " " (map valexprToSMT args') <> ")"

valexprToSMT (view -> Viscstr aRef cRef arg)       = "(" <> toIsCstrName aRef cRef <> " " <> valexprToSMT arg <> ")"
valexprToSMT (view -> Vaccess aRef cRef p _s arg)  = "(" <> toFieldName aRef cRef p <> " " <> valexprToSMT arg <> ")"

valexprToSMT (view -> Vconst c) = constToSMT c

valexprToSMT (view -> Vvar varId)  = toText $ vname varId

valexprToSMT (view -> Vite c expr1 expr2) = "(ite " <> valexprToSMT c <> " "  <> valexprToSMT expr1 <> " " <> valexprToSMT expr2 <> ")"

valexprToSMT (view -> Vsum s) =
    let ol = toOccurListT s in
        case ol of
        {  [o] -> arg2smt o
        ;   _  -> "(+ " <> T.intercalate " " (map arg2smt ol) <> ")"
        }
    where
        arg2smt :: (Variable v) => (ValExpr v, Integer) -> Text
        arg2smt (vexpr, 1)                              = valexprToSMT vexpr
        arg2smt (vexpr, -1)                             = "(- " <> valexprToSMT vexpr <> ")"
        arg2smt (vexpr, multiplier) |  multiplier /= 0  = "(* " <> integer2smt multiplier <> " " <> valexprToSMT vexpr <> ")"
        arg2smt (_, multiplier)                         = error ("valexprToSMT - arg2smt - illegal multiplier " ++ show multiplier)

valexprToSMT (view -> Vproduct p) =
    let ol = toOccurListT p in
        case ol of
        {  [o] -> arg2smt o
        ;   _  -> "(* " <> T.intercalate " " (map arg2smt ol) <> ")"
        }
    where
        arg2smt :: (Variable v) => (ValExpr v, Integer) -> Text
        arg2smt (vexpr, 1)                  = valexprToSMT vexpr
        arg2smt (vexpr, power) |  power > 0 = "(^ " <> valexprToSMT vexpr <> " " <> integer2smt power <> ")"
        arg2smt (_, power)                  = error ("valexprToSMT - arg2smt - illegal power " ++ show power)

valexprToSMT (view -> Vdivide t n) = "(div " <> valexprToSMT t <> " "  <> valexprToSMT n <> ")"
valexprToSMT (view -> Vmodulo t n) = "(mod " <> valexprToSMT t <> " "  <> valexprToSMT n <> ")"
valexprToSMT (view -> Vgez v)      = "(<= 0 " <> valexprToSMT v <> ")"

valexprToSMT (view -> Vequal expr1 expr2)  =
    "(= " <> valexprToSMT expr1 <> " " <> valexprToSMT expr2 <> ")"

valexprToSMT (view -> Vnot expr)  =
    "(not " <> valexprToSMT expr <> ")"

valexprToSMT (view -> Vand exprs)  =
    "(and " <> T.intercalate " " (map valexprToSMT (Set.toList exprs)) <> ")"

valexprToSMT (view -> Vlength expr)  =
    "(str.len " <> valexprToSMT expr <> ")"
valexprToSMT (view -> Vat s p)  =
    "(str.at " <> valexprToSMT s <> " " <> valexprToSMT p <> ")"
valexprToSMT (view -> Vconcat vexprs)  =
    "(str.++ " <> T.intercalate " " (map valexprToSMT vexprs) <> ")"
valexprToSMT (view -> Vstrinre s r)  =
    "(str.in.re " <> valexprToSMT s <> " " <> valexprToSMT r <> ")"
valexprToSMT x = error ("Illegal input valexprToSMT - " ++ show x)


declarationsToSMT :: (Variable v) => [v] -> Text
declarationsToSMT vs  =
    T.intercalate "\n" (map declarationToSMT vs)
    where
      declarationToSMT :: (Variable v) => v -> Text
      declarationToSMT v  =  "(declare-fun " <> toText (vname v) <> "() " <> toSortName (vsort v) <> ")"
