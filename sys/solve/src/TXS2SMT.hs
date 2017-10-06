{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module TXS2SMT

-- ----------------------------------------------------------------------------------------- --
--
-- Translate TorXakis definitions, declarations, and assertions into SMT
--
-- ----------------------------------------------------------------------------------------- --
-- export

( initialMapInstanceTxsToSmtlib  --  :: [ (Ident, String) ]
, insertMap            --  :: (Ident, TxsDef) -> Map.Map Ident String -> Map.Map Ident String
, basicDefinitionsSMT  --  :: String
, sortdefsToSMT        --  :: Map.Map Ident String -> TxsDefs -> String
, funcdefsToSMT        --  :: Map.Map Ident String -> TxsDefs -> String
, assertionsToSMT      --  :: Map.Map Ident String -> TxsDefs -> [ValExpr v] -> String
, declarationsToSMT    --  :: (Variable v) => Map.Map Ident String -> [v] -> String
, justLookup           -- Test Purposes -- :: Map.Map Ident String -> Ident -> String
, valexprToSMT         -- Test Purposes -- :: (Variable v) => Map.Map Ident String -> (ValExpr v) -> String
)

-- ----------------------------------------------------------------------------------------- --
--import

where

import qualified Data.Map      as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set      as Set
import           Data.Text     (Text)
import qualified Data.Text     as T

import           CstrId
import           FreeMonoidX
import           FuncId
import           Product
import           RegexXSD2SMT
import           SMTString
import           SortId
import           StdTDefs
import           Sum
import           TxsDefs
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- initialMapInstanceTxsToSmtlib

initialMapInstanceTxsToSmtlib :: [ (Ident, Text) ]
initialMapInstanceTxsToSmtlib  =  [
-- Sorts :
    (IdSort sortId_Bool,       "Bool"),
    (IdSort sortId_Int,        "Int"),
    (IdSort sortId_String,     "String"),
    (IdSort sortId_Regex,      error "Regex is not defined in SMT"),

-- Bool
    (IdFunc funcId_BoolToString,   error "ToString(Bool) should not be called in SMT"),
    (IdFunc funcId_BoolFromString, error "FromString(Bool) should not be called in SMT"),
    (IdFunc funcId_BoolToXml,      error "ToXml(Bool) should not be called in SMT"),
    (IdFunc funcId_BoolFromXml,    error "FromXml(Bool) should not be called in SMT"),

-- Int
    (IdFunc funcId_IntToString,    error "ToString(Int) should not be called in SMT"),
    (IdFunc funcId_IntFromString,  error "FromString(Int) should not be called in SMT"),
    (IdFunc funcId_IntToXml,       error "ToXml(Int) should not be called in SMT"),
    (IdFunc funcId_IntFromXml,     error "FromXml(Int) should not be called in SMT"),

-- String
    (IdFunc funcId_StringToString,     error "ToString(String) should not be called in SMT"),
    (IdFunc funcId_StringFromString,   error "FromString(String) should not be called in SMT"),
    (IdFunc funcId_StringToXml,        error "ToXml(String) should not be called in SMT"),
    (IdFunc funcId_StringFromXml,      error "FromXml(String) should not be called in SMT"),
    (IdFunc funcId_takeWhile,          error "takeWhile should not be called in SMT"),
    (IdFunc funcId_takeWhileNot,       error "takeWhileNot should not be called in SMT"),
    (IdFunc funcId_dropWhile,          error "dropWhile should not be called in SMT"),
    (IdFunc funcId_dropWhileNot,       error "dropWhileNot should not be called in SMT")

    ]


-- ----------------------------------------------------------------------------------------- --
-- initialMapInstanceTxsToSmtlib

toFieldName :: CstrId -> Int -> Text
toFieldName cstrid field  =  T.concat [toCstrName cstrid, "$", (T.pack . show) field]

toIsCstrName :: CstrId -> Text
toIsCstrName cstrid  =  "is-" <> toCstrName cstrid

toCstrName :: CstrId -> Text
toCstrName cstrid  =  T.concat [SortId.name (cstrsort cstrid), "$", CstrId.name cstrid]

toSortName :: SortId -> Text
toSortName = SortId.name

toFuncName :: FuncId -> Text
toFuncName funcId  =  T.concat ["f", (T.pack . show) (FuncId.unid funcId), "$", FuncId.name funcId]

insertMap :: (Ident, TxsDef) -> Map.Map Ident Text -> Map.Map Ident Text
insertMap (id'@(IdSort sid), DefSort SortDef) mp
  = if id' `Map.member` mp
       then error $ "TXS TXS2SMT insertMap: Sort " ++ show sid ++ " already defined\n"
       else Map.insert id' (toSortName sid) mp


insertMap (id'@(IdCstr cstrid), DefCstr(CstrDef c fs)) mp
  =  if id' `Map.member` mp
       then error $ "TXS TXS2SMT insertMap: Constructor (" ++ show cstrid ++ ", CstrDef " ++
                    show c ++ " " ++ show fs ++  ") already defined\n"
       else foldr ( \(f,p) -> Map.insert (IdFunc f) (toFieldName cstrid p) )
                  ( Map.insert (IdFunc c) (toIsCstrName cstrid)
                               ( Map.insert (IdCstr cstrid) (toCstrName cstrid) mp )
                  )
                  (zip fs [0..])

insertMap (id'@(IdFunc funcId), DefFunc (FuncDef x y)) mp
  =  if id' `Map.member` mp
       then error $ "TXS TXS2SMT insertMap: Function  (" ++ show funcId ++ ", FuncDef " ++
                    show x ++ " " ++ show y ++  ") already defined\n"
       else Map.insert id' (toFuncName funcId) mp

insertMap (_,d) _ = error $ "Illegal Definition for insertMap " ++ show d

-- ----------------------------------------------------------------------------------------- --
-- basic definitions for SMT
-- native Torxakis functions that are not natively supported in SMT
-- ----------------------------------------------------------------------------------------- --
basicDefinitionsSMT :: Text
basicDefinitionsSMT = ""
     -- ++ "(define-fun-rec pow ((a Int)(b Int)) Int (ite (= b 0) 1 (* a (pow a (- b 1)))))"


-- ----------------------------------------------------------------------------------------- --
-- convert definitions to SMT : first sort definitions, second function definitions
-- ----------------------------------------------------------------------------------------- --
-- definitionsToSMT :: Map.Map Ident String -> TxsDefs -> String
-- definitionsToSMT mapI tdefs = (sortdefsToSMT mapI tdefs) ++ "\n\n" ++
--                               (funcdefsToSMT mapI tdefs) ++ "\n\n"


-- ----------------------------------------------------------------------------------------- --
-- convert sort definitions to SMT type declarations (as multiple lines of commands)
-- ----------------------------------------------------------------------------------------- --
sortdefsToSMT :: Map.Map Ident Text -> TxsDefs -> Text
sortdefsToSMT mapI tdefs =
    let sorts = Map.keys (sortDefs tdefs) in
        case sorts of
            []      -> ""
            _       -> "(declare-datatypes () (\n"
                       <> foldMap (\s -> "    ("
                                           <> justLookup mapI (IdSort s)
                                           <> foldMap cstrToSMT (getCstrs s) <> ")\n" )
                                    sorts
                       <> ") )\n"
    where
        -- get the constructors of an ADT
        getCstrs :: SortId -> [(CstrId, CstrDef)]
        getCstrs s = [(cstrId', cstrDef) | (cstrId', cstrDef) <- Map.toList (cstrDefs tdefs), cstrsort cstrId' == s]

        -- convert the given constructor to a SMT constructor declaration
        cstrToSMT :: (CstrId, CstrDef) -> Text
        cstrToSMT (cstrId', CstrDef _ fields) = " (" <> justLookup mapI (IdCstr cstrId') <> cstrFieldsToSMT cstrId' fields <> ")"

        -- convert the given constructor fields to a SMT constructor declaration
        cstrFieldsToSMT :: CstrId -> [FuncId] -> Text
        cstrFieldsToSMT cstrId' fields =
            case fields of
                []  -> ""
                _   -> " (" <> T.intercalate ") (" (map (\(f,p) -> toFieldName cstrId' p <> " " <> justLookup mapI (IdSort (funcsort f))) (zip fields [0..]) ) <> ")"


-- | Convert function definitions to SMT type declarations (as multiple lines
-- of commands).
funcdefsToSMT :: Map.Map Ident Text -> TxsDefs -> Text
funcdefsToSMT mapTxs tdefs =
    -- tdefs contains only new function definitions
    toTxs (map toDT (TxsDefs.toList tdefs))
  where
    toTxs :: [(Text ,Text)] -> Text
    toTxs [] = ""
    toTxs l = let (lD,lT) = unzip l in
                "(define-funs-rec\n  (\n    " <> T.intercalate "\n    " lD <> "\n  )\n  (\n    " <> T.intercalate "\n    " lT <> "\n  )\n)\n"

    toDT :: (Ident, TxsDef) -> (Text, Text)
    toDT (fid@(IdFunc idf), DefFunc (FuncDef vs expr))  = ("("<> justLookup mapTxs fid <> "(" <> T.intercalate " " (map (\v -> "(" <> vname v <> " " <> justLookup mapTxs (IdSort (varsort v)) <> ")") vs) <> ") " <> justLookup mapTxs (IdSort (funcsort idf)) <>")", valexprToSMT mapTxs expr)
    toDT (_,_)                                          = error "Solve - TXS2SMT - funcdefsToSMT - Only FuncId and FuncDef tuple expected."
-- ----------------------------------------------------------------------------------------- --
-- assertions to SMT
-- ----------------------------------------------------------------------------------------- --
assertionsToSMT :: (Variable v) => Map.Map Ident Text -> [ValExpr v] -> Text
assertionsToSMT mapI assertions =
        T.intercalate "\n" (map assertionToSMT assertions)
    where
        assertionToSMT :: (Variable v) => ValExpr v -> Text
        assertionToSMT expr = "(assert " <> valexprToSMT mapI expr <> ")"

justLookup :: Map.Map Ident Text -> Ident -> Text
justLookup mapI ident =
    let ms = Map.lookup ident mapI in
        fromMaybe (error $ "Ident " ++ show ident ++ " not found in mapping with keys: " ++ show (Map.keys mapI) ++ "\n") ms

integer2smt :: Integer -> Text
integer2smt n | n < 0 = "(- " <> (T.pack . show) (abs n) <> ")"
integer2smt n = (T.pack . show) n
-- ----------------------------------------------------------------------------------------- --
-- constToSMT: translate a const to a SMT constraint
-- ----------------------------------------------------------------------------------------- --
constToSMT :: Map.Map Ident Text -> Const -> Text
constToSMT _ (Cbool b) = if b
                            then "true"
                            else "false"
constToSMT _ (Cint n) = integer2smt n
constToSMT _ (Cstring s)  =  "\"" <> stringToSMT s <> "\""
constToSMT _ (Cregex r)  =  xsd2smt r
constToSMT mapI (Cstr cd [])   =        justLookup mapI (IdCstr cd)
constToSMT mapI (Cstr cd args') = "(" <> justLookup mapI (IdCstr cd) <> " " <> T.intercalate " " (map (constToSMT mapI) args') <> ")"
constToSMT _ x = error ("Illegal input constToSMT - " <> show x)

-- ----------------------------------------------------------------------------------------- --
-- valexprToSMT: translate a ValExpr to a SMT constraint
-- ----------------------------------------------------------------------------------------- --
valexprToSMT :: (Variable v) => Map.Map Ident Text -> ValExpr v -> Text
valexprToSMT mapI (view -> Vfunc funcId [])   =        justLookup mapI (IdFunc funcId)
valexprToSMT mapI (view -> Vfunc funcId args') = "(" <> justLookup mapI (IdFunc funcId) <> " " <> T.intercalate " " (map (valexprToSMT mapI) args') <> ")"

valexprToSMT mapI (view -> Vcstr cd [])    =        justLookup mapI (IdCstr cd)
valexprToSMT mapI (view -> Vcstr cd args') = "(" <> justLookup mapI (IdCstr cd) <> " " <> T.intercalate " " (map (valexprToSMT mapI) args') <> ")"

valexprToSMT mapI (view -> Viscstr cd arg)    = "(" <> toIsCstrName cd <> " " <> valexprToSMT mapI arg <> ")"
valexprToSMT mapI (view -> Vaccess cd p arg)  = "(" <> toFieldName cd p <> " " <> valexprToSMT mapI arg <> ")"


valexprToSMT mapI (view -> Vconst c) = constToSMT mapI c

valexprToSMT _ (view -> Vvar varId)  =  vname varId

valexprToSMT mapI (view -> Vite c expr1 expr2) = "(ite " <> valexprToSMT mapI c <> " "  <> valexprToSMT mapI expr1 <> " " <> valexprToSMT mapI expr2 <> ")"


valexprToSMT mapI (view -> Vsum s) =
    let ol = toOccurListT s in
        case ol of
        {  [o] -> arg2smt o
        ;   _  -> "(+ " <> T.intercalate " " (map arg2smt ol) <> ")"
        }
    where
        arg2smt :: (Variable v) => (ValExpr v, Integer) -> Text
        arg2smt (vexpr, 1)                              = valexprToSMT mapI vexpr
        arg2smt (vexpr, -1)                             = "(- " <> valexprToSMT mapI vexpr <> ")"
        arg2smt (vexpr, multiplier) |  multiplier /= 0  = "(* " <> integer2smt multiplier <> " " <> valexprToSMT mapI vexpr <> ")"
        arg2smt (_, multiplier)                         = error ("valexprToSMT - arg2smt - illegal multiplier " ++ show multiplier)

valexprToSMT mapI (view -> Vproduct p) =
    let ol = toOccurListT p in
        case ol of
        {  [o] -> arg2smt o
        ;   _  -> "(* " <> T.intercalate " " (map arg2smt ol) <> ")"
        }
    where
        arg2smt :: (Variable v) => (ValExpr v, Integer) -> Text
        arg2smt (vexpr, 1)                  = valexprToSMT mapI vexpr
        arg2smt (vexpr, power) |  power > 0 = "(^ " <> valexprToSMT mapI vexpr <> " " <> integer2smt power <> ")"
        arg2smt (_, power)                  = error ("valexprToSMT - arg2smt - illegal power " ++ show power)

valexprToSMT mapI (view -> Vdivide t n) = "(div " <> valexprToSMT mapI t <> " "  <> valexprToSMT mapI n <> ")"
valexprToSMT mapI (view -> Vmodulo t n) = "(mod " <> valexprToSMT mapI t <> " "  <> valexprToSMT mapI n <> ")"
valexprToSMT mapI (view -> Vgez v)      = "(<= 0 " <> valexprToSMT mapI v <> ")"

valexprToSMT mapI (view -> Venv venv expr)  =
    if Map.null venv
        then valexprToSMT mapI expr
        else "(let (" <> Map.foldrWithKey keyValuePairVarIdVExprToSMT "" venv <> ") "
              <> valexprToSMT mapI expr <> ")"
    where
        keyValuePairVarIdVExprToSMT :: (Variable v) => v -> ValExpr v -> Text -> Text
        keyValuePairVarIdVExprToSMT v expr' s = s <> "(" <> valexprToSMT mapI (cstrVar v) <> " " <> valexprToSMT mapI expr' <> ")"
-- alternative implementation:
--    valexprToSMT mapI (substVEnv venv vexpr)

valexprToSMT mapI (view -> Vequal expr1 expr2)  =
    "(= " <> valexprToSMT mapI expr1 <> " " <> valexprToSMT mapI expr2 <> ")"

valexprToSMT mapI (view -> Vnot expr)  =
    "(not " <> valexprToSMT mapI expr <> ")"

valexprToSMT mapI (view -> Vand exprs)  =
    "(and " <> T.intercalate " " (map (valexprToSMT mapI) (Set.toList exprs)) <> ")"

valexprToSMT mapI (view -> Vlength expr)  =
    "(str.len " <> valexprToSMT mapI expr <> ")"
valexprToSMT mapI (view -> Vat s p)  =
    "(str.at " <> valexprToSMT mapI s <> " " <> valexprToSMT mapI p <> ")"
valexprToSMT mapI (view -> Vconcat vexprs)  =
    "(str.++ " <> T.intercalate " " (map (valexprToSMT mapI) vexprs) <> ")"
valexprToSMT mapI (view -> Vstrinre s r)  =
    "(str.in.re " <> valexprToSMT mapI s <> " " <> valexprToSMT mapI r <> ")"
valexprToSMT mapI (view -> Vpredef _ funcId args')  =
    "(" <> justLookup mapI (IdFunc funcId) <> " " <> T.intercalate " " (map (valexprToSMT mapI) args') <> ")"

valexprToSMT _ x = error ("Illegal input valexprToSMT - " ++ show x)

-- ----------------------------------------------------------------------------------------- --
declarationsToSMT :: (Variable v) => Map.Map Ident Text -> [v] -> Text
declarationsToSMT mapI vs  =
    T.intercalate "\n" (map declarationToSMT vs)
    where
      declarationToSMT :: (Variable v) => v -> Text
      declarationToSMT v  =  "(declare-fun " <> vname v <> "() " <> justLookup mapI (IdSort (vsort v)) <>")"


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
