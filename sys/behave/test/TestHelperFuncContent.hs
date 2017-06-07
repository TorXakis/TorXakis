{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE ViewPatterns #-}
module TestHelperFuncContent

where
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.String.Utils as Utils
-- import qualified Debug.Trace as Trace

import TxsAlex
import TxsHappy
import TxsDefs hiding (vexpr)
import TxsShow

import StdTDefs
import SortId
---------------------------------------------------------------------------
-- Helper Functions
---------------------------------------------------------------------------
boolSortName :: String
boolSortName = "Bool"

intSortName :: String
intSortName = "Int"

stringSortName :: String
stringSortName = "String"

regexSortName :: String
regexSortName = "Regex"

dontCareUnid :: Int
dontCareUnid = 0

dontCareName :: String
dontCareName = []

type TypedElement = ([String],String)
type TypedElements = [ TypedElement ]

type Constructor = (String, TypedElements)
type Constructors = [ Constructor ] 

identicalFuncId :: FuncId -> FuncId -> Bool
identicalFuncId (FuncId name1 _ funcargs1 funcsort1) (FuncId name2 _ funcargs2 funcsort2) =       (name1 == name2)
                                                                                               && (identicalSortIds funcargs1 funcargs2)
                                                                                               && (identicalSortId funcsort1 funcsort2)
                                                                                               
identicalCstrId :: CstrId -> CstrId -> Bool
identicalCstrId (CstrId name1 _ cstrargs1 cstrsort1) (CstrId name2 _ cstrargs2 cstrsort2) =       (name1 == name2)
                                                                                               && (identicalSortIds cstrargs1 cstrargs2)
                                                                                               && (identicalSortId cstrsort1 cstrsort2)
                                                                                               
identicalVarId :: VarId -> VarId -> Bool
identicalVarId (VarId name1 _ sortid1) (VarId name2 _ sortid2) = (name1 == name2) && (identicalSortId sortid1 sortid2)
                                                                                               
identicalSortIds :: [SortId] -> [SortId] -> Bool
identicalSortIds [] [] = True
identicalSortIds (s1:xs1) (s2:xs2) = (identicalSortId s1 s2) && (identicalSortIds xs1 xs2)
identicalSortIds _ _ = False            -- different length
               
identicalSortId :: SortId -> SortId -> Bool
identicalSortId s1 s2 = (SortId.name s1) == (SortId.name s2)     

identicalMap :: VarEnv VarId VarId -> VarEnv VarId VarId -> Bool
identicalMap map1 map2 = containsAllIdentical (Map.toList map1) (Map.toList map2) 

containsAllIdentical :: [(VarId, VExpr)] -> [(VarId, VExpr)] -> Bool
containsAllIdentical [] _ = True
containsAllIdentical (x:xs) s = (containsIdentical x s) && (containsAllIdentical xs s)

containsIdentical :: (VarId, VExpr) -> [(VarId, VExpr)] -> Bool
containsIdentical x [] = False
containsIdentical x1@(k1,v1) ((k2,v2):xs) =    ( (identicalVarId k1 k2) && (identicalVExpr v1 v2) ) 
                                            || (containsIdentical x1 xs)

identicalVExpr :: VExpr -> VExpr -> Bool
identicalVExpr (view -> Vfunc fid1 vexps1)      (view -> Vfunc fid2 vexps2)      = (identicalFuncId fid1 fid2) && (identicalVExprs vexps1 vexps2)
identicalVExpr (view -> Vcstr cid1 vexps1)      (view -> Vcstr cid2 vexps2)      = (identicalCstrId cid1 cid2) && (identicalVExprs vexps1 vexps2)
identicalVExpr (view -> Vconst c1)              (view -> Vconst c2)              = (c1 == c2)
identicalVExpr (view -> Vvar v1)                (view -> Vvar v2)                = (identicalVarId v1 v2)
identicalVExpr (view -> Vite vexps1 vt1 ve1)    (view -> Vite vexps2 vt2 ve2)    = (identicalVExprs vexps1 vexps2) && (identicalVExpr vt1 vt2)  && (identicalVExpr ve1 ve2)
identicalVExpr (view -> Venv map1 v1)           (view -> Venv map2 v2)           = (identicalMap map1 map2) && (identicalVExpr v1 v2)
identicalVExpr (view -> Vequal vl1 vr1)         (view -> Vequal vl2 vr2)         = (identicalVExpr vl1 vl2)  && (identicalVExpr vr1 vr2)
identicalVExpr (view -> Vpredef p1 fid1 vexps1) (view -> Vpredef p2 fid2 vexps2) = (p1 == p2) && (identicalFuncId fid1 fid2) && (identicalVExprs vexps1 vexps2)
identicalVExpr (view -> Verror s1)              (view -> Verror s2)              = (s1 == s2)
identicalVExpr _                   _                   = False                          -- different 

identicalVExprs :: [VExpr] -> [VExpr] -> Bool
identicalVExprs [] [] = True
identicalVExprs (s1:xs1) (s2:xs2) = (identicalVExpr s1 s2) && (identicalVExprs xs1 xs2)
identicalVExprs _ _ = False            -- different length

data  FuncContent =  FuncContent { vexpr :: VExpr }
    deriving (Ord,Read,Show)
    
instance Eq FuncContent where  
    FuncContent x == FuncContent y = identicalVExpr x y
    
toTorXakisDefs :: (Int, TxsDefs) -> TxsDefs
toTorXakisDefs (a,b) = b

parseTorXakis :: String -> TxsDefs
parseTorXakis txt = -- Trace.trace ("txt = " ++ txt) 
                    let parserOutput = txsParser (txsLexer txt) in
                        -- Trace.trace ("parser output = " ++ show(parserOutput)) 
                            (toTorXakisDefs parserOutput)

fromTypedElementsToSortIds :: TypedElements -> [SortId]
fromTypedElementsToSortIds params = concat (map (\(is,t) -> (map (\i -> expectSortId t) is)) params )

fromMaybeTypeToMaybeSortIds :: (Maybe [String]) -> ExitSort
fromMaybeTypeToMaybeSortIds Nothing = NoExit
fromMaybeTypeToMaybeSortIds (Just params) = Exit (map expectSortId params)

fromTypedElementsToVarIds :: TypedElements -> [VarId]
fromTypedElementsToVarIds params = concat (map (\(is,t) -> (map (\i -> expectVarId i t) is)) params )

fromTypedElementsToVExprs :: TypedElements -> [VExpr]
fromTypedElementsToVExprs params = concat (map (\(is,t) -> (map (\i -> (cstrVar (expectVarId i t))) is)) params )

fromTypedElementsToChanIds :: TypedElements -> [ChanId]
fromTypedElementsToChanIds params = (map (\(ts,n) -> expectChanId n ts) params)

fromTypedElementsToFuncContents :: TypedElements -> [FuncContent]
fromTypedElementsToFuncContents params = concat (map (\(is,t) -> (map (\i -> (FuncContent (cstrVar (expectVarId i t)))) is)) params )

createSortId :: String -> String
createSortId name = name

expectSortId :: String -> SortId
expectSortId name = SortId name dontCareUnid

createCstrId :: String -> TypedElements -> String -> String
createCstrId cstrName fields sortDefName = cstrName ++ " { " ++ 
                                            ( Utils.join " , " (map (\(field,t) -> (Utils.join ", " field) ++ " :: " ++ t) fields) ) 
                                            ++ " }"

expectCstrId :: String -> TypedElements -> String -> CstrId
expectCstrId cstrName types sortDefName = CstrId cstrName dontCareUnid (fromTypedElementsToSortIds types) (expectSortId sortDefName) 

createSortDef :: String -> Constructors -> String
createSortDef sortDefName constrs = "TYPEDEF " ++ sortDefName ++ " ::=\n\t  " ++ 
                                            ( Utils.join "\n\t| " (map (\(constrName, fields) -> createCstrId constrName fields sortDefName) constrs ) )
                                    ++ "\nENDDEF"

expectSortDef :: String -> Constructors -> TxsDefs
expectSortDef sortDefName constrs = 
    let
        funcArgs = [([dontCareName],sortDefName)]
        -- IsConstructor Functions
        isConstrFuncId constrName = expectFuncId ("is"++constrName) funcArgs "Bool"
        -- Accessor Functions for constructor fields
        isAccessorsFuncIds types = concat ( map (\(fields,t) -> map (\field -> expectFuncId field funcArgs t) fields) types )
        -- IsEqual Functions
        isEqualFuncId = expectFuncId eqName [(["x", "y"],sortDefName)] "Bool"
     in
        TxsDefs.fromList ( [
                        -- SortDefinition
                        (IdSort (expectSortId sortDefName), DefSort (SortDef [isEqualFuncId]) )  ] ++ 
                        -- Constructors
                        (map (\(constrName, types) -> (IdCstr (expectCstrId constrName types sortDefName), DefCstr (CstrDef (isConstrFuncId constrName) (isAccessorsFuncIds types)) ) ) constrs )
                     ) 

createConstDef :: String -> String -> FuncContent -> String
createConstDef name sort content =  "CONSTDEF " ++ name ++ " :: " ++ sort ++ " ::=\n" ++
                                            (pshow (vexpr content))     -- ident ?
                                        ++ "\nENDDEF"

expectConstDef :: String -> String -> VExpr -> TxsDefs
expectConstDef name sort content = TxsDefs.fromList( [(IdFunc (expectFuncId name [] sort), DefFunc (FuncDef (fromTypedElementsToVarIds []) content))] )

                                        
createFuncDef :: String -> TypedElements -> String -> FuncContent -> String
createFuncDef name vars sort content =  "FUNCDEF " ++ name ++ 
                                            " ( " ++ ( Utils.join " ; " (map (\(var,t) -> (Utils.join ", " var) ++ " :: " ++ t) vars) ) ++ " ) :: " ++ sort ++ " ::=\n" ++
                                            (pshow (vexpr content))     -- ident ?
                                        ++ "\nENDDEF"
                                            
expectFuncDef :: String -> TypedElements -> String -> FuncContent -> TxsDefs
expectFuncDef name args sort content = TxsDefs.fromList( [(IdFunc (expectFuncId name args sort), DefFunc (FuncDef (fromTypedElementsToVarIds args) (vexpr content)))] )

createProcDef :: String -> TypedElements -> TypedElements -> Maybe [String] -> BExpr -> String
createProcDef name chans vars Nothing content = "PROCDEF " ++ name ++ 
                                            " [ " ++ ( Utils.join " ; " (map (\(chan,t) -> t ++ " :: " ++ (Utils.join ", " chan)) chans) ) ++ " ] " ++
                                            " ( " ++ ( Utils.join " ; " (map (\(var,t) -> (Utils.join ", " var) ++ " :: " ++ t) vars) ) ++ " ) " ++ 
                                            " ::=\n" ++
                                            (pshow content)        -- ident ?
                                            ++ "\nENDDEF"

createProcDef name chans vars (Just exits) content = "PROCDEF " ++ name ++ 
                                            " [ " ++ ( Utils.join " ; " (map (\(chan,t) -> t ++ " :: " ++ (Utils.join ", " chan)) chans) ) ++ " ] " ++
                                            " ( " ++ ( Utils.join " ; " (map (\(var,t) -> (Utils.join ", " var) ++ " :: " ++ t) vars) ) ++ " ) " ++ 
                                            " EXIT ( " ++ ( Utils.join " , " exits) ++ " ) " ++ 
                                            " ::=\n" ++
                                            (pshow content)        -- ident ?
                                            ++ "\nENDDEF"

expectProcDef :: String -> TypedElements -> TypedElements -> Maybe [String] -> BExpr -> TxsDefs
expectProcDef name chans vars exits content = TxsDefs.fromList( [(  IdProc  (expectProcId name chans vars exits), 
                                                                DefProc (ProcDef (fromTypedElementsToChanIds chans)
                                                                                 (fromTypedElementsToVarIds vars) 
                                                                                 content
                                                                        )
                                                              )] )

                                            
                                            
expectProcId :: String -> TypedElements -> TypedElements -> Maybe [String] -> ProcId
expectProcId name chans vars exits = ProcId name 
                                     dontCareUnid
                                     (fromTypedElementsToChanIds chans)
                                     (fromTypedElementsToVarIds vars)
                                     (fromMaybeTypeToMaybeSortIds exits)


expectFuncId :: String -> TypedElements -> String -> FuncId
expectFuncId funcname funcargs funcsort = FuncId funcname dontCareUnid (fromTypedElementsToSortIds funcargs) (expectSortId funcsort)

expectVarId :: String -> String -> VarId
expectVarId varname varsort = VarId varname dontCareUnid (expectSortId varsort)

expectChanId :: String -> [String] -> ChanId
expectChanId name sorts = ChanId name dontCareUnid (map (\x -> expectSortId x) sorts)


findValueOfGenericKeyAssocs :: Ident -> [(Ident,TxsDef)] -> Maybe TxsDef
findValueOfGenericKeyAssocs key []                                                       = Nothing
findValueOfGenericKeyAssocs (IdSort key1) ((IdSort key2, sdef@DefSort{}):xs) =
                                                            if (identicalSortId key1 key2)
                                                               then Just sdef
                                                               else findValueOfGenericKeyAssocs (IdSort key1) xs 
findValueOfGenericKeyAssocs (IdCstr key1) ((IdCstr key2, cdef@DefCstr{}):xs) =
                                                            if (identicalCstrId key1 key2)
                                                               then Just cdef
                                                               else findValueOfGenericKeyAssocs (IdCstr key1) xs 
findValueOfGenericKeyAssocs (IdFunc key1) ((IdFunc key2, fdef@DefFunc{}):xs) =
                                                            if (identicalFuncId key1 key2)
                                                                then Just fdef
                                                                else findValueOfGenericKeyAssocs (IdFunc key1) xs 
findValueOfGenericKeyAssocs key (x:xs) = findValueOfGenericKeyAssocs key xs                                     

                                
findValueOfGenericKey :: Ident -> TxsDefs -> Maybe TxsDef
findValueOfGenericKey key txsdefs = findValueOfGenericKeyAssocs key (TxsDefs.toList txsdefs)



type FuncKey = FuncId

getFuncContent :: String -> FuncKey -> FuncContent
getFuncContent torXakisInput funcKey =
        --Trace.trace ("torXakisInput = \n" ++ torXakisInput) $ 
        --Trace.trace ("funcKey = \n" ++ show(funcKey)) $
            case findValueOfGenericKey (IdFunc funcKey) ( parseTorXakis torXakisInput ) of
                Just (DefFunc (FuncDef _ vexp) )    -> FuncContent vexp
                Nothing                             -> error "Test Error: func name not found"
                Just another                        -> error "Test Error: func name not a FuncDef"

getFuncKey :: String -> TypedElements -> String -> FuncKey
getFuncKey = expectFuncId


constantInt :: Integer -> FuncContent 
constantInt n = FuncContent (cstrConst (Cint n))

constantString :: String -> FuncContent 
constantString s = FuncContent (cstrConst (Cstring s))

constantBool :: String -> FuncContent
constantBool s = FuncContent (cstrConst (Cbool ("True" == s)))

var :: String -> String -> FuncContent
var name sort = FuncContent (cstrVar (expectVarId name sort) )

-- user must ensure first argument bool, then and else part same type
ite :: FuncContent -> FuncContent -> FuncContent -> FuncContent
ite condition thenPart elsePart = FuncContent (cstrIte [vexpr condition] (vexpr thenPart) (vexpr elsePart))

-- user must assert only variables are used as keys
subst :: Map.Map FuncContent FuncContent -> FuncContent -> FuncContent
subst mapFF content = FuncContent (cstrEnv (Map.fromList (map (\(FuncContent (view -> Vvar v), FuncContent y) -> (v,y)) (Map.toList mapFF))) (vexpr content))

functionCall :: FuncKey -> [FuncContent] -> FuncContent
functionCall funcKey args = FuncContent (cstrFunc funcKey (map vexpr args))

