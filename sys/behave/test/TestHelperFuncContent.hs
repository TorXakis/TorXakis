{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module TestHelperFuncContent

where
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.String.Utils as Utils
import qualified Data.Text         as T

import           ChanId
import           Constant
import           CstrDef
import           CstrId
import           FreeMonoidX
import           FuncDef
import           FuncId
import           Id
import           ProcId
import           Sigs
import           SortDef
import           SortId
import           StdTDefs
import           TorXakis.Compiler (compileLegacy)
import           TxsDefs           hiding (vexpr)
import           TxsShow
import           ValExpr
import           VarId

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

dontCareUnid :: Id
dontCareUnid = 0

dontCareName :: String
dontCareName = []

type TypedElement = ([String],String)
type TypedElements = [ TypedElement ]

type Constructor = (String, TypedElements)
type Constructors = [ Constructor ]

identicalLists :: (a-> a-> Bool) -> [a] -> [a] -> Bool
identicalLists _ [] [] = True
identicalLists identicalItems (x1:xs1) (x2:xs2) = identicalItems x1 x2 && identicalLists identicalItems xs1 xs2
identicalLists _ _ _ = False            -- different length

identicalExitSort :: ExitSort -> ExitSort -> Bool
identicalExitSort NoExit NoExit       = True
identicalExitSort (Exit s1) (Exit s2) = identicalLists identicalSortId s1 s2
identicalExitSort _ _                 = False

identicalChanId :: ChanId -> ChanId -> Bool
identicalChanId (ChanId "EXIT" _ _) (ChanId "EXIT" _ _) = True
identicalChanId (ChanId name1 _ chansorts1) (ChanId name2 _ chansorts2) =
       name1 == name2
    && identicalLists identicalSortId chansorts1 chansorts2

identicalChanSort :: ChanSort -> ChanSort -> Bool
identicalChanSort (ChanSort s1) (ChanSort s2) =
    identicalLists identicalSortId s1 s2

identicalProcId :: ProcId -> ProcId -> Bool
identicalProcId (ProcId name1 _ chanids1 varids1 exitSort1) (ProcId name2 _ chanids2 varids2 exitSort2) =
       name1 == name2
    && identicalLists identicalChanSort chanids1 chanids2
    && identicalLists identicalSortId varids1 varids2
    && identicalExitSort exitSort1 exitSort2

identicalFuncId :: FuncId -> FuncId -> Bool
identicalFuncId (FuncId name1 _ funcargs1 funcsort1) (FuncId name2 _ funcargs2 funcsort2) =       name1 == name2
                                                                                               && identicalLists identicalSortId funcargs1 funcargs2
                                                                                               && identicalSortId funcsort1 funcsort2

identicalCstrId :: CstrId -> CstrId -> Bool
identicalCstrId (CstrId name1 _ cstrargs1 cstrsort1) (CstrId name2 _ cstrargs2 cstrsort2) =       name1 == name2
                                                                                               && identicalLists identicalSortId cstrargs1 cstrargs2
                                                                                               && identicalSortId cstrsort1 cstrsort2

identicalVarId :: VarId -> VarId -> Bool
identicalVarId (VarId name1 _ sortid1) (VarId name2 _ sortid2) = name1 == name2 && identicalSortId sortid1 sortid2

identicalSortId :: SortId -> SortId -> Bool
identicalSortId s1 s2 = SortId.name s1 == SortId.name s2

identicalMap :: VarEnv VarId VarId -> VarEnv VarId VarId -> Bool
identicalMap map1 map2 = containsAllIdentical (Map.toList map1) (Map.toList map2)

containsAllIdentical :: [(VarId, VExpr)] -> [(VarId, VExpr)] -> Bool
containsAllIdentical [] _ = True
containsAllIdentical (x:xs) s = containsIdentical x s && containsAllIdentical xs s

containsIdentical :: (VarId, VExpr) -> [(VarId, VExpr)] -> Bool
containsIdentical _ [] = False
containsIdentical x1@(k1,v1) ((k2,v2):xs) =    ( identicalVarId k1 k2 && identicalVExpr v1 v2 )
                                            || containsIdentical x1 xs

identicalVExpr :: VExpr -> VExpr -> Bool
identicalVExpr (ValExpr.view -> Vfunc fid1 vexps1)        (ValExpr.view -> Vfunc fid2 vexps2)        = identicalFuncId fid1 fid2 && identicalLists identicalVExpr vexps1 vexps2
identicalVExpr (ValExpr.view -> Vcstr cid1 vexps1)        (ValExpr.view -> Vcstr cid2 vexps2)        = identicalCstrId cid1 cid2 && identicalLists identicalVExpr vexps1 vexps2
identicalVExpr (ValExpr.view -> Viscstr cid1 vexp1)       (ValExpr.view -> Viscstr cid2 vexp2)       = identicalCstrId cid1 cid2 && identicalVExpr vexp1 vexp2
identicalVExpr (ValExpr.view -> Vaccess cid1 n1 p1 vexp1) (ValExpr.view -> Vaccess cid2 n2 p2 vexp2) = identicalCstrId cid1 cid2 && n1 == n2 && p1 == p2 && identicalVExpr vexp1 vexp2
identicalVExpr (ValExpr.view -> Vconst c1)                (ValExpr.view -> Vconst c2)                = c1 == c2
identicalVExpr (ValExpr.view -> Vvar v1)                  (ValExpr.view -> Vvar v2)                  = identicalVarId v1 v2
identicalVExpr (ValExpr.view -> Vite vc1 vt1 ve1)         (ValExpr.view -> Vite vc2 vt2 ve2)         = identicalVExpr vc1 vc2 && identicalVExpr vt1 vt2 && identicalVExpr ve1 ve2
identicalVExpr (ValExpr.view -> Vequal vl1 vr1)           (ValExpr.view -> Vequal vl2 vr2)           = identicalVExpr vl1 vl2  && identicalVExpr vr1 vr2
identicalVExpr (ValExpr.view -> Vnot v1)                  (ValExpr.view -> Vnot v2)                  = identicalVExpr v1 v2
identicalVExpr (ValExpr.view -> Vand vs1)                 (ValExpr.view -> Vand vs2)                 = identicalLists identicalVExpr (Set.toAscList vs1) (Set.toAscList vs2)
identicalVExpr (ValExpr.view -> Vdivide t1 n1)            (ValExpr.view -> Vdivide t2 n2)            = identicalVExpr t1 t2  && identicalVExpr n1 n2
identicalVExpr (ValExpr.view -> Vmodulo t1 n1)            (ValExpr.view -> Vmodulo t2 n2)            = identicalVExpr t1 t2  && identicalVExpr n1 n2
identicalVExpr (ValExpr.view -> Vsum s1)                  (ValExpr.view -> Vsum s2)                  = let l1 = toOccurListT s1
                                                                                                           l2 = toOccurListT s2
                                                                                                         in identicalLists (\e1 e2 -> snd e1 == snd e2 && identicalVExpr (fst e1) (fst e2)) l1 l2
identicalVExpr (ValExpr.view -> Vproduct s1)              (ValExpr.view -> Vproduct s2)              = let l1 = toOccurListT s1
                                                                                                           l2 = toOccurListT s2
                                                                                                         in identicalLists (\e1 e2 -> snd e1 == snd e2 && identicalVExpr (fst e1) (fst e2)) l1 l2
identicalVExpr (ValExpr.view -> Vgez v1)                  (ValExpr.view -> Vgez v2)                  = identicalVExpr v1 v2
identicalVExpr (ValExpr.view -> Vlength v1)               (ValExpr.view -> Vlength v2)               = identicalVExpr v1 v2
identicalVExpr (ValExpr.view -> Vat s1 p1)                (ValExpr.view -> Vat s2 p2)                = identicalVExpr s1 s2 && identicalVExpr p1 p2
identicalVExpr (ValExpr.view -> Vconcat vs1)              (ValExpr.view -> Vconcat vs2)              = identicalLists identicalVExpr vs1 vs2
identicalVExpr (ValExpr.view -> Vstrinre s1 r1)           (ValExpr.view -> Vstrinre s2 r2)           = identicalVExpr s1 s2 && identicalVExpr r1 r2
identicalVExpr (ValExpr.view -> Vpredef p1 fid1 vexps1)   (ValExpr.view -> Vpredef p2 fid2 vexps2)   = p1 == p2 && identicalFuncId fid1 fid2 && identicalLists identicalVExpr vexps1 vexps2
identicalVExpr _                                          _                                          = False                          -- different

identicalActOffer :: ActOffer -> ActOffer -> Bool
identicalActOffer (ActOffer offers1 _hidvars1 vexpr1) (ActOffer offers2 _hidvars2 vexpr2) =    identicalLists identicalOffer (Set.toAscList offers1) (Set.toAscList offers2)
                                                                        && identicalVExpr vexpr1 vexpr2
identicalOffer :: Offer -> Offer -> Bool
identicalOffer (Offer cid1 chanoffers1) (Offer cid2 chanoffers2)        =    identicalChanId cid1 cid2
                                                                          && identicalLists identicalChanOffer chanoffers1 chanoffers2

identicalChanOffer :: ChanOffer -> ChanOffer -> Bool
identicalChanOffer (Quest vid1) (Quest vid2)                    = identicalVarId vid1 vid2
identicalChanOffer (Exclam vexpr1) (Exclam vexpr2)              = identicalVExpr vexpr1 vexpr2
identicalChanOffer _ _                                          = False

identicalBExpr :: BExpr -> BExpr -> Bool
identicalBExpr b1 b2 = identicalBExpr' (TxsDefs.view b1) (TxsDefs.view b2)

identicalBExpr' :: BExprView -> BExprView -> Bool
identicalBExpr' (ActionPref actOffer1 bExpr1) (ActionPref actOffer2 bExpr2)   =    identicalActOffer actOffer1 actOffer2
                                                                                && identicalBExpr bExpr1 bExpr2
identicalBExpr' (Guard vexpr1 bExpr1) (Guard vexpr2 bExpr2)                   =    identicalVExpr vexpr1 vexpr2
                                                                                && identicalBExpr bExpr1 bExpr2
identicalBExpr' (Choice bExprs1) (Choice bExprs2)                             =    identicalLists identicalBExpr (Set.toAscList bExprs1) (Set.toAscList bExprs2)
identicalBExpr' (Parallel chanids1 bExprs1) (Parallel chanids2 bExprs2)       =    identicalLists identicalChanId (Set.toAscList chanids1) (Set.toAscList chanids2)
                                                                                && identicalLists identicalBExpr bExprs1 bExprs2      -- Set would be better -> Position in list is irrelevant
identicalBExpr' (Enable bexpr11 chanoffers1 bexpr12) (Enable bexpr21 chanoffers2 bexpr22) =    identicalBExpr bexpr11 bexpr21
                                                                                            && identicalLists identicalChanOffer chanoffers1 chanoffers2
                                                                                            && identicalBExpr bexpr12 bexpr22
identicalBExpr' (Disable bexpr11 bexpr12) (Disable bexpr21 bexpr22)           =    identicalBExpr bexpr11 bexpr21
                                                                                && identicalBExpr bexpr12 bexpr22
identicalBExpr' (Interrupt bexpr11 bexpr12) (Interrupt bexpr21 bexpr22)       =    identicalBExpr bexpr11 bexpr21
                                                                                && identicalBExpr bexpr12 bexpr22
identicalBExpr' (ProcInst pid1 chans1 vexprs1) (ProcInst pid2 chans2 vexprs2) =    identicalProcId pid1 pid2
                                                                                && identicalLists identicalChanId chans1 chans2
                                                                                && identicalLists identicalVExpr vexprs1 vexprs2
identicalBExpr' (Hide chans1 bexpr1) (Hide chans2 bexpr2)                     =    identicalLists identicalChanId (Set.toAscList chans1) (Set.toAscList chans2)
                                                                                && identicalBExpr bexpr1 bexpr2
identicalBExpr' (ValueEnv _mp1 _bexpr1) (ValueEnv _mp2 _bexpr2)               = error "TODO - identicalBExpr - ValueEnv"
identicalBExpr' (StAut _sid1 _mp1 _trans1) (StAut _sid2 _mp2 _trans2)         = error "TODO - identicalBExpr - StAut"
identicalBExpr' _ _                                                           = False

newtype  FuncContent =  FuncContent { vexpr :: VExpr }
    deriving (Ord,Read,Show)

instance Eq FuncContent where
    FuncContent x == FuncContent y = identicalVExpr x y

instance SortOf FuncContent where
    sortOf f = sortOf (vexpr f)

toTorXakisDefs :: (Id, TxsDefs, Sigs VarId) -> TxsDefs
toTorXakisDefs (_, b, _) = b

parseTorXakis :: String -> TxsDefs
parseTorXakis txt = -- Trace.trace ("txt = " ++ txt)
                    let parserOutput = compileLegacy txt in
                        -- Trace.trace ("parser output = " ++ show(parserOutput)) $
                            toTorXakisDefs parserOutput

fromTypedElementsToSortIds :: TypedElements -> [SortId]
fromTypedElementsToSortIds = concatMap (\(is,t) -> map (\_ -> expectSortId t) is)

fromMaybeTypeToMaybeSortIds :: Maybe [String] -> ExitSort
fromMaybeTypeToMaybeSortIds Nothing       = NoExit
fromMaybeTypeToMaybeSortIds (Just params) = Exit (map expectSortId params)

fromTypedElementsToVarIds :: TypedElements -> [VarId]
fromTypedElementsToVarIds = concatMap (\(is,t) -> map (`expectVarId` t) is)

fromTypedElementsToVExprs :: TypedElements -> [VExpr]
fromTypedElementsToVExprs = concatMap (\(is,t) -> map (\i -> cstrVar (expectVarId i t) ) is)

fromTypedElementsToChanIds :: TypedElements -> [ChanId]
fromTypedElementsToChanIds = map (\(ts,n) -> expectChanId n ts)

fromTypedElementsToFuncContents :: TypedElements -> [FuncContent]
fromTypedElementsToFuncContents = concatMap (\(is,t) -> map (\i -> FuncContent (cstrVar (expectVarId i t))) is)

createSortId :: String -> String
createSortId nm = nm

expectSortId :: String -> SortId
expectSortId nm = SortId (T.pack nm) dontCareUnid

createCstrId :: String -> TypedElements -> String -> String
createCstrId cstrName fields _sortDefName = cstrName ++ " { "
                                            ++ Utils.join " , " (map (\(field,t) -> Utils.join ", " field ++ " :: " ++ t) fields)
                                            ++ " }"

expectCstrId :: String -> TypedElements -> String -> CstrId
expectCstrId cstrName types sortDefName = CstrId (T.pack cstrName) dontCareUnid (fromTypedElementsToSortIds types) (expectSortId sortDefName)

createSortDef :: String -> Constructors -> String
createSortDef sortDefName constrs = "TYPEDEF " ++ sortDefName ++ " ::=\n\t  " ++
                                            Utils.join "\n\t| " (map (\(constrName, fields) -> createCstrId constrName fields sortDefName) constrs )
                                    ++ "\nENDDEF"

expectSortDef :: String -> Constructors -> TxsDefs
expectSortDef sortDefName constrs =
    let
        funcArgs = [([dontCareName],sortDefName)]
        -- IsConstructor Functions
        isConstrFuncId constrName = expectFuncId ("is"++constrName) funcArgs "Bool"
        -- Accessor Functions for constructor fields
        accessorsFuncIds :: TypedElements -> [ FuncId ]
        accessorsFuncIds = concatMap (\(fields,t) -> map (\field -> expectFuncId field funcArgs t) fields)
     in
        TxsDefs.fromList  ( (IdSort (expectSortId sortDefName), DefSort SortDef)
                          : map (\(constrName, types) -> (IdCstr (expectCstrId constrName types sortDefName), DefCstr (CstrDef (isConstrFuncId constrName) (accessorsFuncIds types)) ) ) constrs
                          )


createConstDef :: String -> String -> FuncContent -> String
createConstDef nm srt content =  "CONSTDEF " ++ nm ++ " :: " ++ srt ++ " ::=\n"
                                  ++ pshow (vexpr content)     -- ident ?
                                  ++ "\nENDDEF"

expectConstDef :: String -> String -> VExpr -> TxsDefs
expectConstDef nm srt content = TxsDefs.fromList [(IdFunc (expectFuncId nm [] srt), DefFunc (FuncDef (fromTypedElementsToVarIds []) content))]


createFuncDef :: String -> TypedElements -> String -> FuncContent -> String
createFuncDef nm vars' srt content =  "FUNCDEF " ++ nm
                                      ++ " ( " ++ Utils.join " ; " (map (\(var',t) -> Utils.join ", " var' ++ " :: " ++ t) vars') ++ " ) :: " ++ srt ++ " ::=\n"
                                      ++ pshow (vexpr content)     -- ident ?
                                      ++ "\nENDDEF"

expectFuncDef :: String -> TypedElements -> String -> FuncContent -> TxsDefs
expectFuncDef nm args' srt content = TxsDefs.fromList [(IdFunc (expectFuncId nm args' srt), DefFunc (FuncDef (fromTypedElementsToVarIds args') (vexpr content)))]

createProcDef :: String -> TypedElements -> TypedElements -> Maybe [String] -> BExpr -> String
createProcDef nm chans vars' exits content = "PROCDEF " ++ nm
                                            ++ " [ " ++ Utils.join " ; " (map (\(chan',t) -> t ++ " :: " ++ Utils.join ", " chan') chans) ++ " ] "
                                            ++ " ( " ++ Utils.join " ; " (map (\(var',t) -> Utils.join ", " var' ++ " :: " ++ t) vars') ++ " ) "
                                            ++ case exits of
                                                    Nothing -> ""
                                                    Just [] -> "EXIT "
                                                    Just l  -> "EXIT " ++ Utils.join " # " l
                                            ++ " ::=\n"
                                            ++ pshow content        -- ident ?
                                            ++ "\nENDDEF"

expectProcDef :: String -> TypedElements -> TypedElements -> Maybe [String] -> BExpr -> TxsDefs
expectProcDef nm chans vars' exits content = TxsDefs.fromList [(  IdProc  (expectProcId nm chans vars' exits),
                                                                   DefProc (ProcDef (fromTypedElementsToChanIds chans)
                                                                                    (fromTypedElementsToVarIds vars')
                                                                                    content
                                                                        )
                                                                )
                                                               ]



expectProcId :: String -> TypedElements -> TypedElements -> Maybe [String] -> ProcId
expectProcId nm chans vars' exits = ProcId (T.pack nm)
                                     dontCareUnid
                                     (toChanSort <$> fromTypedElementsToChanIds chans)
                                     (varsort <$> fromTypedElementsToVarIds vars')
                                     (fromMaybeTypeToMaybeSortIds exits)


expectFuncId :: String -> TypedElements -> String -> FuncId
expectFuncId funcname funcargs' funcsrt = FuncId (T.pack funcname) dontCareUnid (fromTypedElementsToSortIds funcargs') (expectSortId funcsrt)

expectVarId :: String -> String -> VarId
expectVarId varname varsrt = VarId (T.pack varname) dontCareUnid (expectSortId varsrt)

expectChanId :: String -> [String] -> ChanId
expectChanId nm sorts = ChanId (T.pack nm) dontCareUnid (map expectSortId sorts)


findValueOfGenericKeyAssocs :: Ident -> [(Ident,TxsDef)] -> Maybe TxsDef
findValueOfGenericKeyAssocs _key [] = Nothing
findValueOfGenericKeyAssocs (IdSort key1) ((IdSort key2, sdef@DefSort{}):xs) =
                                                            if identicalSortId key1 key2
                                                               then Just sdef
                                                               else findValueOfGenericKeyAssocs (IdSort key1) xs
findValueOfGenericKeyAssocs (IdCstr key1) ((IdCstr key2, cdef@DefCstr{}):xs) =
                                                            if identicalCstrId key1 key2
                                                               then Just cdef
                                                               else findValueOfGenericKeyAssocs (IdCstr key1) xs
findValueOfGenericKeyAssocs (IdFunc key1) ((IdFunc key2, fdef@DefFunc{}):xs) =
                                                            if identicalFuncId key1 key2
                                                                then Just fdef
                                                                else findValueOfGenericKeyAssocs (IdFunc key1) xs
findValueOfGenericKeyAssocs (IdProc key1) ((IdProc key2, pdef@DefProc{}):xs) =
                                                            if identicalProcId key1 key2
                                                                then Just pdef
                                                                else findValueOfGenericKeyAssocs (IdProc key1) xs
findValueOfGenericKeyAssocs key (_x:xs) = findValueOfGenericKeyAssocs key xs


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
                Just _another                       -> error "Test Error: func name not a FuncDef"

getFuncKey :: String -> TypedElements -> String -> FuncKey
getFuncKey = expectFuncId


constantInt :: Integer -> FuncContent
constantInt n = FuncContent (cstrConst (Cint n))

constantString :: String -> FuncContent
constantString = FuncContent . cstrConst . Cstring . T.pack

constantBool :: String -> FuncContent
constantBool s = FuncContent (cstrConst (Cbool ("True" == s)))

varContent :: String -> String -> FuncContent
varContent nm srt = FuncContent (cstrVar (expectVarId nm srt) )

-- user must ensure first argument bool, then and else part same type
ite :: FuncContent -> FuncContent -> FuncContent -> FuncContent
ite condition' thenPart elsePart = FuncContent (cstrITE (vexpr condition') (vexpr thenPart) (vexpr elsePart))

-- user must assert only variables are used as keys
subst :: Map.Map FuncContent FuncContent -> FuncContent -> FuncContent
subst mapFF content = FuncContent (ValExpr.subst (Map.fromList (map (\(FuncContent (ValExpr.view -> Vvar v), FuncContent y) -> (v,y)) (Map.toList mapFF)))
                                                 (Map.empty :: Map.Map FuncId (FuncDef VarId))
                                                 (vexpr content)
                                  )

functionCall :: FuncKey -> [FuncContent] -> FuncContent
functionCall (FuncId "==" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrEqual (vexpr l) (vexpr r))
functionCall (FuncId "<>" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrNot (cstrEqual (vexpr l) (vexpr r) ) )
functionCall (FuncId "toString" _ [si] so) [i] | identicalSortId si sortIdBool && identicalSortId so sortIdString && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrPredef SSB funcIdBoolToString [vexpr i])
functionCall (FuncId "toString" _ [si] so) [i] | identicalSortId si sortIdInt && identicalSortId so sortIdString && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrPredef SSI funcIdIntToString [vexpr i])
functionCall (FuncId "toString" _ [si] so) [i] | identicalSortId si sortIdString && identicalSortId so sortIdString && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrPredef SSS funcIdStringToString [vexpr i])
functionCall (FuncId "fromString" _ [si] so) [i] | identicalSortId si sortIdString && identicalSortId so sortIdBool && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrPredef SSB funcIdBoolFromString [vexpr i])
functionCall (FuncId "fromString" _ [si] so) [i] | identicalSortId si sortIdString && identicalSortId so sortIdInt && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrPredef SSI funcIdIntFromString [vexpr i])
functionCall (FuncId "fromString" _ [si] so) [i] | identicalSortId si sortIdString && identicalSortId so sortIdString && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrPredef SSS funcIdStringFromString [vexpr i])
functionCall (FuncId "not" _ [si] so) [i] | identicalSortId si sortIdBool && identicalSortId so sortIdBool && identicalSortId sortIdBool (sortOf (vexpr i)) =
    FuncContent (cstrNot (vexpr i) )
functionCall (FuncId "/\\" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdBool && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrAnd (Set.fromList [vexpr l, vexpr r]))
functionCall (FuncId "\\/" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdBool && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrOr  (Set.fromList [vexpr l, vexpr r]))
functionCall (FuncId "\\|/" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdBool && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrXor (vexpr l) (vexpr r))
functionCall (FuncId "=>" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdBool && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrImplies (vexpr l) (vexpr r))
functionCall (FuncId "<=>" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdBool && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrEqual (vexpr l) (vexpr r))
functionCall (FuncId "+" _ [si] so) [i] | identicalSortId si sortIdInt && identicalSortId so sortIdInt && identicalSortId si (sortOf (vexpr i)) =
                                          error "This shound't be called"
functionCall (FuncId "+" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdInt && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrPlus (vexpr l) (vexpr r))
functionCall (FuncId "-" _ [si] so) [i] | identicalSortId si sortIdInt && identicalSortId so sortIdInt && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrUnaryMinus (vexpr i))
functionCall (FuncId "-" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdInt && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrMinus (vexpr l) (vexpr r))
functionCall (FuncId "abs" _ [si] so) [i] | identicalSortId si sortIdInt && identicalSortId so sortIdInt && identicalSortId si (sortOf (vexpr i)) =
    FuncContent (cstrAbs (vexpr i))
functionCall (FuncId "*" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdInt && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrTimes (vexpr l) (vexpr r))
functionCall (FuncId "/" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdInt && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrDivide (vexpr l) (vexpr r))
functionCall (FuncId "%" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdInt && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrModulo (vexpr l) (vexpr r))
functionCall (FuncId "<" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrLT (vexpr l) (vexpr r))
functionCall (FuncId "<=" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrLE (vexpr l) (vexpr r))
functionCall (FuncId ">" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrGT (vexpr l) (vexpr r))
functionCall (FuncId ">=" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdInt && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrGE (vexpr l) (vexpr r))
functionCall (FuncId "len" _ [si] so) [i] | identicalSortId si sortIdString && identicalSortId so sortIdInt && identicalSortId sortIdString (sortOf (vexpr i)) =
    FuncContent (cstrLength (vexpr i))
functionCall (FuncId "at" _ [sl,sr] s) [l,r] | identicalSortId sl sortIdString && identicalSortId sr sortIdInt && identicalSortId s sortIdString && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrAt (vexpr l) (vexpr r))
functionCall (FuncId "++" _ [sl,sr] s) [l,r] | sl == sr && identicalSortId sl sortIdString && identicalSortId s sortIdString && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrConcat [vexpr l, vexpr r])
functionCall (FuncId "strinre" _ [sl,sr] s) [l,r] | identicalSortId sl sortIdString && identicalSortId sr sortIdRegex && identicalSortId s sortIdBool && identicalSortId sl (sortOf (vexpr l)) && identicalSortId sr (sortOf (vexpr r)) =
    FuncContent (cstrStrInRe (vexpr l) (vexpr r))

functionCall funcKey args' = FuncContent (cstrFunc (Map.empty :: Map.Map FuncId (FuncDef VarId)) funcKey (map vexpr args'))
