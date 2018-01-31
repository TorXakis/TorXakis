{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module HelperVexprToSMT

where
import qualified Data.Map          as Map
import           Data.Monoid
import qualified Data.Set          as Set
import           Data.Text (Text)
import qualified Data.Text         as T

import           ConstDefs
import           FreeMonoidX
import           FuncDef
import           FuncId
import           Identifier
import           Sort
import           ValExpr
import           VarId

import           HelperToSMT

data  TXS2SMTVExprTest         =  TXS2SMTVExprTest  { input    :: ValExpr VarId
                                                    , expected :: Text
                                                    }
     deriving (Eq,Ord,Read,Show)

toFieldName :: Ref ADTDef -> Ref ConstructorDef -> Int -> Text
toFieldName aRef cRef field  = toCstrName aRef cRef <> "$f" <> (T.pack . show) field

toIsCstrName :: Ref ADTDef -> Ref ConstructorDef -> Text
toIsCstrName aRef cRef  =  "is-" <> toCstrName aRef cRef

toCstrName :: Ref ADTDef -> Ref ConstructorDef -> Text
toCstrName aRef cRef  =  "a" <> (T.pack . show . toInt) aRef <> "$c" <> (T.pack . show . toInt) cRef

toADTName :: Ref ADTDef -> Text
toADTName r = "A" <> (T.pack . show . toInt) r

toFuncName :: FuncId -> Text
toFuncName funcId  =  T.concat ["f", (T.pack . show) (FuncId.unid funcId), "$", FuncId.name funcId]

---------------------------------------------------------------------------
-- Vexpr constructors
---------------------------------------------------------------------------
toSMTVar :: VarId -> Text
toSMTVar v = VarId.name v <> "$$" <> T.pack (show $ VarId.unid v)

createIsConstructor :: Ref ADTDef -> Ref ConstructorDef -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createIsConstructor aRef cRef ie =
    TXS2SMTVExprTest (cstrIsCstr aRef cRef (input ie))
                     ("(" <> toIsCstrName aRef cRef <> " " <> expected ie <> ")")

createAccessor :: Ref ADTDef -> Ref ConstructorDef -> Int -> Sort -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createAccessor aRef cRef p s ie =
    TXS2SMTVExprTest (cstrAccess aRef cRef p s (input ie))
                     ("(" <> toFieldName aRef cRef p <> " " <> expected ie <> ")")

createVfunc :: FuncId -> [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createVfunc funcId ies =
   TXS2SMTVExprTest (cstrFunc (Map.empty :: Map.Map FuncId (FuncDef VarId)) funcId (map input ies))
              ("(" <> toFuncName funcId <> " " <> T.intercalate " " (map expected ies) <> ")")

createVsum :: [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createVsum ies =
    TXS2SMTVExprTest (cstrSum (fromListT (map input ies)))
               ("(+ " <> T.intercalate " " (map expected ies) <> ")")

createVconst :: Const -> TXS2SMTVExprTest
createVconst c@(Cint n) =
    if n < 0
        then TXS2SMTVExprTest (cstrConst c) ("(- " <> T.pack (show $ abs n) <> ")")
        else TXS2SMTVExprTest (cstrConst c) $ T.pack $ show n
createVconst c@(Cstring txt) =
    TXS2SMTVExprTest (cstrConst c) ("\"" <> HelperToSMT.escape txt <> "\"")
createVconst _               = error "Not supported / Not possible"

createVvar :: VarId -> TXS2SMTVExprTest
createVvar v =
    TXS2SMTVExprTest (cstrVar v) (toSMTVar v)

createVite :: TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createVite (TXS2SMTVExprTest inputc expectedc) (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    TXS2SMTVExprTest (cstrITE inputc input1 input2)
           ("(ite " <> expectedc <> " " <> expected1 <> " " <> expected2 <> ")")

createVequal :: TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createVequal (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    TXS2SMTVExprTest (cstrEqual input1 input2) ("(= " <> expected1 <> " " <> expected2 <> ")" )    

createVand :: Set.Set TXS2SMTVExprTest -> TXS2SMTVExprTest
createVand conds =
    TXS2SMTVExprTest (cstrAnd (Set.map input conds))
                     ("(and " <> T.intercalate " " (Set.toList (Set.map expected conds)) <> ")")

createVgez :: TXS2SMTVExprTest -> TXS2SMTVExprTest
createVgez (TXS2SMTVExprTest input expected) =
    TXS2SMTVExprTest (cstrGEZ input ) ("(<= 0 " <> expected <> ")" )

----------------------------------------------------------------
-- functions
------------------------------------------------------------------
createUniminusInt :: TXS2SMTVExprTest -> TXS2SMTVExprTest
createUniminusInt ie =
    TXS2SMTVExprTest (cstrUnaryMinus (input ie))
               ("(- " <> expected ie <> ")")
