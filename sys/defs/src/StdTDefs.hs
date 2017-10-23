{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Predefined, Standard TorXakis Data Types : Bool, Int, Char, String.
module StdTDefs
( eqName
, neqName
, toStringName
, fromStringName
, toXmlName
, fromXmlName

, funcId_BoolToString
, funcId_BoolFromString
, funcId_BoolToXml
, funcId_BoolFromXml

, funcId_IntToString
, funcId_IntFromString
, funcId_IntToXml
, funcId_IntFromXml

, funcId_StringToString
, funcId_StringFromString
, funcId_StringToXml
, funcId_StringFromXml
, funcId_takeWhile
, funcId_takeWhileNot
, funcId_dropWhile
, funcId_dropWhileNot

, equalHandler
, notEqualHandler
, cstrHandler
, iscstrHandler
, accessHandler

, stdSortTable
, stdFuncTable
, stdTDefs

, chanId_Exit
, chanId_Istep
, chanId_Qstep
, chanId_Hit
, chanId_Miss
, module SortOf
)
where

import           Control.Arrow         ((***))
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified GHC.Exts              as Exts

import           ChanId
import           CstrId
import qualified FreeMonoidX           as FMX
import           FuncDef
import           FuncId
import           FuncTable
import           Ident
import           SortDef
import           SortId
import           SortOf
import           TxsDef
import           ValExprDefs
import           ValExprImpls
import           ValExprImplsExtension
import           VarId

stdSortTable :: Map.Map Text SortId
stdSortTable = Map.fromList [ ("Bool",   sortId_Bool)
                            , ("Int",    sortId_Int)
                            , ("String", sortId_String)
                            , ("Regex",  sortId_Regex)
                            ]

stdSortDefs' :: [ ( SortId,       SortDef  ) ]
stdSortDefs' = [ (sortId_Bool,   SortDef)
               , (sortId_Int,    SortDef)
               , (sortId_String, SortDef)
               , (sortId_Regex,  SortDef)
               ]

stdSortDefs :: [ ( Ident,       TxsDef  ) ]
stdSortDefs = map (IdSort Control.Arrow.*** DefSort) stdSortDefs'

-- ----------------------------------------------------------------------------------------- --
-- standard function names

eqName, neqName :: Text
eqName   =  "=="
neqName  =  "<>"

toStringName, fromStringName :: Text
toStringName   = "toString"
fromStringName = "fromString"

toXmlName, fromXmlName :: Text
toXmlName   = "toXml"
fromXmlName = "fromXml"

-- ----------------------------------------------------------------------------------------- --
-- Helper function

oneArgumentHandler :: (ValExpr v -> ValExpr v) -> Handler v
oneArgumentHandler f [a] = f a
oneArgumentHandler _ _   = error "oneArgumentHandler expects one argument"

twoArgumentHandler :: (ValExpr v -> ValExpr v -> ValExpr v) -> Handler v
twoArgumentHandler f [a,b] = f a b
twoArgumentHandler _ _     = error "twoArgumentHandler expects two arguments"

equalHandler :: Ord v => Handler v
equalHandler = twoArgumentHandler cstrEqual

notEqualHandler :: Ord v => Handler v
notEqualHandler = twoArgumentHandler (\a b -> cstrNot (cstrEqual a b))

cstrHandler :: CstrId -> Handler v
cstrHandler = cstrCstr

iscstrHandler :: CstrId -> Handler v
iscstrHandler c = oneArgumentHandler (cstrIsCstr c)

accessHandler :: CstrId -> Int -> Handler v
accessHandler c p = oneArgumentHandler (cstrAccess c p)

-- ----------------------------------------------------------------------------------------- --
-- FuncTable
stdFuncTable :: (Ord v, Integral (ValExpr v)) => FuncTable v
stdFuncTable = FuncTable ( Map.fromList
    [ ( eqName , Map.fromList [ ( Signature [sortId_Bool,     sortId_Bool]    sortId_Bool, equalHandler )
                              , ( Signature [sortId_Int,      sortId_Int]     sortId_Bool, equalHandler )
                              , ( Signature [sortId_String,   sortId_String]  sortId_Bool, equalHandler )
                              ] )
    , ( neqName, Map.fromList [ ( Signature [sortId_Bool,     sortId_Bool]    sortId_Bool, notEqualHandler )
                              , ( Signature [sortId_Int,      sortId_Int]     sortId_Bool, notEqualHandler )
                              , ( Signature [sortId_String,   sortId_String]  sortId_Bool, notEqualHandler )
                              ] )
    , (toStringName, Map.fromList [ ( Signature [sortId_Bool]    sortId_String, cstrPredef SSB funcId_BoolToString )
                                  , ( Signature [sortId_Int]     sortId_String, cstrPredef SSI funcId_IntToString )
                                  , ( Signature [sortId_String]  sortId_String, cstrPredef SSS funcId_StringToString )
                                  ] )
    , (fromStringName, Map.fromList [ ( Signature [sortId_String]  sortId_Bool,   cstrPredef SSB funcId_BoolFromString )
                                    , ( Signature [sortId_String]  sortId_Int,    cstrPredef SSI funcId_IntFromString )
                                    , ( Signature [sortId_String]  sortId_String, cstrPredef SSS funcId_StringFromString )
                                    ] )
    , (toXmlName, Map.fromList [ ( Signature [sortId_Bool]    sortId_String, cstrPredef SSB funcId_BoolToXml )
                               , ( Signature [sortId_Int]     sortId_String, cstrPredef SSI funcId_IntToXml )
                               , ( Signature [sortId_String]  sortId_String, cstrPredef SSS funcId_StringToXml )
                               ] )
    , (fromXmlName, Map.fromList [ ( Signature [sortId_String]  sortId_Bool,     cstrPredef SSB funcId_BoolFromXml )
                                 , ( Signature [sortId_String]  sortId_Int,      cstrPredef SSI funcId_IntFromXml )
                                 , ( Signature [sortId_String]  sortId_String,   cstrPredef SSS funcId_StringFromXml )
                                 ] )
    , ("not",  Map.fromList [ ( Signature [sortId_Bool] sortId_Bool, oneArgumentHandler cstrNot ) ] )
    , ("/\\",  Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, cstrAnd . Set.fromList ) ] )
    , ("\\/",  Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, cstrOr . Set.fromList ) ] )
    , ("\\|/", Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, twoArgumentHandler cstrXor) ] )
    , ("=>",   Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, twoArgumentHandler cstrImplies ) ] )
    , ("<=>",  Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, twoArgumentHandler cstrEqual ) ] )

    , ("+",   Map.fromList [ ( Signature [sortId_Int] sortId_Int, oneArgumentHandler cstrUnaryPlus)
                           , ( Signature [sortId_Int,sortId_Int] sortId_Int, twoArgumentHandler cstrPlus )
                           ] )
    , ("-",   Map.fromList [ ( Signature [sortId_Int] sortId_Int, oneArgumentHandler cstrUnaryMinus )
                           , ( Signature [sortId_Int,sortId_Int] sortId_Int, twoArgumentHandler cstrMinus )
                           ] )
    , ("abs", Map.fromList [ ( Signature [sortId_Int] sortId_Int, oneArgumentHandler cstrAbs ) ] )
    , ("*",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Int, twoArgumentHandler cstrTimes ) ] )
    , ("/",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Int, twoArgumentHandler cstrDivide ) ] )
    , ("%",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Int, twoArgumentHandler cstrModulo ) ] )
    , ("<",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, twoArgumentHandler cstrLT ) ] )
    , ("<=",  Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, twoArgumentHandler cstrLE ) ] )
    , (">",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, twoArgumentHandler cstrGT ) ] )
    , (">=",  Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, twoArgumentHandler cstrGE ) ] )

    , ("len",  Map.fromList [ ( Signature [sortId_String] sortId_Int, oneArgumentHandler cstrLength ) ] )
    , ("at",   Map.fromList [ ( Signature [sortId_String,sortId_Int] sortId_String, twoArgumentHandler cstrAt ) ] )
    , ("++",   Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrConcat ) ] )
    , ("takeWhile",    Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_takeWhile ) ] )
    , ("takeWhileNot", Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_takeWhileNot ) ] )
    , ("dropWhile",    Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_dropWhile ) ] )
    , ("dropWhileNot", Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_dropWhileNot ) ] )

    , ("strinre",   Map.fromList [ ( Signature [sortId_String,sortId_Regex] sortId_Bool, twoArgumentHandler cstrStrInRe ) ] )

    ] )

-- ----------------------------------------------------------------------------------------- --
-- SSB :  Standard Sort Bool


funcId_BoolToString     = FuncId toStringName   213 [sortId_Bool]             sortId_String
funcId_BoolFromString   = FuncId fromStringName 214 [sortId_String]           sortId_Bool

funcId_BoolToXml        = FuncId toXmlName      215 [sortId_Bool]             sortId_String
funcId_BoolFromXml      = FuncId fromXmlName    216 [sortId_String]           sortId_Bool

stdFuncDefsBool' :: [ ( FuncId, FuncDef ) ]
stdFuncDefsBool'
  =  [ ( funcId_BoolToString,   let x = VarId "x" 243 sortId_Bool
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolToString [cstrVar x]) )
     , ( funcId_BoolFromString, let x = VarId "x" 244 sortId_String
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolFromString [cstrVar x]) )
     , ( funcId_BoolToXml,      let x = VarId "x" 245 sortId_Bool
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolToXml [cstrVar x]) )
     , ( funcId_BoolFromXml,    let x = VarId "x" 246 sortId_String
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolFromXml [cstrVar x]) )
     ]

stdFuncDefsBool :: [ ( Ident, TxsDef ) ]
stdFuncDefsBool = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsBool'

-- ----------------------------------------------------------------------------------------- --
-- SSI :  Standard Sort Int


funcId_IntToString      = FuncId toStringName       301 [sortId_Int]            sortId_String
funcId_IntFromString    = FuncId fromStringName     302 [sortId_String]         sortId_Int

funcId_IntToXml         = FuncId toXmlName          303 [sortId_Int]            sortId_String
funcId_IntFromXml       = FuncId fromXmlName        304 [sortId_String]         sortId_Int


stdFuncDefsInt' :: [ ( FuncId, FuncDef ) ]
stdFuncDefsInt'
  =  [ ( funcId_IntToString,    let x = VarId "x" 341 sortId_Int
                                    in FuncDef [x] (cstrPredef SSI funcId_IntToString [cstrVar x]) )
     , ( funcId_IntFromString,  let x = VarId "x" 342 sortId_String
                                    in FuncDef [x] (cstrPredef SSI funcId_IntFromString [cstrVar x]) )
     , ( funcId_IntToXml,       let x = VarId "x" 343 sortId_Int
                                    in FuncDef [x] (cstrPredef SSI funcId_IntToXml [cstrVar x]) )
     , ( funcId_IntFromXml,     let x = VarId "x" 344 sortId_String
                                    in FuncDef [x] (cstrPredef SSI funcId_IntFromXml [cstrVar x]) )
     ]

stdFuncDefsInt :: [ ( Ident, TxsDef ) ]
stdFuncDefsInt = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsInt'
-- ----------------------------------------------------------------------------------------- --
-- SSS :  Standard Sort String

funcId_StringToString       = FuncId toStringName       525 [sortId_String]                 sortId_String
funcId_StringFromString     = FuncId fromStringName     526 [sortId_String]                 sortId_String

funcId_StringToXml          = FuncId toXmlName          527 [sortId_String]                 sortId_String
funcId_StringFromXml        = FuncId fromXmlName        528 [sortId_String]                 sortId_String

funcId_takeWhile            = FuncId "takeWhile"        533 [sortId_String,sortId_String]   sortId_String
funcId_takeWhileNot         = FuncId "takeWhileNot"     534 [sortId_String,sortId_String]   sortId_String
funcId_dropWhile            = FuncId "dropWhile"        535 [sortId_String,sortId_String]   sortId_String
funcId_dropWhileNot         = FuncId "dropWhileNot"     536 [sortId_String,sortId_String]   sortId_String

stdFuncDefsString' :: [ ( FuncId, FuncDef ) ]
stdFuncDefsString'
  =  [
       ( funcId_StringToString,     let { s = VarId "s" 543 sortId_String }
                                        in FuncDef [s] (cstrPredef SSS funcId_StringToString [cstrVar s]) )
     , ( funcId_StringFromString,   let { r = VarId "r" 544 sortId_String }
                                        in FuncDef [r] (cstrPredef SSS funcId_StringFromString [cstrVar r]) )
     , ( funcId_StringToXml,        let { s = VarId "s" 545 sortId_String }
                                        in FuncDef [s] (cstrPredef SSS funcId_StringToXml [cstrVar s]) )
     , ( funcId_StringFromXml,      let { r = VarId "r" 546 sortId_String }
                                        in FuncDef [r] (cstrPredef SSS funcId_StringFromXml [cstrVar r]) )
     , ( funcId_takeWhile
       , let { x = VarId "x" 554 sortId_String
             ; y = VarId "y" 555 sortId_String
             }
          in FuncDef [x,y] (cstrPredef SSS funcId_takeWhile [cstrVar x,cstrVar y])
       )
     , ( funcId_takeWhileNot
       , let { x = VarId "x" 556 sortId_String
             ; y = VarId "y" 557 sortId_String
             }
          in FuncDef [x,y] (cstrPredef SSS funcId_takeWhileNot [cstrVar x,cstrVar y])
       )
     , ( funcId_dropWhile
       , let { x = VarId "x" 558 sortId_String
             ; y = VarId "y" 559 sortId_String
             }
          in FuncDef [x,y] (cstrPredef SSS funcId_dropWhile [cstrVar x,cstrVar y])
       )
     , ( funcId_dropWhileNot
       , let { x = VarId "x" 560 sortId_String
             ; y = VarId "y" 561 sortId_String
             }
          in FuncDef [x,y] (cstrPredef SSS funcId_dropWhileNot [cstrVar x,cstrVar y])
       )
     ]

stdFuncDefsString :: [ ( Ident, TxsDef ) ]
stdFuncDefsString = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsString'

-- * Combined TorXakis standard type definitions
stdTDefs :: [ ( Ident, TxsDef ) ]
stdTDefs =    stdSortDefs
           ++ stdFuncDefsBool
           ++ stdFuncDefsInt
           ++ stdFuncDefsString

-- * Standard channel identifiers
chanId_Exit  = ChanId "EXIT"  901 []
chanId_Istep = ChanId "ISTEP" 902 []
chanId_Qstep = ChanId "QSTEP" 903 []
chanId_Hit   = ChanId "HIT"   904 []
chanId_Miss  = ChanId "MISS"  905 []
