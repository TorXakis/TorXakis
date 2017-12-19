{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           ChanId
import           CstrId
import           FuncDef
import           FuncId
import           FuncTable
import           Ident
import           SortDef
import           SortId
import           SortOf
import           TxsDef
import           ValExpr
import           VarId

stdSortTable :: Map.Map Text SortId
stdSortTable = Map.fromList [ ("Bool",   sortIdBool)
                            , ("Int",    sortIdInt)
                            , ("String", sortIdString)
                            , ("Regex",  sortIdRegex)
                            ]

stdSortDefs' :: [ ( SortId,       SortDef  ) ]
stdSortDefs' = [ (sortIdBool,   SortDef)
               , (sortIdInt,    SortDef)
               , (sortIdString, SortDef)
               , (sortIdRegex,  SortDef)
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
    [ ( eqName , Map.fromList [ ( Signature [sortIdBool,     sortIdBool]    sortIdBool, equalHandler )
                              , ( Signature [sortIdInt,      sortIdInt]     sortIdBool, equalHandler )
                              , ( Signature [sortIdString,   sortIdString]  sortIdBool, equalHandler )
                              ] )
    , ( neqName, Map.fromList [ ( Signature [sortIdBool,     sortIdBool]    sortIdBool, notEqualHandler )
                              , ( Signature [sortIdInt,      sortIdInt]     sortIdBool, notEqualHandler )
                              , ( Signature [sortIdString,   sortIdString]  sortIdBool, notEqualHandler )
                              ] )
    , (toStringName, Map.fromList [ ( Signature [sortIdBool]    sortIdString, cstrPredef SSB funcId_BoolToString )
                                  , ( Signature [sortIdInt]     sortIdString, cstrPredef SSI funcId_IntToString )
                                  , ( Signature [sortIdString]  sortIdString, cstrPredef SSS funcId_StringToString )
                                  ] )
    , (fromStringName, Map.fromList [ ( Signature [sortIdString]  sortIdBool,   cstrPredef SSB funcId_BoolFromString )
                                    , ( Signature [sortIdString]  sortIdInt,    cstrPredef SSI funcId_IntFromString )
                                    , ( Signature [sortIdString]  sortIdString, cstrPredef SSS funcId_StringFromString )
                                    ] )
    , (toXmlName, Map.fromList [ ( Signature [sortIdBool]    sortIdString, cstrPredef SSB funcId_BoolToXml )
                               , ( Signature [sortIdInt]     sortIdString, cstrPredef SSI funcId_IntToXml )
                               , ( Signature [sortIdString]  sortIdString, cstrPredef SSS funcId_StringToXml )
                               ] )
    , (fromXmlName, Map.fromList [ ( Signature [sortIdString]  sortIdBool,     cstrPredef SSB funcId_BoolFromXml )
                                 , ( Signature [sortIdString]  sortIdInt,      cstrPredef SSI funcId_IntFromXml )
                                 , ( Signature [sortIdString]  sortIdString,   cstrPredef SSS funcId_StringFromXml )
                                 ] )
    , ("not",  Map.fromList [ ( Signature [sortIdBool] sortIdBool, oneArgumentHandler cstrNot ) ] )
    , ("/\\",  Map.fromList [ ( Signature [sortIdBool,sortIdBool] sortIdBool, cstrAnd . Set.fromList ) ] )
    , ("\\/",  Map.fromList [ ( Signature [sortIdBool,sortIdBool] sortIdBool, cstrOr . Set.fromList ) ] )
    , ("\\|/", Map.fromList [ ( Signature [sortIdBool,sortIdBool] sortIdBool, twoArgumentHandler cstrXor) ] )
    , ("=>",   Map.fromList [ ( Signature [sortIdBool,sortIdBool] sortIdBool, twoArgumentHandler cstrImplies ) ] )
    , ("<=>",  Map.fromList [ ( Signature [sortIdBool,sortIdBool] sortIdBool, twoArgumentHandler cstrEqual ) ] )

    , ("+",   Map.fromList [ ( Signature [sortIdInt] sortIdInt, oneArgumentHandler cstrUnaryPlus)
                           , ( Signature [sortIdInt,sortIdInt] sortIdInt, twoArgumentHandler cstrPlus )
                           ] )
    , ("-",   Map.fromList [ ( Signature [sortIdInt] sortIdInt, oneArgumentHandler cstrUnaryMinus )
                           , ( Signature [sortIdInt,sortIdInt] sortIdInt, twoArgumentHandler cstrMinus )
                           ] )
    , ("abs", Map.fromList [ ( Signature [sortIdInt] sortIdInt, oneArgumentHandler cstrAbs ) ] )
    , ("*",   Map.fromList [ ( Signature [sortIdInt,sortIdInt] sortIdInt, twoArgumentHandler cstrTimes ) ] )
    , ("/",   Map.fromList [ ( Signature [sortIdInt,sortIdInt] sortIdInt, twoArgumentHandler cstrDivide ) ] )
    , ("%",   Map.fromList [ ( Signature [sortIdInt,sortIdInt] sortIdInt, twoArgumentHandler cstrModulo ) ] )
    , ("<",   Map.fromList [ ( Signature [sortIdInt,sortIdInt] sortIdBool, twoArgumentHandler cstrLT ) ] )
    , ("<=",  Map.fromList [ ( Signature [sortIdInt,sortIdInt] sortIdBool, twoArgumentHandler cstrLE ) ] )
    , (">",   Map.fromList [ ( Signature [sortIdInt,sortIdInt] sortIdBool, twoArgumentHandler cstrGT ) ] )
    , (">=",  Map.fromList [ ( Signature [sortIdInt,sortIdInt] sortIdBool, twoArgumentHandler cstrGE ) ] )

    , ("len",  Map.fromList [ ( Signature [sortIdString] sortIdInt, oneArgumentHandler cstrLength ) ] )
    , ("at",   Map.fromList [ ( Signature [sortIdString,sortIdInt] sortIdString, twoArgumentHandler cstrAt ) ] )
    , ("++",   Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrConcat ) ] )
    , ("takeWhile",    Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcId_takeWhile ) ] )
    , ("takeWhileNot", Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcId_takeWhileNot ) ] )
    , ("dropWhile",    Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcId_dropWhile ) ] )
    , ("dropWhileNot", Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcId_dropWhileNot ) ] )

    , ("strinre",   Map.fromList [ ( Signature [sortIdString,sortIdRegex] sortIdBool, twoArgumentHandler cstrStrInRe ) ] )

    ] )

-- ----------------------------------------------------------------------------------------- --
-- SSB :  Standard Sort Bool


funcId_BoolToString :: FuncId
funcId_BoolToString     = FuncId toStringName   213 [sortIdBool]             sortIdString
funcId_BoolFromString :: FuncId
funcId_BoolFromString   = FuncId fromStringName 214 [sortIdString]           sortIdBool

funcId_BoolToXml :: FuncId
funcId_BoolToXml        = FuncId toXmlName      215 [sortIdBool]             sortIdString
funcId_BoolFromXml :: FuncId
funcId_BoolFromXml      = FuncId fromXmlName    216 [sortIdString]           sortIdBool

stdFuncDefsBool' :: [ ( FuncId, FuncDef VarId) ]
stdFuncDefsBool'
  =  [ ( funcId_BoolToString,   let x = VarId "x" 243 sortIdBool
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolToString [cstrVar x]) )
     , ( funcId_BoolFromString, let x = VarId "x" 244 sortIdString
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolFromString [cstrVar x]) )
     , ( funcId_BoolToXml,      let x = VarId "x" 245 sortIdBool
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolToXml [cstrVar x]) )
     , ( funcId_BoolFromXml,    let x = VarId "x" 246 sortIdString
                                    in FuncDef [x] (cstrPredef SSB funcId_BoolFromXml [cstrVar x]) )
     ]

stdFuncDefsBool :: [ ( Ident, TxsDef ) ]
stdFuncDefsBool = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsBool'

-- ----------------------------------------------------------------------------------------- --
-- SSI :  Standard Sort Int

funcId_IntToString :: FuncId
funcId_IntToString      = FuncId toStringName       301 [sortIdInt]            sortIdString
funcId_IntFromString :: FuncId
funcId_IntFromString    = FuncId fromStringName     302 [sortIdString]         sortIdInt

funcId_IntToXml :: FuncId
funcId_IntToXml         = FuncId toXmlName          303 [sortIdInt]            sortIdString
funcId_IntFromXml :: FuncId
funcId_IntFromXml       = FuncId fromXmlName        304 [sortIdString]         sortIdInt


stdFuncDefsInt' :: [ ( FuncId, FuncDef VarId) ]
stdFuncDefsInt'
  =  [ ( funcId_IntToString,    let x = VarId "x" 341 sortIdInt
                                    in FuncDef [x] (cstrPredef SSI funcId_IntToString [cstrVar x]) )
     , ( funcId_IntFromString,  let x = VarId "x" 342 sortIdString
                                    in FuncDef [x] (cstrPredef SSI funcId_IntFromString [cstrVar x]) )
     , ( funcId_IntToXml,       let x = VarId "x" 343 sortIdInt
                                    in FuncDef [x] (cstrPredef SSI funcId_IntToXml [cstrVar x]) )
     , ( funcId_IntFromXml,     let x = VarId "x" 344 sortIdString
                                    in FuncDef [x] (cstrPredef SSI funcId_IntFromXml [cstrVar x]) )
     ]

stdFuncDefsInt :: [ ( Ident, TxsDef ) ]
stdFuncDefsInt = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsInt'
-- ----------------------------------------------------------------------------------------- --
-- SSS :  Standard Sort String

funcId_StringToString :: FuncId
funcId_StringToString       = FuncId toStringName       525 [sortIdString]                 sortIdString
funcId_StringFromString :: FuncId
funcId_StringFromString     = FuncId fromStringName     526 [sortIdString]                 sortIdString
 
funcId_StringToXml :: FuncId
funcId_StringToXml          = FuncId toXmlName          527 [sortIdString]                 sortIdString
funcId_StringFromXml :: FuncId
funcId_StringFromXml        = FuncId fromXmlName        528 [sortIdString]                 sortIdString

funcId_takeWhile :: FuncId
funcId_takeWhile            = FuncId "takeWhile"        533 [sortIdString,sortIdString]   sortIdString
funcId_takeWhileNot:: FuncId
funcId_takeWhileNot         = FuncId "takeWhileNot"     534 [sortIdString,sortIdString]   sortIdString
funcId_dropWhile :: FuncId
funcId_dropWhile            = FuncId "dropWhile"        535 [sortIdString,sortIdString]   sortIdString
funcId_dropWhileNot :: FuncId
funcId_dropWhileNot         = FuncId "dropWhileNot"     536 [sortIdString,sortIdString]   sortIdString

stdFuncDefsString' :: [ ( FuncId, FuncDef VarId) ]
stdFuncDefsString'
  =  [
       ( funcId_StringToString,     let { s = VarId "s" 543 sortIdString }
                                        in FuncDef [s] (cstrPredef SSS funcId_StringToString [cstrVar s]) )
     , ( funcId_StringFromString,   let { r = VarId "r" 544 sortIdString }
                                        in FuncDef [r] (cstrPredef SSS funcId_StringFromString [cstrVar r]) )
     , ( funcId_StringToXml,        let { s = VarId "s" 545 sortIdString }
                                        in FuncDef [s] (cstrPredef SSS funcId_StringToXml [cstrVar s]) )
     , ( funcId_StringFromXml,      let { r = VarId "r" 546 sortIdString }
                                        in FuncDef [r] (cstrPredef SSS funcId_StringFromXml [cstrVar r]) )
     , ( funcId_takeWhile
       , let { x = VarId "x" 554 sortIdString
             ; y = VarId "y" 555 sortIdString
             }
          in FuncDef [x,y] (cstrPredef SSS funcId_takeWhile [cstrVar x,cstrVar y])
       )
     , ( funcId_takeWhileNot
       , let { x = VarId "x" 556 sortIdString
             ; y = VarId "y" 557 sortIdString
             }
          in FuncDef [x,y] (cstrPredef SSS funcId_takeWhileNot [cstrVar x,cstrVar y])
       )
     , ( funcId_dropWhile
       , let { x = VarId "x" 558 sortIdString
             ; y = VarId "y" 559 sortIdString
             }
          in FuncDef [x,y] (cstrPredef SSS funcId_dropWhile [cstrVar x,cstrVar y])
       )
     , ( funcId_dropWhileNot
       , let { x = VarId "x" 560 sortIdString
             ; y = VarId "y" 561 sortIdString
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
chanId_Exit :: ChanId
chanId_Exit  = ChanId "EXIT"  901 []
chanId_Istep :: ChanId
chanId_Istep = ChanId "ISTEP" 902 []
chanId_Qstep :: ChanId
chanId_Qstep = ChanId "QSTEP" 903 []
chanId_Hit :: ChanId
chanId_Hit   = ChanId "HIT"   904 []
chanId_Miss :: ChanId
chanId_Miss  = ChanId "MISS"  905 []
 