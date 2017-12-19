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

, funcIdBoolToString
, funcIdBoolFromString
, funcIdBoolToXml
, funcIdBoolFromXml

, funcIdIntToString
, funcIdIntFromString
, funcIdIntToXml
, funcIdIntFromXml

, funcIdStringToString
, funcIdStringFromString
, funcIdStringToXml
, funcIdStringFromXml
, funcIdtakeWhile
, funcIdtakeWhileNot
, funcIddropWhile
, funcIddropWhileNot

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
    , (toStringName, Map.fromList [ ( Signature [sortIdBool]    sortIdString, cstrPredef SSB funcIdBoolToString )
                                  , ( Signature [sortIdInt]     sortIdString, cstrPredef SSI funcIdIntToString )
                                  , ( Signature [sortIdString]  sortIdString, cstrPredef SSS funcIdStringToString )
                                  ] )
    , (fromStringName, Map.fromList [ ( Signature [sortIdString]  sortIdBool,   cstrPredef SSB funcIdBoolFromString )
                                    , ( Signature [sortIdString]  sortIdInt,    cstrPredef SSI funcIdIntFromString )
                                    , ( Signature [sortIdString]  sortIdString, cstrPredef SSS funcIdStringFromString )
                                    ] )
    , (toXmlName, Map.fromList [ ( Signature [sortIdBool]    sortIdString, cstrPredef SSB funcIdBoolToXml )
                               , ( Signature [sortIdInt]     sortIdString, cstrPredef SSI funcIdIntToXml )
                               , ( Signature [sortIdString]  sortIdString, cstrPredef SSS funcIdStringToXml )
                               ] )
    , (fromXmlName, Map.fromList [ ( Signature [sortIdString]  sortIdBool,     cstrPredef SSB funcIdBoolFromXml )
                                 , ( Signature [sortIdString]  sortIdInt,      cstrPredef SSI funcIdIntFromXml )
                                 , ( Signature [sortIdString]  sortIdString,   cstrPredef SSS funcIdStringFromXml )
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
    , ("takeWhile",    Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcIdtakeWhile ) ] )
    , ("takeWhileNot", Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcIdtakeWhileNot ) ] )
    , ("dropWhile",    Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcIddropWhile ) ] )
    , ("dropWhileNot", Map.fromList [ ( Signature [sortIdString,sortIdString] sortIdString, cstrPredef SSS funcIddropWhileNot ) ] )

    , ("strinre",   Map.fromList [ ( Signature [sortIdString,sortIdRegex] sortIdBool, twoArgumentHandler cstrStrInRe ) ] )

    ] )

-- ----------------------------------------------------------------------------------------- --
-- SSB :  Standard Sort Bool


funcIdBoolToString :: FuncId
funcIdBoolToString     = FuncId toStringName   213 [sortIdBool]             sortIdString
funcIdBoolFromString :: FuncId
funcIdBoolFromString   = FuncId fromStringName 214 [sortIdString]           sortIdBool

funcIdBoolToXml :: FuncId
funcIdBoolToXml        = FuncId toXmlName      215 [sortIdBool]             sortIdString
funcIdBoolFromXml :: FuncId
funcIdBoolFromXml      = FuncId fromXmlName    216 [sortIdString]           sortIdBool

stdFuncDefsBool' :: [ ( FuncId, FuncDef VarId) ]
stdFuncDefsBool'
  =  [ ( funcIdBoolToString,   let x = VarId "x" 243 sortIdBool
                                    in FuncDef [x] (cstrPredef SSB funcIdBoolToString [cstrVar x]) )
     , ( funcIdBoolFromString, let x = VarId "x" 244 sortIdString
                                    in FuncDef [x] (cstrPredef SSB funcIdBoolFromString [cstrVar x]) )
     , ( funcIdBoolToXml,      let x = VarId "x" 245 sortIdBool
                                    in FuncDef [x] (cstrPredef SSB funcIdBoolToXml [cstrVar x]) )
     , ( funcIdBoolFromXml,    let x = VarId "x" 246 sortIdString
                                    in FuncDef [x] (cstrPredef SSB funcIdBoolFromXml [cstrVar x]) )
     ]

stdFuncDefsBool :: [ ( Ident, TxsDef ) ]
stdFuncDefsBool = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsBool'

-- ----------------------------------------------------------------------------------------- --
-- SSI :  Standard Sort Int

funcIdIntToString :: FuncId
funcIdIntToString      = FuncId toStringName       301 [sortIdInt]            sortIdString
funcIdIntFromString :: FuncId
funcIdIntFromString    = FuncId fromStringName     302 [sortIdString]         sortIdInt

funcIdIntToXml :: FuncId
funcIdIntToXml         = FuncId toXmlName          303 [sortIdInt]            sortIdString
funcIdIntFromXml :: FuncId
funcIdIntFromXml       = FuncId fromXmlName        304 [sortIdString]         sortIdInt


stdFuncDefsInt' :: [ ( FuncId, FuncDef VarId) ]
stdFuncDefsInt'
  =  [ ( funcIdIntToString,    let x = VarId "x" 341 sortIdInt
                                    in FuncDef [x] (cstrPredef SSI funcIdIntToString [cstrVar x]) )
     , ( funcIdIntFromString,  let x = VarId "x" 342 sortIdString
                                    in FuncDef [x] (cstrPredef SSI funcIdIntFromString [cstrVar x]) )
     , ( funcIdIntToXml,       let x = VarId "x" 343 sortIdInt
                                    in FuncDef [x] (cstrPredef SSI funcIdIntToXml [cstrVar x]) )
     , ( funcIdIntFromXml,     let x = VarId "x" 344 sortIdString
                                    in FuncDef [x] (cstrPredef SSI funcIdIntFromXml [cstrVar x]) )
     ]

stdFuncDefsInt :: [ ( Ident, TxsDef ) ]
stdFuncDefsInt = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsInt'
-- ----------------------------------------------------------------------------------------- --
-- SSS :  Standard Sort String

funcIdStringToString :: FuncId
funcIdStringToString       = FuncId toStringName       525 [sortIdString]                 sortIdString
funcIdStringFromString :: FuncId
funcIdStringFromString     = FuncId fromStringName     526 [sortIdString]                 sortIdString
 
funcIdStringToXml :: FuncId
funcIdStringToXml          = FuncId toXmlName          527 [sortIdString]                 sortIdString
funcIdStringFromXml :: FuncId
funcIdStringFromXml        = FuncId fromXmlName        528 [sortIdString]                 sortIdString

funcIdtakeWhile :: FuncId
funcIdtakeWhile            = FuncId "takeWhile"        533 [sortIdString,sortIdString]   sortIdString
funcIdtakeWhileNot:: FuncId
funcIdtakeWhileNot         = FuncId "takeWhileNot"     534 [sortIdString,sortIdString]   sortIdString
funcIddropWhile :: FuncId
funcIddropWhile            = FuncId "dropWhile"        535 [sortIdString,sortIdString]   sortIdString
funcIddropWhileNot :: FuncId
funcIddropWhileNot         = FuncId "dropWhileNot"     536 [sortIdString,sortIdString]   sortIdString

stdFuncDefsString' :: [ ( FuncId, FuncDef VarId) ]
stdFuncDefsString'
  =  [
       ( funcIdStringToString,     let { s = VarId "s" 543 sortIdString }
                                        in FuncDef [s] (cstrPredef SSS funcIdStringToString [cstrVar s]) )
     , ( funcIdStringFromString,   let { r = VarId "r" 544 sortIdString }
                                        in FuncDef [r] (cstrPredef SSS funcIdStringFromString [cstrVar r]) )
     , ( funcIdStringToXml,        let { s = VarId "s" 545 sortIdString }
                                        in FuncDef [s] (cstrPredef SSS funcIdStringToXml [cstrVar s]) )
     , ( funcIdStringFromXml,      let { r = VarId "r" 546 sortIdString }
                                        in FuncDef [r] (cstrPredef SSS funcIdStringFromXml [cstrVar r]) )
     , ( funcIdtakeWhile
       , let { x = VarId "x" 554 sortIdString
             ; y = VarId "y" 555 sortIdString
             }
          in FuncDef [x,y] (cstrPredef SSS funcIdtakeWhile [cstrVar x,cstrVar y])
       )
     , ( funcIdtakeWhileNot
       , let { x = VarId "x" 556 sortIdString
             ; y = VarId "y" 557 sortIdString
             }
          in FuncDef [x,y] (cstrPredef SSS funcIdtakeWhileNot [cstrVar x,cstrVar y])
       )
     , ( funcIddropWhile
       , let { x = VarId "x" 558 sortIdString
             ; y = VarId "y" 559 sortIdString
             }
          in FuncDef [x,y] (cstrPredef SSS funcIddropWhile [cstrVar x,cstrVar y])
       )
     , ( funcIddropWhileNot
       , let { x = VarId "x" 560 sortIdString
             ; y = VarId "y" 561 sortIdString
             }
          in FuncDef [x,y] (cstrPredef SSS funcIddropWhileNot [cstrVar x,cstrVar y])
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
 