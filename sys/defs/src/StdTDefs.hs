{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  StdTDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Predefined, Standard TorXakis Data Types : Bool, Int, Char, String.
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds    #-}
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

, stdFuncTable

, chanIdExit
, chanIdIstep
, chanIdQstep
, chanIdHit
, chanIdMiss
, module SortOf
)
where

import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Data.Text             (Text)

import           ChanId
import           ConstructorDef
import           FuncId
import           FuncTable
import           Identifier
import           SortDef
import           SortOf
import           StandardSortRefs
import           ValExpr

eqName, neqName :: Text
eqName   =  "=="
neqName  =  "<>"

toStringName, fromStringName :: Text
toStringName   = "toString"
fromStringName = "fromString"

toXmlName, fromXmlName :: Text
toXmlName   = "toXml"
fromXmlName = "fromXml"

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

cstrHandler :: TRef SortDef -> TRef ConstructorDef -> Handler v
cstrHandler = cstrCstr

iscstrHandler :: TRef SortDef -> TRef ConstructorDef -> Handler v
iscstrHandler a c = oneArgumentHandler (cstrIsCstr a c)

accessHandler :: TRef SortDef -> TRef ConstructorDef -> Int -> TRef SortDef -> Handler v
accessHandler a c p s = oneArgumentHandler (cstrAccess a c p s)

stdFuncTable :: (Ord v, Integral (ValExpr v)) => FuncTable v
stdFuncTable = FuncTable ( Map.fromList
    [ ( eqName , Map.fromList [ ( Signature [sortRefBool,     sortRefBool]    sortRefBool, equalHandler )
                              , ( Signature [sortRefInt,      sortRefInt]     sortRefBool, equalHandler )
                              , ( Signature [sortRefString,   sortRefString]  sortRefBool, equalHandler )
                              ] )
    , ( neqName, Map.fromList [ ( Signature [sortRefBool,     sortRefBool]    sortRefBool, notEqualHandler )
                              , ( Signature [sortRefInt,      sortRefInt]     sortRefBool, notEqualHandler )
                              , ( Signature [sortRefString,   sortRefString]  sortRefBool, notEqualHandler )
                              ] )
    , (toStringName, Map.fromList [ ( Signature [sortRefBool]    sortRefString, cstrPredef SSB funcIdBoolToString )
                                  , ( Signature [sortRefInt]     sortRefString, cstrPredef SSI funcIdIntToString )
                                  , ( Signature [sortRefString]  sortRefString, cstrPredef SSS funcIdStringToString )
                                  ] )
    , (fromStringName, Map.fromList [ ( Signature [sortRefString]  sortRefBool,   cstrPredef SSB funcIdBoolFromString )
                                    , ( Signature [sortRefString]  sortRefInt,    cstrPredef SSI funcIdIntFromString )
                                    , ( Signature [sortRefString]  sortRefString, cstrPredef SSS funcIdStringFromString )
                                    ] )
    , (toXmlName, Map.fromList [ ( Signature [sortRefBool]    sortRefString, cstrPredef SSB funcIdBoolToXml )
                               , ( Signature [sortRefInt]     sortRefString, cstrPredef SSI funcIdIntToXml )
                               , ( Signature [sortRefString]  sortRefString, cstrPredef SSS funcIdStringToXml )
                               ] )
    , (fromXmlName, Map.fromList [ ( Signature [sortRefString]  sortRefBool,     cstrPredef SSB funcIdBoolFromXml )
                                 , ( Signature [sortRefString]  sortRefInt,      cstrPredef SSI funcIdIntFromXml )
                                 , ( Signature [sortRefString]  sortRefString,   cstrPredef SSS funcIdStringFromXml )
                                 ] )
    , ("not",  Map.fromList [ ( Signature [sortRefBool] sortRefBool, oneArgumentHandler cstrNot ) ] )
    , ("/\\",  Map.fromList [ ( Signature [sortRefBool,sortRefBool] sortRefBool, cstrAnd . Set.fromList ) ] )
    , ("\\/",  Map.fromList [ ( Signature [sortRefBool,sortRefBool] sortRefBool, cstrOr . Set.fromList ) ] )
    , ("\\|/", Map.fromList [ ( Signature [sortRefBool,sortRefBool] sortRefBool, twoArgumentHandler cstrXor) ] )
    , ("=>",   Map.fromList [ ( Signature [sortRefBool,sortRefBool] sortRefBool, twoArgumentHandler cstrImplies ) ] )
    , ("<=>",  Map.fromList [ ( Signature [sortRefBool,sortRefBool] sortRefBool, twoArgumentHandler cstrEqual ) ] )

    , ("+",   Map.fromList [ ( Signature [sortRefInt] sortRefInt, oneArgumentHandler cstrUnaryPlus)
                           , ( Signature [sortRefInt,sortRefInt] sortRefInt, twoArgumentHandler cstrPlus )
                           ] )
    , ("-",   Map.fromList [ ( Signature [sortRefInt] sortRefInt, oneArgumentHandler cstrUnaryMinus )
                           , ( Signature [sortRefInt,sortRefInt] sortRefInt, twoArgumentHandler cstrMinus )
                           ] )
    , ("abs", Map.fromList [ ( Signature [sortRefInt] sortRefInt, oneArgumentHandler cstrAbs ) ] )
    , ("*",   Map.fromList [ ( Signature [sortRefInt,sortRefInt] sortRefInt, twoArgumentHandler cstrTimes ) ] )
    , ("/",   Map.fromList [ ( Signature [sortRefInt,sortRefInt] sortRefInt, twoArgumentHandler cstrDivide ) ] )
    , ("%",   Map.fromList [ ( Signature [sortRefInt,sortRefInt] sortRefInt, twoArgumentHandler cstrModulo ) ] )
    , ("<",   Map.fromList [ ( Signature [sortRefInt,sortRefInt] sortRefBool, twoArgumentHandler cstrLT ) ] )
    , ("<=",  Map.fromList [ ( Signature [sortRefInt,sortRefInt] sortRefBool, twoArgumentHandler cstrLE ) ] )
    , (">",   Map.fromList [ ( Signature [sortRefInt,sortRefInt] sortRefBool, twoArgumentHandler cstrGT ) ] )
    , (">=",  Map.fromList [ ( Signature [sortRefInt,sortRefInt] sortRefBool, twoArgumentHandler cstrGE ) ] )

    , ("len",  Map.fromList [ ( Signature [sortRefString] sortRefInt, oneArgumentHandler cstrLength ) ] )
    , ("at",   Map.fromList [ ( Signature [sortRefString,sortRefInt] sortRefString, twoArgumentHandler cstrAt ) ] )
    , ("++",   Map.fromList [ ( Signature [sortRefString,sortRefString] sortRefString, cstrConcat ) ] )
    , ("takeWhile",    Map.fromList [ ( Signature [sortRefString,sortRefString] sortRefString, cstrPredef SSS funcIdtakeWhile ) ] )
    , ("takeWhileNot", Map.fromList [ ( Signature [sortRefString,sortRefString] sortRefString, cstrPredef SSS funcIdtakeWhileNot ) ] )
    , ("dropWhile",    Map.fromList [ ( Signature [sortRefString,sortRefString] sortRefString, cstrPredef SSS funcIddropWhile ) ] )
    , ("dropWhileNot", Map.fromList [ ( Signature [sortRefString,sortRefString] sortRefString, cstrPredef SSS funcIddropWhileNot ) ] )

    , ("strinre",   Map.fromList [ ( Signature [sortRefString,sortRefRegex] sortRefBool, twoArgumentHandler cstrStrInRe ) ] )

    ] )

-- ----------------------------------------------------------------------------------------- --
-- SSB :  Standard Sort Bool

funcIdBoolToString :: FuncId
funcIdBoolToString     = FuncId toStringName   213 [sortRefBool]             sortRefString
funcIdBoolFromString :: FuncId
funcIdBoolFromString   = FuncId fromStringName 214 [sortRefString]           sortRefBool

funcIdBoolToXml :: FuncId
funcIdBoolToXml        = FuncId toXmlName      215 [sortRefBool]             sortRefString
funcIdBoolFromXml :: FuncId
funcIdBoolFromXml      = FuncId fromXmlName    216 [sortRefString]           sortRefBool

-- ----------------------------------------------------------------------------------------- --
-- SSI :  Standard Sort Int

funcIdIntToString :: FuncId
funcIdIntToString      = FuncId toStringName       301 [sortRefInt]            sortRefString
funcIdIntFromString :: FuncId
funcIdIntFromString    = FuncId fromStringName     302 [sortRefString]         sortRefInt

funcIdIntToXml :: FuncId
funcIdIntToXml         = FuncId toXmlName          303 [sortRefInt]            sortRefString
funcIdIntFromXml :: FuncId
funcIdIntFromXml       = FuncId fromXmlName        304 [sortRefString]         sortRefInt

-- ----------------------------------------------------------------------------------------- --
-- SSS :  Standard Sort String

funcIdStringToString :: FuncId
funcIdStringToString       = FuncId toStringName       525 [sortRefString]                 sortRefString
funcIdStringFromString :: FuncId
funcIdStringFromString     = FuncId fromStringName     526 [sortRefString]                 sortRefString
 
funcIdStringToXml :: FuncId
funcIdStringToXml          = FuncId toXmlName          527 [sortRefString]                 sortRefString
funcIdStringFromXml :: FuncId
funcIdStringFromXml        = FuncId fromXmlName        528 [sortRefString]                 sortRefString

funcIdtakeWhile :: FuncId
funcIdtakeWhile            = FuncId "takeWhile"        533 [sortRefString,sortRefString]   sortRefString
funcIdtakeWhileNot:: FuncId
funcIdtakeWhileNot         = FuncId "takeWhileNot"     534 [sortRefString,sortRefString]   sortRefString
funcIddropWhile :: FuncId
funcIddropWhile            = FuncId "dropWhile"        535 [sortRefString,sortRefString]   sortRefString
funcIddropWhileNot :: FuncId
funcIddropWhileNot         = FuncId "dropWhileNot"     536 [sortRefString,sortRefString]   sortRefString

-- * Standard channel identifiers
chanIdExit :: ChanId
chanIdExit  = ChanId "EXIT"  901 []
chanIdIstep :: ChanId
chanIdIstep = ChanId "ISTEP" 902 []
chanIdQstep :: ChanId
chanIdQstep = ChanId "QSTEP" 903 []
chanIdHit :: ChanId
chanIdHit   = ChanId "HIT"   904 []
chanIdMiss :: ChanId
chanIdMiss  = ChanId "MISS"  905 []
