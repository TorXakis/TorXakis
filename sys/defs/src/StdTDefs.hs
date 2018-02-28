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

import           Data.List.NonEmpty  (fromList)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set

import           ChanId
import           FuncId
import           FuncTable
import           Name
import           Sort
import           SortOf
import           ValExpr

eqName, neqName :: Name
eqName  = fromNonEmpty $ fromList "=="
neqName = fromNonEmpty $ fromList "<>"

toStringName, fromStringName :: Name
toStringName   = fromNonEmpty $ fromList "toString"
fromStringName = fromNonEmpty $ fromList "fromString"

toXmlName, fromXmlName :: Name
toXmlName   = fromNonEmpty $ fromList "toXml"
fromXmlName = fromNonEmpty $ fromList "fromXml"

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

cstrHandler :: Ref (ADTDef Sort) -> Ref (ConstructorDef Sort) -> Handler v
cstrHandler = cstrCstr

iscstrHandler :: Ref (ADTDef Sort) -> Ref (ConstructorDef Sort) -> Handler v
iscstrHandler a c = oneArgumentHandler (cstrIsCstr a c)

accessHandler :: Ref (ADTDef Sort) -> Ref (ConstructorDef Sort) -> Int -> Sort -> Handler v
accessHandler a c p s = oneArgumentHandler (cstrAccess a c p s)

stdFuncTable :: (Ord v, Integral (ValExpr v)) => FuncTable v
stdFuncTable = FuncTable ( HMap.fromList
    [ ( toText eqName , Map.fromList [ ( Signature [SortBool,     SortBool]    SortBool, equalHandler )
                                     , ( Signature [SortInt,      SortInt]     SortBool, equalHandler )
                                     , ( Signature [SortString,   SortString]  SortBool, equalHandler )
                                     ] )
    , ( toText neqName, Map.fromList [ ( Signature [SortBool,     SortBool]    SortBool, notEqualHandler )
                                     , ( Signature [SortInt,      SortInt]     SortBool, notEqualHandler )
                                     , ( Signature [SortString,   SortString]  SortBool, notEqualHandler )
                                     ] )
    , (toText toStringName, Map.fromList [ ( Signature [SortBool]    SortString, cstrPredef SSB funcIdBoolToString )
                                         , ( Signature [SortInt]     SortString, cstrPredef SSI funcIdIntToString )
                                         , ( Signature [SortString]  SortString, cstrPredef SSS funcIdStringToString )
                                         ] )
    , (toText fromStringName, Map.fromList [ ( Signature [SortString]  SortBool,   cstrPredef SSB funcIdBoolFromString )
                                           , ( Signature [SortString]  SortInt,    cstrPredef SSI funcIdIntFromString )
                                           , ( Signature [SortString]  SortString, cstrPredef SSS funcIdStringFromString )
                                           ] )
    , (toText toXmlName, Map.fromList [ ( Signature [SortBool]    SortString, cstrPredef SSB funcIdBoolToXml )
                                      , ( Signature [SortInt]     SortString, cstrPredef SSI funcIdIntToXml )
                                      , ( Signature [SortString]  SortString, cstrPredef SSS funcIdStringToXml )
                                      ] )
    , (toText fromXmlName, Map.fromList [ ( Signature [SortString]  SortBool,     cstrPredef SSB funcIdBoolFromXml )
                                        , ( Signature [SortString]  SortInt,      cstrPredef SSI funcIdIntFromXml )
                                        , ( Signature [SortString]  SortString,   cstrPredef SSS funcIdStringFromXml )
                                        ] )
    , ("not",  Map.fromList [ ( Signature [SortBool] SortBool, oneArgumentHandler cstrNot ) ] )
    , ("/\\",  Map.fromList [ ( Signature [SortBool,SortBool] SortBool, cstrAnd . Set.fromList ) ] )
    , ("\\/",  Map.fromList [ ( Signature [SortBool,SortBool] SortBool, cstrOr . Set.fromList ) ] )
    , ("\\|/", Map.fromList [ ( Signature [SortBool,SortBool] SortBool, twoArgumentHandler cstrXor) ] )
    , ("=>",   Map.fromList [ ( Signature [SortBool,SortBool] SortBool, twoArgumentHandler cstrImplies ) ] )
    , ("<=>",  Map.fromList [ ( Signature [SortBool,SortBool] SortBool, twoArgumentHandler cstrEqual ) ] )

    , ("+",   Map.fromList [ ( Signature [SortInt] SortInt, oneArgumentHandler cstrUnaryPlus)
                           , ( Signature [SortInt,SortInt] SortInt, twoArgumentHandler cstrPlus )
                           ] )
    , ("-",   Map.fromList [ ( Signature [SortInt] SortInt, oneArgumentHandler cstrUnaryMinus )
                           , ( Signature [SortInt,SortInt] SortInt, twoArgumentHandler cstrMinus )
                           ] )
    , ("abs", Map.fromList [ ( Signature [SortInt] SortInt, oneArgumentHandler cstrAbs ) ] )
    , ("*",   Map.fromList [ ( Signature [SortInt,SortInt] SortInt, twoArgumentHandler cstrTimes ) ] )
    , ("/",   Map.fromList [ ( Signature [SortInt,SortInt] SortInt, twoArgumentHandler cstrDivide ) ] )
    , ("%",   Map.fromList [ ( Signature [SortInt,SortInt] SortInt, twoArgumentHandler cstrModulo ) ] )
    , ("<",   Map.fromList [ ( Signature [SortInt,SortInt] SortBool, twoArgumentHandler cstrLT ) ] )
    , ("<=",  Map.fromList [ ( Signature [SortInt,SortInt] SortBool, twoArgumentHandler cstrLE ) ] )
    , (">",   Map.fromList [ ( Signature [SortInt,SortInt] SortBool, twoArgumentHandler cstrGT ) ] )
    , (">=",  Map.fromList [ ( Signature [SortInt,SortInt] SortBool, twoArgumentHandler cstrGE ) ] )

    , ("len",  Map.fromList [ ( Signature [SortString] SortInt, oneArgumentHandler cstrLength ) ] )
    , ("at",   Map.fromList [ ( Signature [SortString,SortInt] SortString, twoArgumentHandler cstrAt ) ] )
    , ("++",   Map.fromList [ ( Signature [SortString,SortString] SortString, cstrConcat ) ] )
    , ("takeWhile",    Map.fromList [ ( Signature [SortString,SortString] SortString, cstrPredef SSS funcIdtakeWhile ) ] )
    , ("takeWhileNot", Map.fromList [ ( Signature [SortString,SortString] SortString, cstrPredef SSS funcIdtakeWhileNot ) ] )
    , ("dropWhile",    Map.fromList [ ( Signature [SortString,SortString] SortString, cstrPredef SSS funcIddropWhile ) ] )
    , ("dropWhileNot", Map.fromList [ ( Signature [SortString,SortString] SortString, cstrPredef SSS funcIddropWhileNot ) ] )

    , ("strinre",   Map.fromList [ ( Signature [SortString,SortRegex] SortBool, twoArgumentHandler cstrStrInRe ) ] )

    ] )

-- ----------------------------------------------------------------------------------------- --
-- SSB :  Standard Sort Bool

funcIdBoolToString :: FuncId
funcIdBoolToString     = FuncId toStringName   213 [SortBool]             SortString
funcIdBoolFromString :: FuncId
funcIdBoolFromString   = FuncId fromStringName 214 [SortString]           SortBool

funcIdBoolToXml :: FuncId
funcIdBoolToXml        = FuncId toXmlName      215 [SortBool]             SortString
funcIdBoolFromXml :: FuncId
funcIdBoolFromXml      = FuncId fromXmlName    216 [SortString]           SortBool

-- ----------------------------------------------------------------------------------------- --
-- SSI :  Standard Sort Int

funcIdIntToString :: FuncId
funcIdIntToString      = FuncId toStringName       301 [SortInt]            SortString
funcIdIntFromString :: FuncId
funcIdIntFromString    = FuncId fromStringName     302 [SortString]         SortInt

funcIdIntToXml :: FuncId
funcIdIntToXml         = FuncId toXmlName          303 [SortInt]            SortString
funcIdIntFromXml :: FuncId
funcIdIntFromXml       = FuncId fromXmlName        304 [SortString]         SortInt

-- ----------------------------------------------------------------------------------------- --
-- SSS :  Standard Sort String

funcIdStringToString :: FuncId
funcIdStringToString       = FuncId toStringName       525 [SortString]                 SortString
funcIdStringFromString :: FuncId
funcIdStringFromString     = FuncId fromStringName     526 [SortString]                 SortString
 
funcIdStringToXml :: FuncId
funcIdStringToXml          = FuncId toXmlName          527 [SortString]                 SortString
funcIdStringFromXml :: FuncId
funcIdStringFromXml        = FuncId fromXmlName        528 [SortString]                 SortString

funcIdtakeWhile :: FuncId
funcIdtakeWhile            = FuncId (fromNonEmpty $ fromList "takeWhile")    533 [SortString,SortString]   SortString
funcIdtakeWhileNot:: FuncId
funcIdtakeWhileNot         = FuncId (fromNonEmpty $ fromList "takeWhileNot") 534 [SortString,SortString]   SortString
funcIddropWhile :: FuncId
funcIddropWhile            = FuncId (fromNonEmpty $ fromList "dropWhile")    535 [SortString,SortString]   SortString
funcIddropWhileNot :: FuncId
funcIddropWhileNot         = FuncId (fromNonEmpty $ fromList "dropWhileNot") 536 [SortString,SortString]   SortString

-- * Standard channel identifiers
chanIdExit :: ChanId
chanIdExit  = ChanId (fromNonEmpty $ fromList "EXIT")  901 []
chanIdIstep :: ChanId
chanIdIstep = ChanId (fromNonEmpty $ fromList "ISTEP") 902 []
chanIdQstep :: ChanId
chanIdQstep = ChanId (fromNonEmpty $ fromList "QSTEP") 903 []
chanIdHit :: ChanId
chanIdHit   = ChanId (fromNonEmpty $ fromList "HIT")   904 []
chanIdMiss :: ChanId
chanIdMiss  = ChanId (fromNonEmpty $ fromList "MISS")  905 []
