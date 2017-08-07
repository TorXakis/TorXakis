{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module StdTDefs

-- ----------------------------------------------------------------------------------------- --
--  
-- Predefined, Standard TorXakis Data Types :  Bool, Int, Char, String
-- 
-- ----------------------------------------------------------------------------------------- --
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
, funcId_uniminusInt    
, funcId_plusInt        
, funcId_minusInt       
, funcId_timesInt       
, funcId_divideInt      
, funcId_moduloInt      
, funcId_ltInt          
, funcId_leInt          
, funcId_gtInt          
, funcId_geInt          
, funcId_absInt         

, funcId_StringToString     
, funcId_StringFromString   
, funcId_StringToXml        
, funcId_StringFromXml      
, funcId_catString          
, funcId_lenString          
, funcId_takeWhile          
, funcId_takeWhileNot       
, funcId_dropWhile          
, funcId_dropWhileNot       
, funcId_atString 

, funcId_strinre

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

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow ( (***) )

import ChanId
import CstrId
import FuncId
import FuncDef
import Ident
import SortDef
import SortId
import SortOf
import TxsDef
import ValExprDefs
import ValExprImpls
import VarId
import FuncTable

stdSortTable :: Map.Map String SortId
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

eqName, neqName :: String
eqName   =  "=="
neqName  =  "<>"

toStringName, fromStringName :: String
toStringName   = "toString"
fromStringName = "fromString"

toXmlName, fromXmlName :: String
toXmlName   = "toXml"
fromXmlName = "fromXml"

-- ----------------------------------------------------------------------------------------- --
-- Helper function
equalHandler :: Ord v => Handler v
equalHandler [a,b]  = cstrEqual a b
equalHandler _      = error "equalHandler expects two arguments"

notEqualHandler :: Ord v => Handler v
notEqualHandler [a, b] = cstrNot (cstrEqual a b)
notEqualHandler _      = error "notEqualHandler expects two arguments"

cstrHandler :: CstrId -> Handler v
cstrHandler = cstrCstr

iscstrHandler :: CstrId -> Handler v
iscstrHandler c [a] = cstrIsCstr c a
iscstrHandler _ _   = error "iscstrHandler expects one argument"

accessHandler :: CstrId -> Int -> Handler v
accessHandler c p [a] = cstrAccess c p a
accessHandler _ _ _   = error "accessHandler expects one argument"

impliesHandler :: Ord v => Handler v
impliesHandler [a,b] = cstrImplies a b
impliesHandler _     = error "impliesHandler expects two arguments"

xorHandler :: Ord v => Handler v
xorHandler [a, b] = cstrOr (Set.fromList [arg0, arg1])
  where arg0 = cstrAnd (Set.fromList [a, cstrNot b])
        arg1 = cstrAnd (Set.fromList [cstrNot a, b])  
xorHandler _      = error "xorHandler expects two arguments"

-- ----------------------------------------------------------------------------------------- --
-- FuncTable
stdFuncTable :: Ord v => FuncTable v
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
    , ("not",  Map.fromList [ ( Signature [sortId_Bool] sortId_Bool, cstrNot . head ) ] )
    , ("/\\",  Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, cstrAnd . Set.fromList ) ] )
    , ("\\/",  Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, cstrOr . Set.fromList ) ] )
    , ("\\|/", Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, xorHandler) ] )
    , ("=>",   Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, impliesHandler ) ] )
    , ("<=>",  Map.fromList [ ( Signature [sortId_Bool,sortId_Bool] sortId_Bool, equalHandler ) ] )

    , ("+",   Map.fromList [ ( Signature [sortId_Int] sortId_Int, head)
                           , ( Signature [sortId_Int,sortId_Int] sortId_Int, cstrPredef SSI funcId_plusInt ) 
                           ] )
    , ("-",   Map.fromList [ ( Signature [sortId_Int] sortId_Int, cstrPredef SSI funcId_uniminusInt )
                           , ( Signature [sortId_Int,sortId_Int] sortId_Int, cstrPredef SSI funcId_minusInt )
                           ] )
    , ("abs", Map.fromList [ ( Signature [sortId_Int] sortId_Int, cstrPredef SSI funcId_absInt ) ] )
    , ("*",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Int, cstrPredef SSI funcId_timesInt ) ] )
    , ("/",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Int, cstrPredef SSI funcId_divideInt ) ] )
    , ("%",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Int, cstrPredef SSI funcId_moduloInt ) ] )
    , ("<",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, cstrPredef SSI funcId_ltInt ) ] )
    , ("<=",  Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, cstrPredef SSI funcId_leInt ) ] )
    , (">",   Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, cstrPredef SSI funcId_gtInt ) ] )
    , (">=",  Map.fromList [ ( Signature [sortId_Int,sortId_Int] sortId_Bool, cstrPredef SSI funcId_geInt ) ] )

    , ("len",  Map.fromList [ ( Signature [sortId_String] sortId_Int, cstrPredef SSS funcId_lenString ) ] )
    , ("at",   Map.fromList [ ( Signature [sortId_String,sortId_Int] sortId_String, cstrPredef SSS funcId_atString ) ] )
    , ("++",   Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_catString ) ] )
    , ("takeWhile",    Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_takeWhile ) ] )
    , ("takeWhileNot", Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_takeWhileNot ) ] )
    , ("dropWhile",    Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_dropWhile ) ] )
    , ("dropWhileNot", Map.fromList [ ( Signature [sortId_String,sortId_String] sortId_String, cstrPredef SSS funcId_dropWhileNot ) ] )

    , ("strinre",   Map.fromList [ ( Signature [sortId_String,sortId_Regex] sortId_Bool, cstrPredef SSR funcId_strinre ) ] )

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

funcId_uniminusInt      = FuncId "-"                306 [sortId_Int]            sortId_Int
funcId_plusInt          = FuncId "+"                307 [sortId_Int,sortId_Int] sortId_Int
funcId_minusInt         = FuncId "-"                308 [sortId_Int,sortId_Int] sortId_Int
funcId_timesInt         = FuncId "*"                309 [sortId_Int,sortId_Int] sortId_Int
funcId_divideInt        = FuncId "/"                310 [sortId_Int,sortId_Int] sortId_Int
funcId_moduloInt        = FuncId "%"                311 [sortId_Int,sortId_Int] sortId_Int
-- power is non-linear function that can't be solved by problemsolver (yet)
-- funcId_powerInt      = FuncId "^"                312 [sortId_Int,sortId_Int] sortId_Int
funcId_ltInt            = FuncId "<"                315 [sortId_Int,sortId_Int] sortId_Bool
funcId_leInt            = FuncId "<="               316 [sortId_Int,sortId_Int] sortId_Bool
funcId_gtInt            = FuncId ">"                317 [sortId_Int,sortId_Int] sortId_Bool
funcId_geInt            = FuncId ">="               318 [sortId_Int,sortId_Int] sortId_Bool
funcId_absInt           = FuncId "abs"              319 [sortId_Int]            sortId_Int


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
     , ( funcId_uniminusInt,    let x = VarId "x" 346 sortId_Int
                                    in FuncDef [x] (cstrPredef SSI funcId_uniminusInt [cstrVar x]) )
     , ( funcId_plusInt,        let { x = VarId "x" 347 sortId_Int
                                    ; y = VarId "y" 348 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_plusInt [cstrVar x,cstrVar y]) )
     , ( funcId_minusInt,       let { x = VarId "x" 349 sortId_Int
                                    ; y = VarId "y" 350 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_minusInt [cstrVar x,cstrVar y]) )
     , ( funcId_timesInt,       let { x = VarId "x" 351 sortId_Int
                                    ; y = VarId "y" 352 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_timesInt [cstrVar x,cstrVar y]) )
     , ( funcId_divideInt,      let { x = VarId "x" 353 sortId_Int
                                    ; y = VarId "y" 354 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_divideInt [cstrVar x,cstrVar y]) )
     , ( funcId_moduloInt,      let { x = VarId "x" 355 sortId_Int
                                    ; y = VarId "y" 356 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_moduloInt [cstrVar x,cstrVar y]) )
--     , ( funcId_powerInt,     let { x = VarId "x" 357 sortId_Int
--                                  ; y = VarId "y" 358 sortId_Int
--                                  }
--                                  in FuncDef [x,y] (cstrPredef SSI funcId_powerInt [cstrVar x,cstrVar y]) )
     , ( funcId_ltInt,          let { x = VarId "x" 363 sortId_Int
                                    ; y = VarId "y" 364 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_ltInt [cstrVar x,cstrVar y]) )
     , ( funcId_leInt,          let { x = VarId "x" 365 sortId_Int
                                    ; y = VarId "y" 366 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_leInt [cstrVar x,cstrVar y]) )
     , ( funcId_gtInt,          let { x = VarId "x" 367 sortId_Int
                                    ; y = VarId "y" 368 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_gtInt [cstrVar x,cstrVar y]) )
     , ( funcId_geInt,          let { x = VarId "x" 369 sortId_Int
                                    ; y = VarId "y" 370 sortId_Int
                                    }
                                    in FuncDef [x,y] (cstrPredef SSI funcId_geInt [cstrVar x,cstrVar y]) )
     , ( funcId_absInt,         let x = VarId "x" 371 sortId_Int
                                    in FuncDef [x] (cstrPredef SSI funcId_absInt [cstrVar x]) )
     ]

stdFuncDefsInt :: [ ( Ident, TxsDef ) ]
stdFuncDefsInt = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsInt'
-- ----------------------------------------------------------------------------------------- --
-- SSS :  Standard Sort String

funcId_StringToString       = FuncId toStringName       525 [sortId_String]                 sortId_String
funcId_StringFromString     = FuncId fromStringName     526 [sortId_String]                 sortId_String

funcId_StringToXml          = FuncId toXmlName          527 [sortId_String]                 sortId_String
funcId_StringFromXml        = FuncId fromXmlName        528 [sortId_String]                 sortId_String

funcId_catString            = FuncId "++"               531 [sortId_String,sortId_String]   sortId_String
funcId_lenString            = FuncId "len"              532 [sortId_String]                 sortId_Int

funcId_takeWhile            = FuncId "takeWhile"        533 [sortId_String,sortId_String]   sortId_String
funcId_takeWhileNot         = FuncId "takeWhileNot"     534 [sortId_String,sortId_String]   sortId_String
funcId_dropWhile            = FuncId "dropWhile"        535 [sortId_String,sortId_String]   sortId_String
funcId_dropWhileNot         = FuncId "dropWhileNot"     536 [sortId_String,sortId_String]   sortId_String

funcId_atString             = FuncId "at"               537 [sortId_String, sortId_Int]     sortId_String

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
     , ( funcId_catString, let { x = VarId "x" 551 sortId_String
                               ; y = VarId "y" 552 sortId_String
                               }
                            in FuncDef [x,y] (cstrPredef SSS funcId_catString [cstrVar x,cstrVar y]) )
     , ( funcId_lenString, let { x = VarId "x" 553 sortId_String
                               }
                            in FuncDef [x] (cstrPredef SSS funcId_lenString [cstrVar x]) )
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
     , ( funcId_atString
       , let { s = VarId "s" 562 sortId_String
             ; n = VarId "n" 563 sortId_Int
             }
          in FuncDef [s,n] (cstrPredef SSS funcId_atString [cstrVar s, cstrVar n])
       )
     ]

stdFuncDefsString :: [ ( Ident, TxsDef ) ]
stdFuncDefsString = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsString'
-- ----------------------------------------------------------------------------------------- --
-- SSR :  Standard Sort Regex

funcId_strinre    = FuncId "strinre"     611 [sortId_String, sortId_Regex]    sortId_Bool

stdFuncDefsRegex' :: [ ( FuncId, FuncDef ) ]
stdFuncDefsRegex'
  =  [ ( funcId_strinre,  let { x = VarId "s" 643 sortId_String
                              ; y = VarId "r" 644 sortId_Regex
                              }
                            in FuncDef [x,y] (cstrPredef SSR funcId_strinre [cstrVar x,cstrVar y]) )
     ]

stdFuncDefsRegex :: [ ( Ident, TxsDef ) ]
stdFuncDefsRegex = map (IdFunc Control.Arrow.*** DefFunc) stdFuncDefsRegex'
-- ----------------------------------------------------------------------------------------- --
-- combined TorXakis standard type definitions


stdTDefs :: [ ( Ident, TxsDef ) ]
stdTDefs =    stdSortDefs
           ++ stdFuncDefsBool
           ++ stdFuncDefsInt
           ++ stdFuncDefsString
           ++ stdFuncDefsRegex

-- ----------------------------------------------------------------------------------------- --
-- standard channel identifiers ------------------------------------------------------------ --


chanId_Exit  = ChanId "EXIT"  901 []
chanId_Istep = ChanId "ISTEP" 902 []  
chanId_Qstep = ChanId "QSTEP" 903 []  
chanId_Hit   = ChanId "HIT"   904 []  
chanId_Miss  = ChanId "MISS"  905 []  


-- ----------------------------------------------------------------------------------------- --
-- 
-- ----------------------------------------------------------------------------------------- --