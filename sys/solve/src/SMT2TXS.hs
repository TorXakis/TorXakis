{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module SMT2TXS

-- ----------------------------------------------------------------------------------------- --
--
-- Translate SMT results to TorXakis ValExpr's
--
-- ----------------------------------------------------------------------------------------- --
-- export

( smtValueToValExpr   --  :: SMTValue -> TxsDefs -> SortId -> Walue
)

-- ----------------------------------------------------------------------------------------- --
-- import

where
import qualified Data.Map as Map

import StdTDefs
import TxsDefs
import TxsShow
import SortId
import CstrId

import SMTHappy

import Data.String.Utils

-- ---------------------------------------------------------
-- Note: 
-- performance might improve, when parsing of smt value 
-- and mapping onto Torxakis data structure is combined 
-- into a single attribute grammar 
-- ----------------------------------------------------------

-- ----------------------------------------------------------------------------------------- --
-- lookup a constructor given its sort and constructor name in the given TorXakis definitions

lookupConstructor :: TxsDefs -> SortId -> String -> CstrId
lookupConstructor tdefs sid n
  =  case [ cstr
          | cstr@CstrId{ CstrId.name = n', cstrsort = sid' } <- Map.keys (cstrDefs tdefs)
          , n == n'
          , sid == sid'
          ] of
     { [c] -> c
     ; _   -> error $ "TXS SMT2TXS lookupConstructor: No (unique) constructor for sort " ++
                      show sid ++ " and name " ++ n ++ "\n"
     }


-- ----------------------------------------------------------------------------------------- --
-- convert an SMT expression to a ValExpr given a varName
-- the varName is the name of a SMT identifier that refers to a SMT variable

    
smtValueToValExpr :: SMTValue -> TxsDefs -> SortId -> Const
smtValueToValExpr (SMTBool b) _ sort
  =  if sortId_Bool == sort
       then Cbool b
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "Bool expected, got " ++ pshow sort ++ "\n"

smtValueToValExpr (SMTInt i) _ sort
  =  if sortId_Int == sort
       then Cint i
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "Int expected, got " ++ pshow sort ++ "\n"
                
smtValueToValExpr (SMTString s) _ sort
  =  if sortId_String == sort
       then Cstring s
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "String expected, got " ++ pshow sort ++ "\n"
                
smtValueToValExpr (SMTConstructor cname argValues) tdefs sort =
    let nameSort = SortId.name sort in 
        if startswith (nameSort++"$") cname
            then  -- look up internal CstrId
                let cstrid = lookupConstructor tdefs sort (drop (1+length nameSort) cname) in
                    if length (cstrargs cstrid) == length argValues
                        then  -- recursively translate the arguments:
                            let vexprArgs = map (\(argValue, sort') -> smtValueToValExpr argValue tdefs sort')
                                                (zip argValues (cstrargs cstrid)) in 
                                Cstr cstrid vexprArgs
                        else Cerror $ "TXS SMT2TXS smtValueToValExpr: Number of arguments mismatch " ++
                                      "in constructor " ++ cname ++ " of sort " ++ nameSort ++
                                      " : definition " ++ show (length (cstrargs cstrid)) ++
                                      " vs actual " ++ show (length argValues) ++ "\n"
            else Cerror $ "TXS SMT2TXS smtValueToValExpr: CstrName " ++ cname ++
                          " does not start with sort " ++ nameSort ++ "\n"
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --