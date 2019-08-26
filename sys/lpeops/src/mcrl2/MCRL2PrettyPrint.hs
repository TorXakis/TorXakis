{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MCRL2PrettyPrint
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module MCRL2PrettyPrint (
showSort,
showDExpr,
showPExpr,
showSpecification
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified MCRL2Defs

showObjectId :: MCRL2Defs.ObjectId -> String
showObjectId = Text.unpack

showSort :: MCRL2Defs.Sort -> String
showSort MCRL2Defs.BoolSort = "Bool"
showSort MCRL2Defs.IntSort = "Int"
showSort (MCRL2Defs.ListSort elemSort) = "List(" ++ showSort elemSort ++ ")"
showSort (MCRL2Defs.SetSort elemSort) = "Set(" ++ showSort elemSort ++ ")"
showSort (MCRL2Defs.BagSort elemSort) = "Bag(" ++ showSort elemSort ++ ")"
showSort (MCRL2Defs.StructSort constructors) = "struct " ++ List.intercalate " | " (map showConstructor constructors)
showSort (MCRL2Defs.SortRef sortId) = showObjectId sortId
showSort (MCRL2Defs.MultiSort sorts) = "(" ++ List.intercalate " # " (map showSort sorts) ++ ")"
showSort (MCRL2Defs.FunctionSort inputSort outputSort) = "(" ++ showSort inputSort ++ " -> " ++ showSort outputSort ++ ")"
showSort other = error ("'showSort' not defined for " ++ show other)

showConstructor :: MCRL2Defs.Constructor -> String
showConstructor (MCRL2Defs.Constructor cstrName [] recognizer) =
    showObjectId cstrName ++ " ? " ++ showObjectId recognizer
showConstructor (MCRL2Defs.Constructor cstrName fields recognizer) =
    showObjectId cstrName ++ "(" ++ List.intercalate ", " (map showVariable fields) ++ ") ? " ++ showObjectId recognizer
showConstructor other = error ("'showConstructor' not defined for " ++ show other)

showDExpr :: MCRL2Defs.DExpr -> String
showDExpr (MCRL2Defs.DBool value) = if value then "true" else "false"
showDExpr (MCRL2Defs.DInt value) = show value
showDExpr (MCRL2Defs.DList elems) = "[" ++ List.intercalate ", " (map showDExpr elems) ++ "]"
showDExpr (MCRL2Defs.DFiniteSet elems) = "{" ++ List.intercalate ", " (map showDExpr elems) ++ "}"
showDExpr (MCRL2Defs.DVariableRef (MCRL2Defs.Variable varId _varSort)) = showObjectId varId
showDExpr (MCRL2Defs.DConstructorRef cstrId []) = showObjectId cstrId
showDExpr (MCRL2Defs.DConstructorRef cstrId varEqs) = showObjectId cstrId ++ "(" ++ List.intercalate ", " (map (showDExpr . snd) varEqs) ++ ")"
showDExpr (MCRL2Defs.DRecognizer recognizerId expr) = showObjectId recognizerId ++ "(" ++ showDExpr expr ++ ")"
showDExpr (MCRL2Defs.DFieldAccess fieldId expr) = showObjectId fieldId ++ "(" ++ showDExpr expr ++ ")"
showDExpr (MCRL2Defs.DMappingRef mappingId []) = showObjectId mappingId
showDExpr (MCRL2Defs.DMappingRef mappingId exprs) = showObjectId mappingId ++ "(" ++ List.intercalate ", " (map showDExpr exprs) ++ ")"
showDExpr (MCRL2Defs.DEqual lhs rhs) = "(" ++ showDExpr lhs ++ " == " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DNotEqual lhs rhs) = "(" ++ showDExpr lhs ++ " <> " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DGreater lhs rhs) = "(" ++ showDExpr lhs ++ " > " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DSmaller lhs rhs) = "(" ++ showDExpr lhs ++ " < " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DGreaterEquals lhs rhs) = "(" ++ showDExpr lhs ++ " >= " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DSmallerEquals lhs rhs) = "(" ++ showDExpr lhs ++ " <= " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DAnd lhs rhs) = "(" ++ showDExpr lhs ++ " && " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DOr lhs rhs) = "(" ++ showDExpr lhs ++ " || " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DIfThenElse condition ifBranch elseBranch) = "if(" ++ showDExpr condition ++ ", " ++ showDExpr ifBranch ++ ", " ++ showDExpr elseBranch ++ ")"
showDExpr (MCRL2Defs.DNot expr) = "!(" ++ showDExpr expr ++ ")"
showDExpr (MCRL2Defs.DAdd lhs rhs) = "(" ++ showDExpr lhs ++ " + " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DSubtract lhs rhs) = "(" ++ showDExpr lhs ++ " - " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DMultiply lhs rhs) = "(" ++ showDExpr lhs ++ " * " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DDivide lhs rhs) = "(" ++ showDExpr lhs ++ " / " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DModulo lhs rhs) = "(" ++ showDExpr lhs ++ " mod " ++ showDExpr rhs ++ ")"
showDExpr (MCRL2Defs.DListSize expr) = "#(" ++ showDExpr expr ++ ")"
showDExpr (MCRL2Defs.DListElement expr index) = "(" ++ showDExpr expr ++ " . " ++ showDExpr index ++ ")"
showDExpr (MCRL2Defs.DListConcatenate lhs rhs) = "(" ++ showDExpr lhs ++ " ++ " ++ showDExpr rhs ++ ")"
showDExpr other = error ("'showDExpr' not defined for " ++ show other)

showVariable :: MCRL2Defs.Variable -> String
showVariable (MCRL2Defs.Variable varId varSort) = showObjectId varId ++ ": " ++ showSort varSort
showVariable other = error ("'showVariable' not defined for " ++ show other)

showAInstance :: MCRL2Defs.AInstance -> String
showAInstance (MCRL2Defs.AInstance actionId []) = showObjectId actionId
showAInstance (MCRL2Defs.AInstance actionId params) = showObjectId actionId ++ "(" ++ List.intercalate ", " (map showDExpr params) ++ ")"

showAExpr :: MCRL2Defs.AExpr -> String
showAExpr MCRL2Defs.ATau = "tau"
showAExpr (MCRL2Defs.AExpr actionInstances) = List.intercalate "|" (map showAInstance actionInstances)

showMultiAction :: MCRL2Defs.MultiAction -> String
showMultiAction actions = List.intercalate "|" (map showObjectId actions)

showComm :: (MCRL2Defs.MultiAction, MCRL2Defs.ObjectId) -> String
showComm (multiAction, targetActionId) = showMultiAction multiAction ++ " -> " ++ showObjectId targetActionId

showRename :: (MCRL2Defs.ObjectId, MCRL2Defs.ObjectId) -> String
showRename (sourceActionId, targetActionId) = showObjectId sourceActionId ++ " -> " ++ showObjectId targetActionId

showPExpr :: String -> MCRL2Defs.PExpr -> String
showPExpr _prefix (MCRL2Defs.PAction expr) = showAExpr expr
showPExpr prefix (MCRL2Defs.PSeq exprs) =
    let newPrefix = prefix ++ "\t" in
      "(\n" ++ newPrefix ++ List.intercalate ("\n" ++ newPrefix ++ ". ") (map (showPExpr newPrefix) exprs) ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PPar exprs) =
    let newPrefix = prefix ++ "\t" in
      "(\n" ++ newPrefix ++ List.intercalate ("\n" ++ newPrefix ++ "|| ") (map (showPExpr newPrefix) exprs) ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PChoice exprs) =
    let newPrefix = prefix ++ "\t" in
      "(\n" ++ newPrefix ++ List.intercalate ("\n" ++ newPrefix ++ "+ ") (map (showPExpr newPrefix) exprs) ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PSum [] expr) = showPExpr prefix expr
showPExpr prefix (MCRL2Defs.PSum vars expr) =
    let newPrefix = prefix ++ "\t" in
      "(sum " ++ List.intercalate ", " (map showVariable vars) ++ " .\n" ++ newPrefix ++ showPExpr newPrefix expr ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PGuard condition ifBranch elseBranch) =
    let newPrefix = prefix ++ "\t" in
      "((" ++ showDExpr condition ++ ")\n" ++ newPrefix ++ "-> " ++ showPExpr newPrefix ifBranch ++ "\n" ++ newPrefix ++ "<> " ++ showPExpr newPrefix elseBranch ++ "\n" ++ prefix ++ ")"
showPExpr _prefix MCRL2Defs.PDeadlock = "delta"
showPExpr prefix (MCRL2Defs.PHide actions expr) =
    let newPrefix = prefix ++ "\t" in
      "hide({" ++ List.intercalate ", " (map showObjectId actions) ++ "},\n" ++ newPrefix ++ showPExpr newPrefix expr ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PAllow multiActions expr) =
    let newPrefix = prefix ++ "\t" in
      "allow({" ++ List.intercalate ", " (map showMultiAction multiActions) ++ "},\n" ++ newPrefix ++ showPExpr newPrefix expr ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PComm comms expr) =
    let newPrefix = prefix ++ "\t" in
      "comm({" ++ List.intercalate ", " (map showComm comms) ++ "},\n" ++ newPrefix ++ showPExpr newPrefix expr ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PRename renames expr) =
    let newPrefix = prefix ++ "\t" in
      "rename({" ++ List.intercalate ", " (map showRename renames) ++ "},\n" ++ newPrefix ++ showPExpr newPrefix expr ++ "\n" ++ prefix ++ ")"
showPExpr prefix (MCRL2Defs.PBlock actions expr) =
    let newPrefix = prefix ++ "\t" in
      "block({" ++ List.intercalate ", " (map showObjectId actions) ++ "},\n" ++ newPrefix ++ showPExpr newPrefix expr ++ ")"
showPExpr _prefix (MCRL2Defs.PInst procId varEqs) = showObjectId procId ++ "(" ++ List.intercalate ", " (map (showDExpr . snd) varEqs) ++ ")"

showSortDecl :: (MCRL2Defs.ObjectId, MCRL2Defs.Sort) -> String
showSortDecl (sortId, MCRL2Defs.ImplicitSort) = showObjectId sortId
showSortDecl (sortId, sort) = showObjectId sortId ++ " = " ++ showSort sort

showMappingDecl :: (MCRL2Defs.ObjectId, MCRL2Defs.Sort) -> String
showMappingDecl (mappingId, mappingSort) = showObjectId mappingId ++ ": " ++ showSort mappingSort

showEquation :: MCRL2Defs.Equation -> String
showEquation (MCRL2Defs.Equation lhs rhs) = showDExpr lhs ++ " = " ++ showDExpr rhs

showEquationGroup :: MCRL2Defs.EquationGroup -> String
showEquationGroup (MCRL2Defs.EquationGroup _ []) = ""
showEquationGroup (MCRL2Defs.EquationGroup variables equations) =
    showObjectsWithHeader "var" showVariable variables ++ showObjectsWithHeader "eqn" showEquation equations
-- showEquationGroup

showActionDecl :: (MCRL2Defs.ObjectId, MCRL2Defs.Action) -> String
showActionDecl (actionId, MCRL2Defs.Action MCRL2Defs.ImplicitSort) = showObjectId actionId
showActionDecl (actionId, MCRL2Defs.Action sort) = showObjectId actionId ++ ": " ++ showSort sort
showActionDecl other = error ("'showActionDecl' not defined for " ++ show other)

showProcessDecl :: (MCRL2Defs.ObjectId, MCRL2Defs.Process) -> String
showProcessDecl (procId, MCRL2Defs.Process [] body) = showObjectId procId ++ " =\n\t" ++ showPExpr "\t" body
showProcessDecl (procId, MCRL2Defs.Process params body) = showObjectId procId ++ "(" ++ List.intercalate ", " (map showVariable params) ++ ")\n\t= " ++ showPExpr "\t" body
showProcessDecl other = error ("'showProcessDecl' not defined for " ++ show other)

showSpecification :: MCRL2Defs.Specification -> String
showSpecification specification =
       showObjectsWithHeader "sort" showSortDecl (Map.toList (MCRL2Defs.sorts specification))
    ++ showObjectsWithHeader "map" showMappingDecl (Map.toList (MCRL2Defs.mappings specification))
    ++ concatMap showEquationGroup (MCRL2Defs.equationGroups specification)
    ++ showObjectsWithHeader "act" showActionDecl (Map.toList (MCRL2Defs.actions specification))
    ++ showObjectsWithHeader "glob" showVariable (Map.elems (MCRL2Defs.globals specification))
    ++ showObjectsWithHeader "proc" showProcessDecl (Map.toList (MCRL2Defs.processes specification))
    ++ "init\n\t" ++ showPExpr "\t" (MCRL2Defs.init specification) ++ ";\n"
-- showSpecification

showObjectsWithHeader :: String -> (t -> String) -> [t] -> String
showObjectsWithHeader _header _f [] = ""
showObjectsWithHeader header f objects = header ++ "\n" ++ concatMap (\object -> "\t" ++ f object ++ ";\n") objects



