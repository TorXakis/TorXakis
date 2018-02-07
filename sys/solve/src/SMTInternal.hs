{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module SMTInternal

-- ------------------------------------------------------------
-- SMT Internal should not be included directly in production code.
-- SMT Internal contains all non-interface SMT functions.
-- Some of these functions are used for test purposes
-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Exception   (onException)
import           Control.Monad.State (gets, lift, modify)

import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Monoid
import qualified Data.Set            as Set
import           Data.String.Utils   (endswith, replace, startswith, strip)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           System.IO
import           System.Process

import           ConstDefs
import           FuncDef
import           FuncId
import           Identifier
import           SMT2TXS
import           SMTAlex
import           SMTData
import           SMTHappy
import           SolveDefs
import           Sort
import           TXS2SMT
import           ValExpr
import           Variable
import           VarId

-- ----------------------------------------------------------------------------------------- --
-- opens a connection to the SMTLIB interactive shell
-- defines the logic and sets the appropriate options
-- and returns a handle for SMT
-- ----------------------------------------------------------------------------------------- --
openSolver :: SMT String
openSolver = do
    n <- getInfo "name"
    v <- getInfo "version"
    SMTInternal.init
    push
    return $ n ++ " [" ++ v ++ "]"

-- ----------------------------------------------------------------------------------------- --
-- close the connection to the SMTLIB interactive shell
-- ----------------------------------------------------------------------------------------- --
close :: SMT ()
close  =  do
    hin <- gets inHandle
    hout <- gets outHandle
    herr <- gets errHandle
    lfmh <- gets logFileHandle
    lift $ hClose hin
    lift $ hClose hout
    lift $ hClose herr
    case lfmh of
        Nothing  -> return ()
        Just lfh -> lift $ hClose lfh

-- ----------------------------------------------------------------------------------------- --
-- push
-- ----------------------------------------------------------------------------------------- --
push :: SMT ()
push = put "(push 1)"

-- ----------------------------------------------------------------------------------------- --
-- pop
-- ----------------------------------------------------------------------------------------- --
pop :: SMT ()
pop = put "(pop 1)"

-- ----------------------------------------------------------------------------------------- --
-- SMT communication functions via process fork
-- ----------------------------------------------------------------------------------------- --

createSMTEnv :: CreateProcess -> Bool -> IO SmtEnv
createSMTEnv cmd lgFlag =  do
    (Just hin, Just hout, Just herr, ph) <- createProcess cmd
                                                           { std_out = CreatePipe
                                                           , std_in = CreatePipe
                                                           , std_err = CreatePipe
                                                           }
    hSetBuffering hin  NoBuffering         -- alternative: LineBuffering
    hSetBuffering hout NoBuffering
    hSetBuffering herr NoBuffering

    hSetEncoding hin  latin1
    hSetEncoding hout latin1
    hSetEncoding herr latin1

    -- open SMT logging file
    lg <- if lgFlag
            then do timeZone <- getCurrentTimeZone
                    startTime <- getCurrentTime
                    let timeString = replace ":" "-" $
                                     replace " " "-" $
                                        show (utcToLocalTime timeZone startTime) in do
                        h <- openFile ("logSMT." ++ timeString ++ ".smt2") WriteMode
                        hSetBuffering h NoBuffering
                        hSetEncoding h latin1
                        return $ Just h
            else return Nothing

    --ein <- hGetEncoding hin
    --eout <- hGetEncoding hout
    --eerr <- hGetEncoding herr
    --Trace.trace ("hin encoding = " ++ show ein ++ "\n" ++
    --             "hout encoding = " ++ show eout ++ "\n" ++
    --             "herr encoding = " ++ show eerr ++ "\n" )


    -- hSetNewlineMode hin  ( NewlineMode { inputNL = LF,   outputNL = CRLF } )
    -- hSetNewlineMode hout ( NewlineMode { inputNL = CRLF, outputNL = LF   } )


    return $ SmtEnv hin
                    hout
                    herr
                    ph
                    lg
                    emptyADTDefs
                    Set.empty
                    Map.empty

-- ----------------------------------------------------------------------------------------- --
-- addDefinitions
-- ----------------------------------------------------------------------------------------- --
addADTDefinitions :: ADTDefs -> SMT ()
addADTDefinitions newADTDefs = do
    adtDefsInSmt <- gets adtDefs
    let newUniqueADTDefs = Map.withoutKeys (adtDefsToMap newADTDefs)
                                            $ Map.keysSet $ adtDefsToMap adtDefsInSmt
        (txt, newDec) = adtDefsToSMT newUniqueADTDefs
    putT txt
    put "\n\n"
    
    let allADTDefs = addADTDefs (Map.toList newUniqueADTDefs) adtDefsInSmt
    case allADTDefs of
        Right aDefs -> do   dec <- gets decoderMap
                            modify (\e -> e { adtDefs = aDefs
                                            , decoderMap = Map.union dec newDec
                                            } )
        Left  err   -> error $ "SMTInternal - addADTDefinitions: Expectation violated - ADTDEFS + newUnique is a valid ADTDEFS. Error:"
                             ++ T.unpack (toErrorText err)

addFuncDefinitions :: Map.Map FuncId (FuncDef VarId) -> SMT ()
addFuncDefinitions funcDefs = do
    fIds <- gets funcIds
    let newFuncs = Map.filterWithKey (\k _ -> Set.notMember k fIds) funcDefs
    putT $ funcdefsToSMT newFuncs
    put "\n\n"
    modify (\e -> e { funcIds = Set.union fIds $ Map.keysSet funcDefs } )

-- --------------------------------------------------------------------------------------------
-- addDeclarations
-- --------------------------------------------------------------------------------------------
addDeclarations :: (Variable v) => [v] -> SMT ()
addDeclarations [] = return ()
addDeclarations vs  =  do
    putT ( declarationsToSMT vs )
    return ()

-- ----------------------------------------------------------------------------------------- --
-- addAssertions
-- ----------------------------------------------------------------------------------------- --
addAssertions :: (Variable v) => [ValExpr v] -> SMT ()
addAssertions vexps  =  do
    putT ( assertionsToSMT vexps )
    return ()

-- ----------------------------------------------------------------------------------------- --
-- getSolvable
-- ----------------------------------------------------------------------------------------- --
getSolvable :: SMT SolvableProblem
getSolvable = do
    put "(check-sat)"
    s <- getSMTresponse
    return $ case s of
               "sat"        -> Sat
               "unsat"      -> Unsat
               "unknown"    -> Unknown
               _            -> error ("SMT checkSat: Unexpected result '"++ s ++ "'")


-- ----------------------------------------------------------------------------------------- --
-- getSolution
-- ----------------------------------------------------------------------------------------- --
getSolution :: (Variable v) => [v] -> SMT (Solution v)
getSolution []    = return Map.empty
getSolution vs    = do
    putT ("(get-value (" <> T.intercalate " " (map vname vs) <>"))")
    s <- getSMTresponse
    let vnameSMTValueMap = Map.mapKeys T.pack . smtParser . smtLexer $ s
    aDefs <- gets adtDefs
    decoder <- gets decoderMap
    return $ Map.fromList (map (toConst vnameSMTValueMap aDefs decoder) vs)
  where
    toConst :: (Variable v) => Map.Map Text SMTValue
                            -> ADTDefs
                            -> Map.Map Text (Ref ADTDef, Ref ConstructorDef)
                            -> v
                            -> (v, Const)
    toConst mp ad dc v = case Map.lookup (vname v) mp of
                            Just smtValue   -> (v, smtValueToConst smtValue (vsort v) ad dc)
                            Nothing         -> error "getSolution - SMT hasn't returned the value of requested variable."

-- ------------------------------------------
-- get SMT info
-- ------------------------------------------
getInfo :: String -> SMT String
getInfo info = do
    put ("(get-info :" ++ info ++ ")")
    s <- getSMTresponse
    let list = strip s in
        if startswith "(" list && endswith ")" list
        then
            let tuple = strip (List.init . tail $ list) in
                case List.stripPrefix (":" ++ info) tuple of
                   Just res -> let string = strip res in
                               if startswith "\"" string && endswith "\"" string
                               then return $ List.init . tail $ string
                               else error ("SMT response violates quotes in pattern.\nExpected (:" ++ info ++ " \"<name>\")\nActual "++ list)
                   Nothing -> error ("SMT response violates info in pattern.\nExpected (:" ++ info ++ " \"<name>\")\nActual "++ list)
        else
            error ("SMT response violates brackets in pattern.\nExpected (:" ++ info ++ " \"<name>\")\nActual "++ list)


-- ----------------------------------------------------------------------------------------- --
-- init
-- ----------------------------------------------------------------------------------------- --

init :: SMT ()
init  =  do
    put "(set-logic ALL)"
    put "(set-option :produce-models true)"
    put "(set-info :smt-lib-version 2.5)"
    putT basicDefinitionsSMT
    return ()

-- | execute the SMT command given as a string
put :: String -> SMT ()
put cmd  = do
  lg <- gets logFileHandle
  lift $ hPutSmtLog lg cmd
  mapM_ putLine (lines cmd)
  where
        putLine :: String -> SMT ()
        putLine cmd'  = do
            hin <- gets inHandle
            -- execute command
            lift $ hPutStrLn hin cmd'
            herr <- gets errHandle
            -- show errors - if there are any - and wait for the command to finish executing
            lift $ checkErrors herr "SMT WARN >> "
            return ()

putT :: Text -> SMT ()
putT = put . T.unpack

-- | Transform value expression to an SMT string.
valExprToString :: Variable v => ValExpr v -> SMT Text
valExprToString v = do
  return $ valexprToSMT v

-- ----------------------------------------------------------------------------------------- --
--  return error messages if any are present
--  where the given prefix is prepended to every line

checkErrors :: Handle -> String -> IO ()
checkErrors herr prefix  = do
    errors <- getAllInput herr
    case errors of
        []  -> return ()
        _   -> let pes = unlines $ map (prefix ++) (lines errors) in
                putStrLn pes

-- ---------------------------------------------------------------------------------------- --
-- read all available data from given handle
-- this operation doesn't block when no data can be read
getAllInput :: Handle -> IO String
getAllInput h = do
    ready <- hReady h
    if ready
        then do x <- hGetChar h
                xs <- getAllInput h
                return $ x:xs
        else return []

-- ----------------------------------------------------------------------------------------- --
-- read the response (as lines) from the handle
-- this operation is blocking until some data can be read
getResponse :: Handle -> Integer -> IO String
getResponse h count = do
    s <- hGetLine h
    let newCount = count + countBracket s in
        if 0 == newCount
            then return s
            else do
                    tl <- getResponse h newCount
                    return $ s ++ tl

getSMTresponse :: SMT String
getSMTresponse = do
    hout <- gets outHandle
    ph <- gets smtProcessHandle
    lift $ getResponse hout 0 `onException` exitWithError ph
    where
      exitWithError procHandle = do
        ec <- getProcessExitCode procHandle
        -- The output and error handles of the SMT process are closed when this
        -- error occurs (maybe because they're pipes?) so no information can be
        -- given to the user, other than this.
        putStrLn $ "getSMTresponse: SMT exited with status: " ++ show ec
        error "getSMTresponse: Could not get a response from the solver"

countBracket :: String -> Integer
countBracket ('"':xs) = skipCountInsideString xs          -- ignore brackets inside strings
countBracket ('(':xs) = 1 + countBracket xs
countBracket (')':xs) = -1 + countBracket xs
countBracket (_:xs)   = countBracket xs
countBracket []       = 0

skipCountInsideString :: String -> Integer
skipCountInsideString ('"':'"':xxs) = skipCountInsideString xxs       -- escape quote, stay in string
skipCountInsideString ('"':xs)      = countBracket xs            -- outside string
skipCountInsideString (_:xs)        = skipCountInsideString xs
skipCountInsideString []            = 0

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

hPutSmtLog :: Maybe Handle -> String -> IO ()
hPutSmtLog (Just lg) s = hPutStrLn lg s
hPutSmtLog Nothing   _ = return ()
