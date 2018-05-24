{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.String.Utils   (endswith, replace, startswith, strip)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           System.IO
import           System.Process

import           ConstDefs
import           SMT2TXS
import           SMTAlex
import           SMTData
import           SMTHappy
import           SolveDefs
import           TXS2SMT
import           ValExpr
import           Variable

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


    return (SmtEnv hin
                   hout
                   herr
                   ph
                   lg
                   initialEnvNames
                   (EnvDefs Map.empty Map.empty Map.empty)
            )

-- ----------------------------------------------------------------------------------------- --
-- addDefinitions
-- ----------------------------------------------------------------------------------------- --
addDefinitions :: EnvDefs -> SMT ()
addDefinitions edefs =  do
    enames <- gets envNames
    -- exclude earlier defined sorts, e.g. for the pre-defined data types,
    -- such as Bool, Int, and String, because we use the equivalent SMTLIB built-in types
    let newSorts = Map.filterWithKey (\k _ -> Map.notMember k (sortNames enames)) (sortDefs edefs)
    let snames = foldr insertSort enames (Map.toList newSorts)
    
    -- constructors of sort introduce functions
    let newCstrs = Map.filterWithKey (\k _ -> Map.notMember k (cstrNames snames)) (cstrDefs edefs)
    let cnames = foldr insertCstr snames (Map.toList newCstrs)
    
    putT ( sortdefsToSMT cnames (EnvDefs newSorts newCstrs Map.empty) )
    put "\n\n"
    
    
    let newFuncs = Map.filterWithKey (\k _ -> Map.notMember k (funcNames cnames)) (funcDefs edefs)
    let fnames = foldr insertFunc cnames (Map.toList newFuncs)
    putT ( funcdefsToSMT fnames newFuncs )
    put "\n\n"
    
    original <- gets envDefs
    modify ( \e -> e { envNames = fnames
                     , envDefs = EnvDefs (Map.union (sortDefs original) (sortDefs edefs))
                                         (Map.union (cstrDefs original) (cstrDefs edefs))
                                         (Map.union (funcDefs original) (funcDefs edefs))
                     } 
           )
       -- use union to be certain all definitions remain included

-- --------------------------------------------------------------------------------------------
-- addDeclarations
-- --------------------------------------------------------------------------------------------
addDeclarations :: (Variable v) => [v] -> SMT ()
addDeclarations [] = return ()
addDeclarations vs  =  do
    mapI <- gets envNames
    putT ( declarationsToSMT mapI vs )
    return ()

-- ----------------------------------------------------------------------------------------- --
-- addAssertions
-- ----------------------------------------------------------------------------------------- --
addAssertions :: (Variable v) => [ValExpr v] -> SMT ()
addAssertions vexps  =  do
    mapI <- gets envNames
    putT ( assertionsToSMT mapI vexps )
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
    edefs <- gets envDefs
    return $ Map.fromList (map (toConst edefs vnameSMTValueMap) vs)
  where
    toConst :: (Variable v) => EnvDefs -> Map.Map Text SMTValue -> v -> (v, Const)
    toConst edefs mp v = case Map.lookup (vname v) mp of
                            Just smtValue   -> case smtValueToValExpr smtValue (cstrDefs edefs) (vsort v) of
                                                    Left t -> error $ "getSolution - SMT parse error:\n" ++ t
                                                    Right val -> (v,val)
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
                   Just res -> let str = strip res in
                               if startswith "\"" str && endswith "\"" str
                               then return $ List.init . tail $ str
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
  mapI <- gets envNames
  return $ valexprToSMT mapI v

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
      exitWithError :: procHandle -> IO ()
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
