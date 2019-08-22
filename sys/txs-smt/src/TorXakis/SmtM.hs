{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Assertions
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the SMT ProblemSolver instance.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TorXakis.SmtM
( SmtM (..)
, SmtState
, initSmtState
, openSmtSolver
, closeSmtSolver
)
where
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.State
import           Control.Monad.Except
import           Data.List
import           Data.String.Utils
import           Data.Time
import           System.Exit
import           System.IO
import           System.Process

import           TorXakis.Error
import           TorXakis.ProblemSolver

data ActiveSmtState = ActiveSmtState { inHandle       :: Handle
                                     , outHandle      :: Handle
                                     , processHandle  :: ProcessHandle
                                     , logFileMHandle :: Maybe Handle
                                     , depth          :: Integer
                                     }
-- | Smt State
newtype SmtState = SmtState { mActive :: Maybe ActiveSmtState
                            }

-- | initSmtState constructor
initSmtState :: SmtState
initSmtState = SmtState Nothing

-- | Smt Monad
newtype SmtM a = SmtM { -- | Run the Smt solver
                        runSmt :: StateT SmtState (ExceptT Error IO) a
                      }
                      deriving (Functor, Applicative, Monad, MonadState SmtState, MonadIO, MonadError Error)

instance ProblemSolver SmtM where
    info = do
                n <- getInfo "name"
                v <- getInfo "version"
                return $ "SMT solver using " ++ n ++ " [" ++ v ++ "]"

    addSorts = undefined
    addFunctions = undefined

    push = do
            smtPut "(push 1)"
            -- update depth (not returned by smt solver)
            st <- get
            case mActive st of
                Nothing     -> throwError $ Error "No Smt Solver is open for push cmd"
                Just active -> let newDepth = succ (TorXakis.SmtM.depth active)
                                in do
                                    put $ SmtState (Just active{ TorXakis.SmtM.depth = newDepth })
                                    return newDepth

    pop = do
            st <- get
            case mActive st of
                Nothing     -> throwError $ Error "No Smt Solver is open for pop cmd"
                Just active -> let d = TorXakis.SmtM.depth active
                                in
                                    if d > 0
                                    then let newDepth = pred d
                                            in do
                                                smtPut "(pop 1)"
                                                put $ SmtState (Just active{ TorXakis.SmtM.depth = newDepth })
                                                return newDepth
                                    else throwError $ Error "Attempt to pop while the stack is empty."

    depth = do
                st <- get
                case mActive st of
                    Nothing     -> throwError $ Error "No Smt Solver is open for depth"
                    Just active -> return (TorXakis.SmtM.depth active)

    declareVariables = undefined
    addAssertions = undefined

    solvable = do
                smtPut "(check-sat)"
                s <- smtGet
                case s of
                    "sat"        -> return $ SolvableProblem (Just True)
                    "unsat"      -> return $ SolvableProblem (Just False)
                    "unknown"    -> return $ SolvableProblem Nothing
                    _            -> throwError $ Error ("solvable - Unexpected result by smt check-sat '"++ s ++ "'")

    solve = undefined
    kindOfProblem = undefined
    toValExprContext = undefined

-- | open Smt Solver
openSmtSolver :: FilePath -> [String] -> Bool -> SmtM ()
openSmtSolver fp args lgFlag = do
    st <- get
    case mActive st of
        Just _ -> throwError $ Error "A Smt Solver is already open"
        Nothing -> do
            -- start smt solver process
            (Just hin, Just hout, Just herr, ph) <- liftIO $ createProcess (proc fp args) { std_in  = CreatePipe
                                                                                          , std_out = CreatePipe
                                                                                          , std_err = CreatePipe
                                                                                          }
            liftIO $ hSetBuffering hin  NoBuffering
            liftIO $ hSetBuffering hout NoBuffering
            liftIO $ hSetBuffering herr NoBuffering

            liftIO $ hSetEncoding hin  latin1
            liftIO $ hSetEncoding hout latin1
            liftIO $ hSetEncoding herr latin1

            -- open SMT logging file
            mlg <- if lgFlag
                    then do timeZone <- liftIO getCurrentTimeZone
                            startTime <- liftIO getCurrentTime
                            let timeString = replace ":" "-" $
                                             replace " " "-" $
                                             show (utcToLocalTime timeZone startTime) in do
                                h <- liftIO $ openFile ("logSMT." ++ timeString ++ ".smt2") WriteMode
                                liftIO $ hSetBuffering h NoBuffering
                                liftIO $ hSetEncoding h latin1
                                return $ Just h
                    else return Nothing

            _ <- liftIO $ forkIO (showErrors herr "SMT WARN >> ")

            put $ SmtState (Just (ActiveSmtState hin hout ph mlg 0))

            --smtPut "(set-option :produce-models true)"
            --smtPut "(set-logic ALL)"
            --smtPut "(set-info :smt-lib-version 2.5)"

-- | close Smt Solver
-- ----------------------------------------------------------------------------------------- --
closeSmtSolver :: SmtM ()
closeSmtSolver = do
    st <- get
    case mActive st of
        Nothing     -> throwError $ Error "No Smt Solver is open to close"
        Just active -> do
                    smtPut "(exit)"
                    case logFileMHandle active of
                        Nothing -> return ()
                        Just h  -> liftIO $ hClose h
                    ec <- liftIO $ waitForProcess (processHandle active)
                    case ec of
                        ExitSuccess   -> return ()
                        ExitFailure c -> throwError $ Error ("Smt Solver exited with error code " ++ show c)
                    put $ SmtState Nothing


-- | get Info of SMT Solver
getInfo :: String -> SmtM String
getInfo topic = do
    smtPut ("(get-info :" ++ topic ++ ")")
    s <- smtGet
    let list = strip s in
        if startswith "(" list && endswith ")" list
        then let tuple = strip (init . tail $ list) in
                case stripPrefix (":" ++ topic) tuple of
                   Just res -> let str = strip res in
                               if startswith "\"" str && endswith "\"" str
                               then return $ init . tail $ str
                               else throwError $ Error ("SMT response violates quotes in pattern.\nExpected (:" ++ topic ++ " \"<name>\")\nActual "++ list)
                   Nothing -> throwError $ Error ("SMT response violates topic in pattern.\nExpected (:" ++ topic ++ " \"<name>\")\nActual "++ list)
        else throwError $ Error ("SMT response violates brackets in pattern.\nExpected (:" ++ topic ++ " \"<name>\")\nActual "++ list)

-- | execute the SMT commands given as a string
smtPut :: String -> SmtM ()
smtPut cmds  = do
        st <- get
        case mActive st of
            Nothing     -> throwError $ Error "No Smt Solver is open for put cmd"
            Just active -> do
                            liftIO $ maybeLog (logFileMHandle active)
                            liftIO $ hPutStrLn (inHandle active) cmds
    where
        -- | write string to Log-file when available
        maybeLog :: Maybe Handle -> IO ()
        maybeLog Nothing  = return ()
        maybeLog (Just h) = liftIO $ hPutStrLn h cmds

-- | show errors
--   The given prefix is prepended to every error line.
showErrors :: Handle -- ^ handle
            -> String -- ^ prefix
            -> IO ()
showErrors h prefix = do
    s <- hIsEOF h
    unless s $ do
                    msg <- hGetLine h
                    hPutStrLn stderr (prefix ++ msg)
                    showErrors h prefix

smtGet :: SmtM String
smtGet = do
        st <- get
        case mActive st of
            Nothing     -> throwError $ Error "No Smt Solver is open for put cmd"
            Just active -> let hout = outHandle active
                               ph = processHandle active
                            in
                                liftIO $ getResponse hout `onException` exitWithError ph
    where
        exitWithError :: ProcessHandle -> IO ()
        exitWithError procHandle = do
            ec <- getProcessExitCode procHandle
            -- The output and error handles of the SMT process are closed when this
            -- error occurs (maybe because they're pipes?) so no information can be
            -- given to the user, other than this.
            error ("smtGet: Could not get a response from the SMT solver\n" ++
                   "smtGet: SMT solver exited with status: " ++ show ec)

-- | read the response (as lines) from the handle
-- this operation is blocking until some data can be read and checks that all brackets are closed.
getResponse :: Handle -> IO String
getResponse h = getResponseCount 0
    where
        getResponseCount :: Integer -> IO String
        getResponseCount count = do
            s <- hGetLine h
            let newCount = count + countBracket s in
                if 0 == newCount
                    then return s
                    else do
                            tl <- getResponseCount newCount
                            return $ s ++ tl

        -- | count the open bracket pairs in the string
        -- i.e. '(' opens a pair and ')' closes a pair.
        -- Note that brackets inside strings (") should be ignored
        countBracket :: String -> Integer
        countBracket ('"':xs) = skipCountInsideString xs          -- ignore brackets inside strings
            where
                skipCountInsideString :: String -> Integer
                skipCountInsideString ('"':'"':ccs) = skipCountInsideString ccs       -- escape quote, stay in string
                skipCountInsideString ('"':cs)      = countBracket cs                 -- outside string
                skipCountInsideString (_:cs)        = skipCountInsideString cs
                skipCountInsideString []            = 0
        countBracket ('(':xs) = 1 + countBracket xs
        countBracket (')':xs) = -1 + countBracket xs
        countBracket (_:xs)   = countBracket xs
        countBracket []       = 0

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
