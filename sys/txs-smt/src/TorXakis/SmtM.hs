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
, mkSmtState
, destroySmtState
)
where
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Bimap
import           Data.HashMap
import           Data.List
import           Data.Stack
import           Data.String.Utils
import qualified Data.Text as T
import           Data.Time
import           Numeric
import           System.Exit
import           System.IO
import           System.Process

import           TorXakis.ContextFunc
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.ProblemSolver
import           TorXakis.Sort
import           TorXakis.Var

type ADTMap  = Data.HashMap.Map (RefByName ADTDef) T.Text
type FuncMap = Data.HashMap.Map RefByFuncSignature T.Text
type VarMap  = Data.Bimap.Bimap (RefByName VarDef) T.Text
type CstrMap = Data.Bimap.Bimap (RefByName ADTDef, RefByName ConstructorDef) T.Text

-- | Smt State
data SmtState = SmtState { inHandle       :: Handle
                         , outHandle      :: Handle
                         , processHandle  :: ProcessHandle
                         , logFileMHandle :: Maybe Handle
                         , depth          :: Integer
                         -- defined definitions
                         , funcContext :: ContextFunc
                         , varDefs     :: Stack (NameMap VarDef)
                         -- translation to SMT (and from SMT when necessary)
                         , adtId     :: Integer
                         , adtToSmt  :: ADTMap -- `a + show adtId`
                         , funcId    :: Integer
                         , funcToSmt :: FuncMap -- `f + show funcId`
                         , varId     :: Integer
                         , varToSmt  :: VarMap -- `v + show varId`
                         , cstrToSmt :: CstrMap  -- `c + show adtId + $ + show (local cstrId)`
                         }

-- | mkSmtState - make Smt State
--
-- * open Smt Solver
--
-- * handle input, output and errors from Smt Solver
--
-- * when wanted, log Smt commands
--
-- * Bring Smt Solver to desired initial state
mkSmtState :: FilePath -> [String] -> Bool -> IO (Either Error SmtState)
mkSmtState fp as lgFlag = do
        -- start smt solver process
        (Just hin, Just hout, Just herr, ph) <-  createProcess (proc fp as) { std_in  = CreatePipe
                                                                            , std_out = CreatePipe
                                                                            , std_err = CreatePipe
                                                                            }
        hSetBuffering hin  NoBuffering
        hSetBuffering hout NoBuffering
        hSetBuffering herr NoBuffering

        hSetEncoding hin  latin1
        hSetEncoding hout latin1
        hSetEncoding herr latin1

        -- open SMT logging file
        mlg <- if lgFlag
                then do timeZone <- getCurrentTimeZone
                        startTime <- getCurrentTime
                        let timeString = replace ":" "-" $
                                         replace " " "-" $
                                         show (utcToLocalTime timeZone startTime) in do
                            h <-  openFile ("logSMT." ++ timeString ++ ".smt2") WriteMode
                            hSetBuffering h NoBuffering
                            hSetEncoding h latin1
                            return $ Just h
                else return Nothing

        _ <-  forkIO (showErrors herr "SMT WARN >> ")

        es <- runExceptT $ execStateT ( toStateT (do
                                                    smtPut "(set-option :produce-models true)"
                                                    smtPut "(set-logic ALL)"
                                                    smtPut "(set-info :smt-lib-version 2.5)"
                                                )
                                      )
                                      ( SmtState hin
                                                 hout
                                                 ph
                                                 mlg
                                                 0
                                                 TorXakis.ContextFunc.empty
                                                 stackNew
                                                 0
                                                 Data.HashMap.empty
                                                 0
                                                 Data.HashMap.empty
                                                 0
                                                 Data.Bimap.empty
                                                 Data.Bimap.empty
                                      )
        case es of
            Left err -> return $ Left err
            Right s  -> return $ Right s

-- | destroySmtState
--
-- * exit Smt Solver, and check its exit code
--
-- * close log file
destroySmtState :: SmtState -> IO (Maybe Error)
destroySmtState s = do
    response <- runExceptT $ runStateT ( toStateT (smtPut "(exit)") ) s
    case logFileMHandle s of
        Nothing -> return ()
        Just h  -> hClose h
    case response of
            Left err -> return $ Just err
            Right _  -> do
                            ec <- waitForProcess (processHandle s)
                            case ec of
                                ExitSuccess   -> return Nothing
                                ExitFailure c -> return $ Just (Error ("Smt Solver exited with error code " ++ show c))


-- | Smt Monad
newtype SmtM a = SmtM { -- | to `StateT`
                        toStateT :: StateT SmtState (ExceptT Error IO) a
                      }
                      deriving (Functor, Applicative, Monad, MonadState SmtState, MonadIO, MonadError Error)


instance ProblemSolver SmtM where
    info = do
                n <- getInfo "name"
                v <- getInfo "version"
                return $ "SMT solver using " ++ n ++ " [" ++ v ++ "]"

    addADTs as =
            do
                st <- get
                if TorXakis.SmtM.depth st > 0
                then throwError $ Error "AddADTs only a depth 0. Depth is controlled by push and pop."
                else case as of
                        [] -> return ()
                        _ -> case TorXakis.ContextFunc.addADTs as (TorXakis.SmtM.funcContext st) of
                                Left e -> throwError $ Error ("Smt ProblemSolver - addADTs failed due to " ++ show e)
                                Right funcContext' ->
                                    let aId = adtId st
                                        aId' = aId + toInteger (length as)
                                        cTs = cstrToSmt st
                                        aTs = adtToSmt st
                                        (aTs', cTs') = foldl insertADT (aTs, cTs) $ zip as [aId ..]
                                        in do
                                            put $ st{ TorXakis.SmtM.funcContext = funcContext'
                                                    , TorXakis.SmtM.adtId       = aId'
                                                    , TorXakis.SmtM.adtToSmt    = aTs'
                                                    , TorXakis.SmtM.cstrToSmt   = cTs'
                                                    }
        where
            insertADT :: (ADTMap, CstrMap) -> (ADTDef, Integer) -> (ADTMap, CstrMap)
            insertADT (am, cm) (a, i) =
                    ( Data.HashMap.insert aRef toSmtAdtText am
                    , foldl (flip (uncurry Data.Bimap.insert)) cm $ zip (fmap toRefTuple (elemsConstructor a)) (fmap toSmtCstrText [0..])
                    )
                where
                    aRef :: RefByName ADTDef
                    aRef = RefByName (adtName a)

                    iText :: T.Text
                    iText = T.pack (showHex i "")

                    toSmtAdtText :: T.Text
                    toSmtAdtText = T.append (T.singleton 'a') iText

                    toRefTuple :: ConstructorDef -> (RefByName ADTDef, RefByName ConstructorDef)
                    toRefTuple cd = (aRef, RefByName (constructorName cd))

                    toSmtCstrText :: Integer -> T.Text
                    toSmtCstrText c = T.concat [ T.singleton 'c'
                                               , iText
                                               , T.singleton '$'
                                               , T.pack (showHex c "")
                                               ]

{-(fmap
                                            -- | convert sort definitions to SMT type declarations (as multiple lines of commands)
sortdefsToSMT :: EnvNames -> EnvDefs -> Text
sortdefsToSMT enames edefs =
    let sorts = Map.keys (sortDefs edefs) in
        case sorts of
            []      -> ""
            _       -> "(declare-datatypes () (\n"
                       <> foldMap (\s -> "    (" <> justLookupSort s enames <> foldMap cstrToSMT (getCstrs s) <> ")\n" )
                                  sorts
                       <> ") )\n"
    where
        -- get the constructors of an ADT
        getCstrs :: SortId -> [(CstrId, CstrDef)]
        getCstrs s = [(cstrId', cstrDef) | (cstrId', cstrDef) <- Map.toList (cstrDefs edefs), cstrsort cstrId' == s]

        -- convert the given constructor to a SMT constructor declaration
        cstrToSMT :: (CstrId, CstrDef) -> Text
        cstrToSMT (cstrId', CstrDef _ fields) = " (" <> justLookupCstr cstrId' enames
                                                     <> cstrFieldsToSMT cstrId' fields
                                                     <> ")"

        -- convert the given constructor fields to a SMT constructor declaration
        cstrFieldsToSMT :: CstrId -> [FuncId] -> Text
        cstrFieldsToSMT cstrId' fields =
            case fields of
                []  -> ""
                _   -> " (" <> T.intercalate ") (" (map (\(f,p) -> toFieldName cstrId' p <> " " <> justLookupSort (funcsort f) enames)
                                                        (zip fields [0..]) ) <> ")"
-}





    addFunctions _ = undefined

    push = do
            smtPut "(push 1)"
            -- update depth (not returned by smt solver)
            st <- get
            let newDepth = succ (TorXakis.SmtM.depth st)
                in do
                    put $ st { TorXakis.SmtM.depth = newDepth }
                    return newDepth

    pop = do
            st <- get
            let d = TorXakis.SmtM.depth st in
                if d > 0
                then let newDepth = pred d
                        in do
                            smtPut "(pop 1)"
                            put $ st{ TorXakis.SmtM.depth = newDepth }
                            return newDepth
                else throwError $ Error "Attempt to pop while the stack is empty."

    depth = gets TorXakis.SmtM.depth

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
        liftIO $ maybeLog (logFileMHandle st)
        liftIO $ hPutStrLn (inHandle st) cmds
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
            let hout = outHandle st
                ph = processHandle st
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
