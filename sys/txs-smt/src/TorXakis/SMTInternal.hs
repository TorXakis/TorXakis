{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}

module TorXakis.SMTInternal

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

import           Constant
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
    toConst :: (Variable v) => EnvDefs -> Map.Map Text SMTValue -> v -> (v, Constant)
    toConst edefs mp v = case Map.lookup (vname v) mp of
                            Just smtValue   -> case smtValueToValExpr smtValue (cstrDefs edefs) (vsort v) of
                                                    Left t -> error $ "getSolution - SMT parse error:\n" ++ t
                                                    Right val -> (v,val)
                            Nothing         -> error "getSolution - SMT hasn't returned the value of requested variable."



-- ----------------------------------------------------------------------------------------- --
-- init
-- ----------------------------------------------------------------------------------------- --





putT :: Text -> SMT ()
putT = put . T.unpack

-- | Transform value expression to an SMT string.
valExprToString :: Variable v => ValExpr v -> SMT Text
valExprToString v = do
  mapI <- gets envNames
  return $ valexprToSMT mapI v





-- ----------------------------------------------------------------------------------------- --





