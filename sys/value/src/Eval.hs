{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE ViewPatterns #-}
module Eval

-- ----------------------------------------------------------------------------------------- --
--
-- Evaluation of Value Expressions
--
-- ----------------------------------------------------------------------------------------- --

where

import Text.Regex.TDFA

import System.IO
import Control.Monad.State
import Control.Exception
import Control.DeepSeq

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import Data.String.Utils

import TxsDefs
import TxsAlex
import TxsHappy
import TxsEnv
import StdTDefs
import TxsShow
import TxsUtils(partSubst)

import XmlFormat

import RegexAlex
import RegexPosixHappy


-- ----------------------------------------------------------------------------------------- --
-- eval :  evaluation of value expression
--         eval should only work on closed vexpr 


eval :: (Variable v) => (ValExpr v) -> IOE Const

eval (view -> Vfunc fid vexps)  =  do
     env     <- get
     let alldefs = envtdefs env
     case Map.lookup fid (funcDefs alldefs) of
     { Nothing                  -> do lift $ hPutStrLn stderr 
                                           $ "TXS Eval eval: undefined function\n"
                                      return $ Cerror "eval: undefined function"
     ; Just (FuncDef args vexp) -> do vals <- mapM eval vexps
                                      we   <- return $ Map.fromList (zip args vals)
                                      eval (cstrEnv (Map.map cstrConst we) vexp)
     }

eval (view -> Vcstr cid vexps)  =  do
     vals <- mapM eval vexps
     return $ Cstr cid vals

eval (view -> Vconst const)  =  do
     return $ const

eval (view -> Vvar vid)  =  do
     lift $ hPutStrLn stderr $ "TXS Eval eval: Evaluation of expression with variable\n"
     return $ Cerror "eval: expression with variable"

eval (view -> Vite conds vexp1 vexp2)  =  do
     cond <- evalCnrs conds
     if  cond
       then eval vexp1
       else eval vexp2

eval (view -> Venv ve vexp)  =  do
     eval (partSubst ve vexp)

eval (view -> Vequal vexp1 vexp2)  =  do
     val1 <- eval vexp1
     val2 <- eval vexp2
     bool2txs ( val1 == val2 )

eval (view -> Vpredef kd fid vexps)  =  do
     case kd of
     { AFS -> case vexps of
              { [vexp]        -> do evalAFS fid vexp
              ; _             -> do lift $ hPutStrLn stderr
                                         $ "TXS Eval eval: error alg. field selection\nfid = "++ (show fid) ++ "\n"
                                    return $ Cerror "error alg. field selection"
              }
     ; ACC -> case vexps of
              { [vexp]        -> do evalACC fid vexp
              ; _             -> do lift $ hPutStrLn stderr
                                         $ "TXS Eval eval: error alg. constructor check\nfid = "++ (show fid) ++ "\n"
                                    return $ Cerror "error alg. constructor check"
              }
     ; ANE -> case vexps of
              { [vexp1,vexp2] -> do evalANE vexp1 vexp2
              ; _             -> do lift $ hPutStrLn stderr
                                         $ "TXS Eval eval: error alg. equality\nfid = "++ (show fid) ++ "\n"
                                    return $ Cerror " error alg. non-equality"
              }
     ; AST -> case vexps of
              { [vexp] -> do wal <- eval vexp
                             str2txs (pshow wal)
              ; _      -> do lift $ hPutStrLn stderr
                                  $ "TXS Eval eval: error AST\n"
                             return $ Cerror " error AST"
              }
     ; ASF -> case vexps of
              { [vexp] -> do s <- txs2str vexp
                             uid     <- gets envuid
                             tdefs   <- gets envtdefs
                             ((uid',vexp'),e) <- lift $ catch
                                      ( let p = vexprParser (  ( Ctdefs  $ tdefs )
                                                             : ( Cvarenv $ [] )
                                                             : ( Cunid   $ uid + 1 )
                                                             : ( txsLexer s )
                                                            )
                                         in return $! (show p) `deepseq` (p,"")
                                      )
                                      ( \e -> return $ ((uid,cstrError ""),(show (e::ErrorCall)))
                                      )
                             if  e /= ""
                               then do lift $ hPutStrLn stderr
                                            $ "TXS Eval eval: error ASF\n"
                                       return $ Cerror " error ASF"
                               else do eval vexp'
              ; _      -> do lift $ hPutStrLn stderr
                                  $ "TXS Eval eval: error ASF\n"
                             return $ Cerror " error ASF"
              }
     ; AXT -> case vexps of
              { [vexp] -> do wal <- eval vexp
                             tdefs <- gets envtdefs
                             str2txs $ constToXml tdefs wal
              ; _      -> do lift $ hPutStrLn stderr "TXS Eval eval: error AXT\n"
                             return $ Cerror " error AXT"
              }
     ; AXF -> case vexps of
                { [vexp] -> do  Cstring s <- eval vexp
                                tdefs <- gets envtdefs
                                return $ constFromXml tdefs (funcsort fid) s
                ; _      -> do  lift $ hPutStrLn stderr "TXS Eval eval: error AXF\n"
                                return $ Cerror " error AXF"
                }
     ; SSB -> do evalSSB fid vexps
     ; SSI -> do evalSSI fid vexps
     ; SSS -> do evalSSS fid vexps
     ; SSR -> do evalSSR fid vexps
     }

eval (view -> Verror str)  =  do
     return $ Cerror str


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic field selection


evalAFS :: (Variable v) => FuncId -> (ValExpr v) -> IOE Const
evalAFS fid vexp  =  do
     tdefs <- gets envtdefs
     val   <- eval vexp
     case val of
     { Cstr cid vals
         -> case Map.lookup cid (cstrDefs tdefs) of
            { Just (CstrDef cc fs)
                -> case List.elemIndices fid fs of
                   { [index] -> return $ vals!!index
                   ; _       -> do lift $ hPutStrLn stderr
                                         $ "TXS Eval evalAFS: no field\nfid = " ++ (show fid) ++ "\nvexp = " ++ (show vexp) ++ "\nval = " ++ (show val) ++ "\ncc = " ++ (show cc) ++ "\n"
                                   return $ Cerror "evalAFS: no field"
                   }
            ; _ -> do lift $ hPutStrLn stderr
                           $ "TXS Eval evalAFS: used constructor not found\n" ++ (show val) ++ "\n"
                      return $ Cerror "evalAFS: used constructor not found"
            }
     ; _ -> do lift $ hPutStrLn stderr 
                    $ "TXS Eval evalAFS: alg. field selection not on constructor\n" ++ (show val) ++ "\n"
               return $ Cerror "evalAFS: alg. field selection not on constructor"
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic constructor check


evalACC :: (Variable v) => FuncId -> (ValExpr v) -> IOE Const
evalACC fid vexp  =  do
     tdefs <- gets envtdefs
     val   <- eval vexp
     case val of
     { Cstr cid vals
         -> case Map.lookup cid (cstrDefs tdefs) of
            { Just (CstrDef cc fs)  -> bool2txs (cc == fid)
            ; _                     -> do lift $ hPutStrLn stderr
                                               $ "TXS Eval evalACC: constructor not found\n" ++ (show val) ++ "\n"
                                          return $ Cerror "evalACC: constructor not found"
            }
     ; _ -> do lift $ hPutStrLn stderr
                    $ "TXS Eval evalACC: alg. constructor check not on constructor\n" ++ (show val) ++ "\n"
               return $ Cerror "evalACC: alg. constructor check not on constructor"
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic (non)equality


evalAEQ :: (Variable v) => (ValExpr v) -> (ValExpr v) -> IOE Const
evalAEQ vexp1 vexp2  =  do
     val1 <- eval vexp1
     val2 <- eval vexp2
     bool2txs $ val1 == val2


evalANE :: (Variable v) => (ValExpr v) -> (ValExpr v) -> IOE Const
evalANE vexp1 vexp2  =  do
     val1 <- eval vexp1
     val2 <- eval vexp2
     bool2txs $ not ( val1 == val2 )


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Bool - SSB


evalSSB :: (Variable v) => FuncId -> [ValExpr v] -> IOE Const
evalSSB (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "toString",    [v1]    ) -> do b1 <- txs2bool v1
                                        str2txs $ show b1
     ; ( "fromString",  [v1]    ) -> do s1 <- txs2str v1
                                        bool2txs $ read s1
     ; ( "toXml",       [v1]    ) -> do wal <- eval v1
                                        tdefs <- gets envtdefs
                                        str2txs $ constToXml tdefs wal          
     ; ( "fromXml",     [v1]    ) -> do Cstring s <- eval v1
                                        tdefs <- gets envtdefs
                                        return $ constFromXml tdefs sortId_Bool s
     ; ( "<>",          [v1,v2] ) -> do b1 <- txs2bool v1
                                        b2 <- txs2bool v2
                                        bool2txs $ b1 /= b2
     ; ( "not",         [v1]    ) -> do b1 <- txs2bool v1
                                        bool2txs $ not b1
     ; ( "/\\",         [v1,v2] ) -> do b1 <- txs2bool v1
                                        if b1 
                                        then do
                                            b2 <- txs2bool v2
                                            bool2txs b2
                                        else 
                                            bool2txs False 
     ; ( "\\/",         [v1,v2] ) -> do b1 <- txs2bool v1
                                        if b1 
                                            then bool2txs True
                                            else do
                                                b2 <- txs2bool v2
                                                bool2txs b2
     ; ( "\\|/",        [v1,v2] ) -> do b1 <- txs2bool v1
                                        b2 <- txs2bool v2
                                        bool2txs $ b1 /= b2
     ; ( "=>",          [v1,v2] ) -> do b1 <- txs2bool v1
                                        if b1 
                                            then do
                                                b2 <- txs2bool v2
                                                bool2txs b2
                                            else 
                                                bool2txs True
     ; ( "<=>",         [v1,v2] ) -> do b1 <- txs2bool v1
                                        b2 <- txs2bool v2
                                        bool2txs $ b1 == b2
     ; _                         -> do lift $ hPutStrLn stderr
                                            $ "TXS Eval evalSSB: standard Bool opn\n"
                                       return $ Cerror "evalSSB: standard Bool opn"
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Int - SSI


evalSSI :: (Variable v) => FuncId -> [ValExpr v] -> IOE Const
evalSSI (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "toString",    [v1]    ) -> do i1 <- txs2int v1
                                        str2txs $ show i1
     ; ( "fromString",  [v1]    ) -> do s1 <- txs2str v1
                                        int2txs $ read s1
     ; ( "toXml",       [v1]    ) -> do wal <- eval v1
                                        tdefs <- gets envtdefs
                                        str2txs $ constToXml tdefs wal
     ; ( "fromXml",     [v1]    ) -> do Cstring s <- eval v1
                                        tdefs <- gets envtdefs
                                        return $ constFromXml tdefs sortId_Int s
     ; ( "+",           [v1]    ) -> do i1 <- txs2int v1
                                        int2txs $ i1
     ; ( "-",           [v1]    ) -> do i1 <- txs2int v1
                                        int2txs $ (-1) * i1
     ; ( "+",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 + i2
     ; ( "-",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 - i2
     ; ( "*",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 * i2
     ; ( "/",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 `div` i2
     ; ( "%",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs  $ i1 `mod` i2
--     ; ( "^",          [v1,v2] ) -> do i1 <- txs2int v1
--                                       i2 <- txs2int v2
--                                       int2txs $ i1 ^ i2
     ; ( "<>",          [v1,v2] ) -> do i1 <- txs2int v1 
                                        i2 <- txs2int v2
                                        bool2txs $ i1 /= i2
     ; ( "<",           [v1,v2] ) -> do i1 <- txs2int v1 
                                        i2 <- txs2int v2
                                        bool2txs $ i1 < i2
     ; ( "<=",          [v1,v2] ) -> do i1 <- txs2int v1 
                                        i2 <- txs2int v2
                                        bool2txs $ i1 <= i2
     ; ( ">",           [v1,v2] ) -> do i1 <- txs2int v1 
                                        i2 <- txs2int v2
                                        bool2txs $ i1 > i2
     ; ( ">=",          [v1,v2] ) -> do i1 <- txs2int v1 
                                        i2 <- txs2int v2
                                        bool2txs $ i1 >= i2
     ; ( "abs",         [v1]    ) -> do i1 <- txs2int v1 
                                        int2txs $ if i1<0 then -i1 else i1
     ; _                         -> do lift $ hPutStrLn stderr
                                            $ "TXS Eval evalSSI: standard Int opn\n"
                                       return $ Cerror "evalSSI: standard Int opn"
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for String - SSS


evalSSS :: (Variable v) => FuncId -> [ValExpr v] -> IOE Const
evalSSS (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "toString",   [v] ) -> do s <- txs2str v
                                   str2txs $ show s
     ; ( "fromString", [v] ) -> do s <- txs2str v
                                   str2txs $ read s
     ; ( "toXml",      [v] ) -> do wal <- eval v
                                   tdefs <- gets envtdefs
                                   str2txs $ constToXml tdefs wal
     ; ( "fromXml",    [v] ) -> do Cstring s <- eval v
                                   tdefs <- gets envtdefs
                                   return $ constFromXml tdefs sortId_String s
     ; ( "<>",     [v1,v2] ) -> do s1 <- txs2str v1
                                   s2 <- txs2str v2
                                   bool2txs $ s1 /= s2
     ; ( "++",     [v1,v2] ) -> do s1 <- txs2str v1
                                   s2 <- txs2str v2
                                   str2txs $ s1 ++ s2
     ; ( "len",    [v1]    ) -> do s1 <- txs2str v1 
                                   int2txs  $ toInteger (length s1)
     ; ( "takeWhile",    [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ takeWhile (`elem` s1) s2
     ; ( "takeWhileNot", [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ takeWhile (`notElem` s1) s2
     ; ( "dropWhile",    [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ dropWhile (`elem` s1) s2
     ; ( "dropWhileNot", [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ dropWhile (`notElem` s1) s2
     ; ( "at", [v1,v2] )           -> do s1 <- txs2str v1
                                         i2 <- txs2int v2
                                         i2' <- return $ fromInteger i2
                                         str2txs $ if 0<=i2' && i2'<(length s1)
                                                     then [s1!!i2'] else ""
     ; _                           -> do lift $ hPutStrLn stderr
                                              $ "TXS Eval evalSSS: standard String opn\n"
                                         return $ Cerror "evalSSS: standard String opn"
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Regex - SSR


evalSSR :: (Variable v) => FuncId -> [ValExpr v] -> IOE Const
evalSSR (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "strinre",[v1,v2] ) -> do rawRegex     <- txs2regex v2
                                   haskellRegex <- return $ regexPosixParser (regexLexer rawRegex)
                                   value <- txs2str v1
                                   bool2txs $ value =~ haskellRegex
     ; _ -> do lift $ hPutStrLn stderr $ "TXS Eval evalSSR: error in standard Regex opn\n"
               return $ Cerror "evalSSR: error in standard Regex opn"
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: values from torxakis to haskell and v.v.


txs2bool :: (Variable v) => (ValExpr v) -> IOE Bool
txs2bool vexp  =  do
     wal <- eval vexp
     case wal of
        Cbool b -> return b
        _       -> do
                        lift $ hPutStrLn stderr $ "TXS Eval txs2bool: not on Bool\nbut on: " ++ (show wal)++"\n"
                        return False  -- PvdL: why False?


bool2txs :: Bool -> IOE Const
bool2txs b = return $ Cbool b


txs2int :: (Variable v) => (ValExpr v) -> IOE Integer
txs2int vexp  =  do
     wal <- eval vexp
     case wal of
     { Cint i   -> do return $ i
     ; v        -> do lift $ hPutStrLn stderr
                                $ "TXS Eval txs2int: not on Int:\nbut on: " ++ (show v)++"\n"
                      return $ 0
     }

int2txs :: Integer -> IOE Const
int2txs i  =  do
     return $ Cint i


txs2str :: (Variable v) => (ValExpr v) -> IOE String
txs2str vexp  =  do
     wal <- eval vexp
     case wal of
     { Cstring s -> do return $ s
     ; v         -> do lift $ hPutStrLn stderr 
                                $ "TXS Eval txs2str: not on String\nbut on : " ++ (show v) ++ "\n"
                       return $ "txs2str: not on String"
     }


str2txs :: String -> IOE Const
str2txs s  =  do
     return $ Cstring s


txs2regex :: (Variable v) => (ValExpr v) -> IOE String
txs2regex vexp  =  do
     wal <- eval vexp
     case wal of
     { Cregex r -> do return $ r
     ; v        -> do lift $ hPutStrLn stderr
                                $ "TXS Eval txs2regex: not on Regex:\nbut on: " ++ (show v) ++ "\n"
                                ++ "VExp: " ++ (show vexp)
                      return $ "txs2str: not on Regex"
     }


regex2txs :: String -> IOE Const
regex2txs r  =  do
     return $ Cregex r


-- ----------------------------------------------------------------------------------------- --
-- evaluation of constraints

evalCnr :: (Variable v) => (ValExpr v) -> IOE Bool
evalCnr vexp  =  do
     val <- eval vexp     
     return $ val == Cbool True

evalCnrs :: (Variable v) => [ValExpr v] -> IOE Bool
evalCnrs []  = return True
evalCnrs (vexp:vexps) = do
    val <- evalCnr vexp
    if val then
        evalCnrs vexps
    else
        return False

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

