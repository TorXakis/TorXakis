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
--
-- Evaluation of Value Expressions
--
-- ----------------------------------------------------------------------------------------- --
-- export

( eval       -- eval :: (TxsDefs.Variable v) => (TxsDefs.ValExpr v) -> IOB.IOB Const
             -- evaluation of value expression; eval shall only work on closed vexpr
, evalCnrs   -- evalCnrs :: (Variable v) => [ValExpr v] -> IOB.IOB Bool
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Text.Regex.TDFA

import System.IO
import Control.Monad.State
import Control.Exception
import Control.DeepSeq

import qualified Data.List as List
import qualified Data.Map  as Map
import Data.String.Utils

-- import from behavedefs
import qualified EnvBTree  as IOB
import qualified EnvData   as EnvData

-- import from defs
import StdTDefs
import TxsDefs
import TxsShow
import TxsUtils 
import XmlFormat
 
-- import from front
import qualified TxsAlex   as TxsAlex
import qualified TxsHappy  as TxsHappy

-- import from solve
import RegexAlex
import RegexPosixHappy


-- ----------------------------------------------------------------------------------------- --
-- eval :  evaluation of value expression
--         eval shall only work on closed vexpr 


eval :: (TxsDefs.Variable v) => (TxsDefs.ValExpr v) -> IOB.IOB Const

eval (view -> Vfunc fid vexps)  =  do
     envb    <- get
     let tdefs = IOB.tdefs envb
     case Map.lookup fid (funcDefs tdefs) of
       Nothing -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                   $ "Undefined function: " ++ (fshow fid) ]
                     return $ Cerror "eval: undefined function"
       Just (FuncDef args vexp)
               -> do vals <- mapM eval vexps
                     we   <- return $ Map.fromList (zip args vals)
                     eval (cstrEnv (Map.map cstrConst we) vexp)

eval (view -> Vcstr cid vexps)  =  do
     vals <- mapM eval vexps
     return $ Cstr cid vals

eval (view -> Viscstr cid1 arg) = do
     Cstr cid2 _ <- eval arg
     bool2txs ( cid1 == cid2 )
     
eval (view -> Vaccess _cid1 p arg) = do
     Cstr _cid2 args <- eval arg
     return $ args!!p                   -- TODO: check cids are equal?

eval (view -> Vconst const)  =  do
     return $ const

eval (view -> Vvar vid)  =  do
     IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                   $ "Evaluation of value expression with free variable(s)" ]
     return $ Cerror ""

eval (view -> Vite conds vexp1 vexp2)  =  do
     cond <- evalCnrs conds
     if  cond
       then eval vexp1
       else eval vexp2

eval (view -> Venv ve vexp)  =  do
     eval (TxsUtils.partSubst ve vexp)

eval (view -> Vequal vexp1 vexp2)  =  do
     val1 <- eval vexp1
     val2 <- eval vexp2
     bool2txs ( val1 == val2 )

eval (view -> Vpredef kd fid vexps)  =  do
     case kd of
       AFS -> case vexps of
                [vexp]        -> do evalAFS fid vexp
                _             -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                  $ "eval: AFS: " ++ (show fid) ]
                                    return $ Cerror ""
       ACC -> case vexps of
                [vexp]        -> do evalACC fid vexp
                _             -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                  $ "eval: ACC: " ++ (show fid) ]
                                    return $ Cerror ""
       ANE -> case vexps of
                [vexp1,vexp2] -> do evalANE vexp1 vexp2
                _             -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                  $ "eval: ANE: " ++ (show fid) ]
                                    return $ Cerror ""
       AST -> case vexps of
                [vexp] -> do wal <- eval vexp
                             str2txs (pshow wal)
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: AST" ]
                             return $ Cerror ""
       ASF -> case vexps of
                [vexp] -> do s <- txs2str vexp
                             uid     <- gets IOB.unid
                             tdefs   <- gets IOB.tdefs
                             sigs    <- gets IOB.sigs
                             ((uid',vexp'),e) <- lift $ catch
                                ( let p = TxsHappy.vexprParser (  ( TxsAlex.Ctdefs  tdefs )
                                                                : ( TxsAlex.Csigs   sigs )
                                                                : ( TxsAlex.Cvarenv [] )
                                                                : ( TxsAlex.Cunid (uid + 1) )
                                                                : ( TxsAlex.txsLexer s )
                                                               )
                                   in return $! (show p) `deepseq` (p,"")
                                )
                                ( \e -> return $ ((uid,cstrError ""), (show (e::ErrorCall)))
                                )
                             if  e /= ""
                               then do IOB.putMsgs $ map EnvData.TXS_CORE_SYSTEM_ERROR
                                         [ "eval: ASF"
                                         , "vexpr: " ++ s
                                         , "signatures" ++ show sigs
                                         ]
                                       return $ Cerror ""
                               else do eval vexp'
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: ASF" ]
                             return $ Cerror ""
       AXT -> case vexps of
                [vexp] -> do wal <- eval vexp
                             tdefs <- gets IOB.tdefs
                             str2txs $ constToXml tdefs wal
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval AXT" ]
                             return $ Cerror ""
       AXF -> case vexps of
                  [vexp] -> do Cstring s <- eval vexp
                               tdefs <- gets IOB.tdefs
                               return $ constFromXml tdefs (funcsort fid) s
                  _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: AXF" ]
                               return $ Cerror ""
       SSB -> do evalSSB fid vexps
       SSI -> do evalSSI fid vexps
       SSS -> do evalSSS fid vexps
       SSR -> do evalSSR fid vexps

eval (view -> Verror str)  =  do
     IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_WARNING $ "eval: Verror "++ str ]
     return $ Cerror ""


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic field selection


evalAFS :: (Variable v) => FuncId -> (ValExpr v) -> IOB.IOB Const
evalAFS fid vexp  =  do
     tdefs <- gets IOB.tdefs
     val   <- eval vexp
     case val of
     { Cstr cid vals
         -> case Map.lookup cid (cstrDefs tdefs) of
            { Just (CstrDef cc fs)
                -> case List.elemIndices fid fs of
                   { [index] -> return $ vals!!index
                   ; _       -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $
                                                     "evalAFS: no field: " ++ (fshow fid) ]
                                   return $ Cerror ""
                   }
            ; _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                    $ "evalAFS: used constructor not found: " ++ (fshow val) ]
                      return $ Cerror ""
            }
     ; _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                             $ "evalAFS: fiels selection not on constructor:" ++ (fshow val) ]
               return $ Cerror ""
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic constructor check


evalACC :: (Variable v) => FuncId -> (ValExpr v) -> IOB.IOB Const
evalACC fid vexp  =  do
     tdefs <- gets IOB.tdefs
     val   <- eval vexp
     case val of
     { Cstr cid vals
         -> case Map.lookup cid (cstrDefs tdefs) of
            { Just (CstrDef cc fs) -> bool2txs (cc == fid)
            ; _                    -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                           $ "evalACC: constructor not found: " ++ (show val) ]
                                         return $ Cerror "evalACC: constructor not found"
            }
     ; _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                             $ "evalACC: constr check not on constructor: " ++ (show val) ]
               return $ Cerror ""
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic (non)equality


evalAEQ :: (Variable v) => (ValExpr v) -> (ValExpr v) -> IOB.IOB Const
evalAEQ vexp1 vexp2  =  do
     val1 <- eval vexp1
     val2 <- eval vexp2
     bool2txs $ val1 == val2


evalANE :: (Variable v) => (ValExpr v) -> (ValExpr v) -> IOB.IOB Const
evalANE vexp1 vexp2  =  do
     val1 <- eval vexp1
     val2 <- eval vexp2
     bool2txs $ not ( val1 == val2 )


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Bool - SSB


evalSSB :: (Variable v) => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSB (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "toString",    [v1]    ) -> do b1 <- txs2bool v1
                                        str2txs $ show b1
     ; ( "fromString",  [v1]    ) -> do s1 <- txs2str v1
                                        bool2txs $ read s1
     ; ( "toXml",       [v1]    ) -> do wal <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        str2txs $ constToXml tdefs wal          
     ; ( "fromXml",     [v1]    ) -> do Cstring s <- eval v1
                                        tdefs <- gets IOB.tdefs
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
     ; _                          -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                      $ "evalSSB: standard Bool opn" ]
                                        return $ Cerror ""
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Int - SSI


evalSSI :: (Variable v) => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSI (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "toString",    [v1]    ) -> do i1 <- txs2int v1
                                        str2txs $ show i1
     ; ( "fromString",  [v1]    ) -> do s1 <- txs2str v1
                                        int2txs $ read s1
     ; ( "toXml",       [v1]    ) -> do wal <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        str2txs $ constToXml tdefs wal
     ; ( "fromXml",     [v1]    ) -> do Cstring s <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        return $ constFromXml tdefs sortId_Int s
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
     ; _                          -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                      $ "evalSSI: standard Int opn" ]
                                        return $ Cerror ""
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for String - SSS


evalSSS :: (Variable v) => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSS (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "toString",   [v] ) -> do s <- txs2str v
                                   str2txs $ show s
     ; ( "fromString", [v] ) -> do s <- txs2str v
                                   str2txs $ read s
     ; ( "toXml",      [v] ) -> do wal <- eval v
                                   tdefs <- gets IOB.tdefs
                                   str2txs $ constToXml tdefs wal
     ; ( "fromXml",    [v] ) -> do Cstring s <- eval v
                                   tdefs <- gets IOB.tdefs
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
     ; _                           -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                       $ "evalSSS: standard String opn" ]
                                         return $ Cerror ""
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Regex - SSR


evalSSR :: (Variable v) => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSR (FuncId nm uid args srt) vexps  =  do
     case ( nm, vexps ) of
     { ( "strinre",[v1,v2] ) -> do rawRegex     <- txs2regex v2
                                   haskellRegex <- return
                                                     $ regexPosixParser (regexLexer rawRegex)
                                   value <- txs2str v1
                                   bool2txs $ value =~ haskellRegex
     ; _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                             $ "evalSSR: error in standard Regex opn" ]
               return $ Cerror ""
     }


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: values from torxakis to haskell and v.v.


txs2bool :: (Variable v) => (ValExpr v) -> IOB.IOB Bool
txs2bool vexp  =  do
     wal <- eval vexp
     case wal of
        Cbool b -> return b
        _       -> do
                        IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                              $ "txs2bool: not on Bool: " ++ (show wal) ]
                        return False  -- PvdL: why False?


bool2txs :: Bool -> IOB.IOB Const
bool2txs b = return $ Cbool b


txs2int :: (Variable v) => (ValExpr v) -> IOB.IOB Integer
txs2int vexp  =  do
     wal <- eval vexp
     case wal of
     { Cint i -> do return $ i
     ; v      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                  $ "txs2int: not on Int: " ++ (show v) ]
                    return $ 0
     }

int2txs :: Integer -> IOB.IOB Const
int2txs i  =  do
     return $ Cint i


txs2str :: (Variable v) => (ValExpr v) -> IOB.IOB String
txs2str vexp  =  do
     wal <- eval vexp
     case wal of
     { Cstring s -> do return $ s
     ; v         -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                     $ "txs2str: not on String: " ++ (show v) ]
                       return $ ""
     }


str2txs :: String -> IOB.IOB Const
str2txs s  =  do
     return $ Cstring s


txs2regex :: (Variable v) => (ValExpr v) -> IOB.IOB String
txs2regex vexp  =  do
     wal <- eval vexp
     case wal of
     { Cregex r -> do return $ r
     ; v        -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                    $ "txs2regex: not on Regex: " ++ (show v) ]
                      return $ ""
     }


regex2txs :: String -> IOB.IOB Const
regex2txs r  =  do
     return $ Cregex r


-- ----------------------------------------------------------------------------------------- --
-- evaluation of constraints

evalCnr :: (Variable v) => (ValExpr v) -> IOB.IOB Bool
evalCnr vexp  =  do
     val <- eval vexp     
     return $ val == Cbool True

evalCnrs :: (Variable v) => [ValExpr v] -> IOB.IOB Bool
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

