{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module ReadObjs

-- ----------------------------------------------------------------------------------------- --
--
--   Module for parsing/reading TXS objects
--
-- ----------------------------------------------------------------------------------------- --
-- export

(
{-
 readOffers   -- :: Id -> Sigs VarId -> D.VEnv -> [D.ChanId] -> String
               -- -> Either String (Id, Set.Set D.Offer)
, readAction   -- :: Id -> Sigs VarId -> D.VEnv -> [D.ChanId] -> String
               -- -> Either String (Id, DD.Action)
, readBExpr    -- :: Id -> Sigs VarId -> D.VEnv -> [D.ChanId] -> String
               -- -> Either String (Id, D.BExpr)
-}
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

{-

-- import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
-- import           Control.Monad.State
-- import qualified Data.Char           as Char
-- import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set
-- import qualified Data.Text           as T
-- import           Network             hiding (socketPort)
-- import           Network.Socket      hiding (accept, sClose)
-- import           System.IO
-- import from local
-- import           CmdLineParser
-- import           ToProcdef
-- import qualified TxsServerConfig     as SC

-- import from serverenv
-- import qualified EnvServer           as IOS
-- import qualified IfServer            as IFS


-- import from core
-- import qualified BuildInfo
import qualified TxsCore
-- import qualified TxsManual
-- import qualified VersionInfo

-- import from defs
import qualified TxsDefs             as D
import qualified TxsDDefs            as DD
import           Sigs     (Sigs)
import           Id

-- import qualified TxsShow
import qualified Utils
import           VarId    (VarId)

-- import from valexpr
-- -- import qualified ValExpr

-- import from front
import qualified TxsAlex
import qualified TxsHappy

-- import from cnect
-- import SockExplW
-- import SockImplW

-}

-- ----------------------------------------------------------------------------------------- --
-- readOffers :  read Offers from String


{-

readOffers :: Id
           -> Sigs VarId
           -> D.VEnv
           -> [D.ChanId]
           -> String
           -> IO (Either String (Id, Set.Set D.Offer))
readOffers uid sigs vals chids s  =  do
     ((uid',offs'),e) <- catch ( let p = TxsHappy.prefoffsParser
                                            ( TxsAlex.Csigs    sigs
                                            : TxsAlex.Cchanenv chids
                                            : TxsAlex.Cvarenv  (Map.keys vals)
                                            : TxsAlex.Cunid    (_id uid + 1)
                                            : TxsAlex.txsLexer s
                                            )
                                  in return $!! (p,"")
                               )
                               ( \e -> return ((uid,Set.empty),show (e::ErrorCall)))
     if  e == ""
       then return $ Right (uid', offs')
       else return $ Left $ "ReadOffers: incorrect offer: " ++ e


-- ----------------------------------------------------------------------------------------- --
-- readAction  :  read Action from String

readAction :: Id
           -> Sigs VarId
           -> D.VEnv
           -> [D.ChanId]
           -> String
           -> IO (Either String (Id, DD.Action))
readAction uid sigs vals chids s  =  do
     eith <- readOffers uid sigs vals chids s
     case eith of
       Right (uid', offs)
         -> do let qstnoffs = [ q
                              | q@D.Quest{} <- concatMap D.chanoffers (Set.toList offs)
                              ]
               if  null qstnoffs
                 then do
?????              acts <- sequence [ Utils.liftP2 (chid, sequence [ TxsCore.txsEval vexp
                                                                   | D.Exclam vexp <- choffs
                                                                   ]
                                                   )
                                    | D.Offer chid choffs <- Set.toList offs
                                    ]
                   return $ Right (uid', DD.Act (Set.fromList acts))
                 else return $ Left $ "ReadAction: incorrect action (no question mark allowed)"
       Left e
         -> Left $ "ReadAction: " ++ e


-- ----------------------------------------------------------------------------------------- --
-- readBExpr :  read BExpr from String

readBExpr :: Id
          -> Sigs VarId
          -> D.VEnv
          -> [D.ChanId]
          -> String
          -> IO (Either String (Id, D.BExpr))
readBExpr uid sigs vals chids s  =  do
     ((uid',bexpr'),e) <- catch ( let p = TxsHappy.bexprParser
                                             ( TxsAlex.Csigs    sigs
                                             : TxsAlex.Cchanenv chids
                                             : TxsAlex.Cvarenv  (Map.keys vals)
                                             : TxsAlex.Cunid    (_id uid + 1)
                                             : TxsAlex.txsLexer s 
                                             ) 
                                   in return $!! (p,"")
                                )
                                ( \e -> return ((uid, D.Stop), show (e::ErrorCall)))
     if  e == ""
       then return $ Right (uid', bexpr')
       else return $ Left $ "ReadBexpr: incorrect behaviour expression: " ++ e

-}

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

