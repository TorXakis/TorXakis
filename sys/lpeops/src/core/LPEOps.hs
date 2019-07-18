{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEOps
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEOps (
lpeOpsVersion,
LPEOp(..),
LPEOperation,
lpeOperations,
discardLPE,
printLPE,
printAbbrevLPE,
exportLPE,
exportAbbrevLPE,
getLPEOperation,
getLPEOperations
) where

import qualified Control.Monad.State as MonadState
import qualified Data.Text as Text
import qualified Text.Read as Read
import qualified EnvCore as IOC
import qualified EnvData
import qualified TxsDefs
import qualified LPEClean
import qualified LPEConstElm
import qualified LPEParElm
-- import qualified LPEIStepElm
import qualified LPEDataReset
import qualified LPEParReset
import qualified LPEConfCheck
import qualified LPEIsDet
-- import qualified LPEDeterminize
-- import qualified LPEAngelic
-- import qualified LPEUGuards
import qualified LPE2MCRL2
import qualified LPEStepper
import qualified LPEStepper2
import qualified LPEInfo
import           LPEPrettyPrint
import           LPEConversion
import           LPEValidity
import           ConcatEither

lpeOpsVersion :: String
lpeOpsVersion = "2019.07.05.02"

data LPEOp = LPEOpLoopInf | LPEOpLoop Int | LPEOp LPEOperation

insertAtToken :: String -> String -> String
insertAtToken _ [] = []
insertAtToken [] list = list
insertAtToken insertion list = search [] list
  where
    search :: String -> String -> String
    search beenHere [] = beenHere
    search beenHere [x] = beenHere ++ [x]
    search beenHere ('%':'i':xs) = search (beenHere ++ insertion) xs
    search beenHere (x:xs) = search (beenHere ++ [x]) xs
-- insertAtToken

-- Core method that does the following:
--  1. Transforms a closed process expression to an LPE;
--  2. Applies the given operations to the LPE, which results in a new LPE;
--  3. Transforms the new LPE to a new model (with the specified name) and process.
lpeOperations :: [LPEOp] -> TxsDefs.ModelId -> String -> TxsDefs.VExpr -> IOC.IOC (Either [String] TxsDefs.ModelId)
lpeOperations operations modelId out invariant = do
    msgsOrLpe <- model2lpe modelId
    case msgsOrLpe of
      Left msgs -> return (Left msgs)
      Right lpe -> do msgsOrNewLpe <- lpeOperation operations 1 operations [lpe, lpe] out invariant
                      case msgsOrNewLpe of
                        Left msgs -> return (Left msgs)
                        Right [] -> return (Left ["No output LPE found!"]) -- Should not happen.
                        Right (newLpe:_) -> do if newLpe /= lpe
                                               then IOC.putMsgs [ EnvData.TXS_CORE_ANY "( LPE has been rewritten! )" ]
                                               else IOC.putMsgs [ EnvData.TXS_CORE_ANY "( LPE is identical to input. )" ]
                                               temp <- lpe2model (newLpe { lpeName = Text.pack (insertAtToken "" out) })
                                               return (Right temp)
-- lpeOperations

lpeOperation :: [LPEOp] -> Int -> [LPEOp] -> [LPE] -> String -> TxsDefs.VExpr -> IOC.IOC (Either [String] [LPE])
lpeOperation _ops _ _ [] _out _invariant = return (Left ["No input LPE found!"])
lpeOperation _ops _ [] lpeInstances _out _invariant = return (Right lpeInstances)
lpeOperation ops i (LPEOpLoop n:xs) (lpe:ys) out invariant =
    if (i >= n) || (lpe `elem` ys)
    then lpeOperation ops i xs (lpe:ys) out invariant
    else do IOC.putMsgs [ EnvData.TXS_CORE_ANY ("<<loop*" ++ show (n - i - 1) ++ ">>") ]
            lpeOperation ops (i + 1) ops (lpe:lpe:ys) out invariant
lpeOperation ops i (LPEOpLoopInf:xs) (lpe:ys) out invariant =
    if lpe `elem` ys
    then lpeOperation ops i xs (lpe:ys) out invariant
    else do IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<loop>>" ]
            lpeOperation ops (i + 1) ops (lpe:lpe:ys) out invariant
lpeOperation ops i (LPEOp op:xs) (lpe:ys) out invariant = do
    msgsOrNewLpe <- op lpe (insertAtToken (show i) out) invariant
    case msgsOrNewLpe of
      Left msgs -> return (Left msgs)
      Right newLpe -> let problems = validateLPEModel newLpe in
                        if null problems
                        then lpeOperation ops i xs (newLpe:ys) out invariant
                        else do IOC.putMsgs [ EnvData.TXS_CORE_ANY ("lpe = " ++ showLPE newLpe) ]
                                return (Left problems)
-- lpeOperation

discardLPE :: LPEOperation
discardLPE _ _ _ = return (Left ["LPE discarded!"])

printLPE :: LPEOperation
printLPE lpe _out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<show>>" ]
    IOC.putMsgs [ EnvData.TXS_CORE_ANY (showLPE lpe) ]
    return (Right lpe)
-- printLPE

printAbbrevLPE :: LPEOperation
printAbbrevLPE lpe _out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<show*>>" ]
    IOC.putMsgs [ EnvData.TXS_CORE_ANY (showAbbrevLPE lpe) ]
    return (Right lpe)
-- printAbbrevLPE

exportLPE :: LPEOperation
exportLPE lpe out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<export>>" ]
    let filename = out ++ ".txs"
    MonadState.liftIO $ writeFile filename (showLPE lpe)
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("LPE exported to " ++ filename ++ "!") ]
    return (Right lpe)
-- exportLPE

exportAbbrevLPE :: LPEOperation
exportAbbrevLPE lpe out _invariant = do
    IOC.putMsgs [ EnvData.TXS_CORE_ANY "<<export*>>" ]
    let filename = out ++ ".txs"
    MonadState.liftIO $ writeFile filename (showAbbrevLPE lpe)
    IOC.putMsgs [ EnvData.TXS_CORE_ANY ("LPE exported to " ++ filename ++ "!") ]
    return (Right lpe)
-- exportAbbrevLPE

getLPEOperation :: String -> Either String LPEOps.LPEOp
getLPEOperation opName = case opName of
                           "stop" -> Right (LPEOps.LPEOp LPEOps.discardLPE)
                           "valid" -> Right (LPEOps.LPEOp validateLPE)
                           "show" -> Right (LPEOps.LPEOp LPEOps.printLPE)
                           "show*" -> Right (LPEOps.LPEOp LPEOps.printAbbrevLPE)
                           "export" -> Right (LPEOps.LPEOp LPEOps.exportLPE)
                           "export*" -> Right (LPEOps.LPEOp LPEOps.exportAbbrevLPE)
                           "loop" -> Right LPEOps.LPEOpLoopInf
                           'l':'o':'o':'p':'*':xs -> case Read.readMaybe xs of
                                                       Just n -> Right (LPEOps.LPEOpLoop n)
                                                       Nothing -> Left ("Invalid operand in LPE operation (" ++ xs ++ ")!")
                           "clean" -> Right (LPEOps.LPEOp LPEClean.cleanLPE)
                           "cstelm" -> Right (LPEOps.LPEOp LPEConstElm.constElm)
                           "parelm" -> Right (LPEOps.LPEOp LPEParElm.parElm)
                           -- "istepelm" -> Right (LPEOps.LPEOp LPEIStepElm.iStepElm)
                           "datareset" -> Right (LPEOps.LPEOp LPEDataReset.dataReset)
                           "parreset" -> Right (LPEOps.LPEOp LPEParReset.parReset)
                           "isdet" -> Right (LPEOps.LPEOp LPEIsDet.isDeterministicLPE)
                           -- "det" -> Right (LPEOps.LPEOp LPEDeterminize.determinizeLPE)
                           -- "angelic" -> Right (LPEOps.LPEOp LPEAngelic.makeInputEnabledLPE)
                           -- "uguard" -> Right (LPEOps.LPEOp LPEUGuards.addUGuardsToLPE)
                           "confelm" -> Right (LPEOps.LPEOp LPEConfCheck.confElm)
                           "mcrl2" -> Right (LPEOps.LPEOp LPE2MCRL2.lpe2mcrl2)
                           's':'t':'e':'p':'*':xs -> case Read.readMaybe xs of
                                                       Just n -> Right (LPEOps.LPEOp (LPEStepper.stepLPE n))
                                                       Nothing -> Left ("Invalid operand in LPE operation (" ++ xs ++ ")!")
                           's':'t':'e':'p':'2':'*':xs -> case Read.readMaybe xs of
                                                           Just n -> Right (LPEOps.LPEOp (LPEStepper2.stepLPE n))
                                                           Nothing -> Left ("Invalid operand in LPE operation (" ++ xs ++ ")!")
                           "info" -> Right (LPEOps.LPEOp LPEInfo.lpeInfo)
                           _ -> Left ("Unknown LPE operation (" ++ opName ++ ")!")
-- getLPEOperation

getLPEOperations :: String -> Either [String] [LPEOps.LPEOp]
getLPEOperations opChain =
    concatEither (map getLPEOperation (filter (\opName -> opName /= []) (splitByArrow opChain)))
  where
    splitByArrow :: String -> [String]
    splitByArrow [] = [[]]
    splitByArrow [x] = [[x]]
    splitByArrow ('-':'>':xs) = []:splitByArrow xs
    splitByArrow (x:xs) =
        case splitByArrow xs of
          [] -> [[x]] -- Should not happen.
          (y:ys) -> (x:y):ys
    -- splitByArrow
-- getLPEOperations

