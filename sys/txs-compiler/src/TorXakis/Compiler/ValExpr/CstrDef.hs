{-# LANGUAGE FlexibleContexts  #-}
module TorXakis.Compiler.ValExpr.CstrDef where

import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import           SortId (SortId)
import           CstrId (CstrId)
import           CstrDef (CstrDef (CstrDef))

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.ValExpr.FuncId
    
compileToCstrDefs :: ( MapsTo Text SortId mm
                     , HasCstrIds e )
                  => mm -> e -> [ADTDecl] -> CompilerM (Map CstrId CstrDef)
compileToCstrDefs mm e ds = 
    Map.fromList . concat <$> traverse (adtToCstrDefs mm e) ds

adtToCstrDefs :: ( MapsTo Text SortId mm
                 , HasCstrIds e )
               => mm -> e -> ADTDecl -> CompilerM [(CstrId, CstrDef)]
adtToCstrDefs mm e a =
    traverse (cstrToCstrDefs mm e) (constructors a)

cstrToCstrDefs :: (MapsTo Text SortId mm
                  , HasCstrIds e )
               => mm -> e -> CstrDecl -> CompilerM (CstrId, CstrDef)
cstrToCstrDefs mm e c = do
    cId <- findCstrIdM e (getLoc c)
    isCstrFid <- cstrToIsCstrFuncId cId
    cstrAccFids <- traverse (cstrToAccFuncId mm cId) (cstrFields c)
    return (cId, CstrDef isCstrFid cstrAccFids)
