module TorXakis.Compiler.CstrDef where

import           Data.Map (Map)
import qualified Data.Map as Map

import           SortId (SortId)
import           CstrId (CstrId)
import           CstrDef (CstrDef (CstrDef))

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.FuncId
    
compileToCstrDefs :: (HasCstrIds e, HasSortIds e)
                  => e -> [ADTDecl] -> CompilerM (Map CstrId CstrDef)
compileToCstrDefs e ds = 
    Map.fromList . concat <$> traverse (adtToCstrDefs e) ds

adtToCstrDefs :: (HasCstrIds e, HasSortIds e)
               => e -> ADTDecl -> CompilerM [(CstrId, CstrDef)]
adtToCstrDefs e a =
    traverse (cstrToCstrDefs e) (child a)

cstrToCstrDefs :: (HasCstrIds e, HasSortIds e)
               => e -> CstrDecl -> CompilerM (CstrId, CstrDef)
cstrToCstrDefs e c = do
    cId <- findCstrIdM e (getLoc c)
    isCstrFid <- cstrToIsCstrFuncId cId
    cstrAccFids <- traverse (cstrToAccFuncId e cId) (child c)
    return (cId, CstrDef isCstrFid cstrAccFids)
