-- | 

module TorXakis.Compiler.CstrId where

import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Id (Id (Id))
import           SortId (SortId)
import           CstrId (CstrId (CstrId))

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.SortId

compileToCstrId :: Env -> [ADTDecl] -> CompilerM (Map Int CstrId)
compileToCstrId e ds = Map.fromList . concat <$>
    traverse (adtToCstrId e) ds

adtToCstrId :: Env
            -> ADTDecl
            -> CompilerM [(Int, CstrId)]
adtToCstrId e a = do
    sId <- findSortM e (nodeName a)
    traverse (cstrToCstrId e sId) (child a)

cstrToCstrId :: Env
             -> SortId -- ^ SortId of the containing ADT.
             -> CstrDecl
             -> CompilerM (Int, CstrId)
cstrToCstrId e sId c = do
    i <- getNextId
    aSids <- traverse (fieldSort e) (child c)
    -- TODO: here we might confuse i with the parser uid. We need to change this!
    return (uid . nodeMdata $ c, CstrId (nodeName c) (Id i) aSids sId)
