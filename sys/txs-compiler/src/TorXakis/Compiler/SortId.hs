-- | 

module TorXakis.Compiler.SortId where

import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Id (Id (Id))
import           SortId (SortId (SortId))
    
import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data

-- | Construct a list of sort ID's from a list of ADT declarations.
--
-- The ID of the ADT will coincide with the ID of the SortId.
compileToSortId :: [ADTDecl] -> CompilerM (Map Text SortId)
compileToSortId ds = Map.fromList . zip (nodeName <$> ds) <$>
    traverse adtDeclToSortId ds

adtDeclToSortId :: ADTDecl -> CompilerM SortId
adtDeclToSortId a = do 
    i <- getNextId
    return $ SortId (nodeName a) (Id i)

fieldSort :: Env -> FieldDecl -> CompilerM SortId
fieldSort e f = findSortM e (nodeName . child $ f)    
