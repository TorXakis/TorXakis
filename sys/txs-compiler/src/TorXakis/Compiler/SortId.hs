-- | 

module TorXakis.Compiler.SortId where

import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.Error.Class (liftEither)

import           Id (Id (Id))
import           SortId (SortId (SortId))
    
import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error

-- | Construct a list of sort ID's from a list of ADT declarations.
--
-- The ID of the ADT will coincide with the ID of the SortId.
compileToSortId :: [ADTDecl] -> CompilerM (Map Text SortId)
compileToSortId ds = Map.fromList . zip (toText . nodeName <$> ds) <$>
    traverse adtDeclToSortId ds

adtDeclToSortId :: ADTDecl -> CompilerM SortId
adtDeclToSortId a = do 
    i <- getNextId
    return $ SortId (toText . nodeName $ a) (Id i)

sortIdOfFieldDecl :: HasSortIds e => e -> FieldDecl -> Either Error SortId
sortIdOfFieldDecl e f = findSortId e (toText . nodeName . child $ f)

sortIdOfFieldDeclM :: HasSortIds e => e -> FieldDecl -> CompilerM SortId
sortIdOfFieldDeclM e f = liftEither $ sortIdOfFieldDecl e f
