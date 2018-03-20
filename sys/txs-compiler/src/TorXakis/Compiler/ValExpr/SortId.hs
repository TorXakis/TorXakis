-- | 

module TorXakis.Compiler.ValExpr.SortId where

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
compileToSortId ds = Map.fromList . zip (adtName <$> ds) <$>
    traverse adtDeclToSortId ds

adtDeclToSortId :: ADTDecl -> CompilerM SortId
adtDeclToSortId a = do 
    i <- getNextId
    return $ SortId (adtName a) (Id i)

sortIdOfFieldDecl :: HasSortIds e => e -> FieldDecl -> Either Error SortId
sortIdOfFieldDecl e = findSortId e . fieldSort

sortIdOfFieldDeclM :: HasSortIds e => e -> FieldDecl -> CompilerM SortId
sortIdOfFieldDeclM e f = liftEither $ sortIdOfFieldDecl e f

sortIdOfVarDecl :: HasSortIds e => e -> VarDecl -> Either Error SortId
sortIdOfVarDecl e = findSortId e . varDeclSort

sortIdOfVarDeclM :: HasSortIds e => e -> VarDecl -> CompilerM SortId
sortIdOfVarDeclM e f = liftEither $ sortIdOfVarDecl e f
