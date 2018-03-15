-- | 

module TorXakis.Compiler.SortId where

import           SortId
    
import           TorXakis.Parser.Data

-- | Construct a list of sort ID's from a list of ADT declarations.
--
-- The ID of the ADT will coincide with the ID of the SortId.
compileToSortId :: [ADTDecl] -> [SortId]
compileToSortId = undefined

