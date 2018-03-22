-- |

module TorXakis.Parser.ValExprDecl where

import           Text.Parsec            ((<|>))

import           TorXakis.Parser.Common
import           TorXakis.Parser.Data

valExpP :: TxsParser ExpDecl
valExpP =  mkVarExp <$> lcIdentifier <*> getMetadata
       <|> mkBoolConstExp <$> txsBoolP <*> getMetadata
       <|> mkIntConstExp <$> txsIntP <*> getMetadata
       <|> mkStringConstExp <$> txsStringP <*> getMetadata

-- letExpP :: TxsParser ExpDecl
-- letExpP = do
--     txsSymbol "LET"
--     letVarDeclsP
--     txsSymbol "IN"
--     valExpP
--     txsSymbol "NI"

-- letVarDeclsP :: TxsParser LetValDecl
-- letVarDeclsP = undefined

txsBoolP :: TxsParser Bool
txsBoolP =  (txsSymbol "True" >> return True)
        <|> (txsSymbol "False" >> return False)

