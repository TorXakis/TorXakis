-- | 

module TorXakis.Parser.ValExprDecl where

import           Text.Parsec ( (<|>) )

import           TorXakis.Parser.Data
import           TorXakis.Parser.Common

valExpP :: TxsParser ExpDecl
valExpP =  mkVarExp <$> lcIdentifier <*> getMetadata
       <|> mkBoolConstExp <$> txsBoolP <*> getMetadata
       <|> mkIntConstExp <$> txsIntP <*> getMetadata
       <|> mkStringConstExp <$> txsStringP <*> getMetadata

txsBoolP :: TxsParser Bool
txsBoolP =  (txsSymbol "True" >> return True)
        <|> (txsSymbol "False" >> return False)
                             
