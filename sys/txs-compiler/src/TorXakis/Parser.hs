{-# LANGUAGE TemplateHaskell #-}
module TorXakis.Parser
    ( ParsedDefs
    , adts
    , funcs
    , consts
    , models
    , txsP
    , parseFile
    , parse
    )
where

import           Control.Arrow             (left)
import           Control.Lens              (Lens', to, (%~), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Identity    (runIdentity)
import qualified Data.Text                 as T
import           Text.Parsec               (ParseError, eof, errorPos, many,
                                            runParserT, sourceColumn,
                                            sourceLine, (<|>))

import           TorXakis.Compiler.Error   (Error (Error), ErrorLoc (ErrorLoc),
                                            ErrorType (ParseError), errorColumn,
                                            errorLine, errorLoc, errorMsg,
                                            errorType)
import           TorXakis.Parser.Common    (TxsParser, txsWhitespace)
import           TorXakis.Parser.ConstDecl (constDeclsP)
import           TorXakis.Parser.Data
import           TorXakis.Parser.FuncDefs  (fdeclP)
import           TorXakis.Parser.ModelDecl
import           TorXakis.Parser.TypeDefs  (adtP)

-- | TorXakis definitions generated by the parser.
data ParsedDefs = ParsedDefs
    { _adts   :: [ADTDecl]
    , _funcs  :: [FuncDecl]
    , _consts :: [FuncDecl]
    , _models :: [ModelDecl]
    } deriving (Eq, Show)
makeLenses ''ParsedDefs

parse :: String -> Either Error ParsedDefs
parse = undefined

parseFile :: FilePath -> IO (Either Error ParsedDefs)
parseFile fp = left parseErrorAsError <$> do
    input <- readFile fp
    return $ runIdentity (runParserT txsP (mkState 1000) fp input)
    where
      parseErrorAsError :: ParseError -> Error
      parseErrorAsError err = Error
          { errorType = ParseError
          , errorLoc = ErrorLoc
              { errorLine   = sourceLine (errorPos err)
              , errorColumn = sourceColumn (errorPos err)
              }
          , errorMsg = T.pack (show err)
          }

-- | TorXakis top-level definitions
data TLDef = TLADT ADTDecl
           | TLFunc FuncDecl -- TODO: make this a list of '[FuncDecl]', and rename to 'TLFuncs'
           | TLConsts [FuncDecl]
           | TLModel ModelDecl

-- | Group a list of top-level definitions per-type.
asParsedDefs :: [TLDef] -> ParsedDefs
asParsedDefs = foldr sep empty
    where
      sep (TLADT a)     = adts %~ (a:)
      sep (TLFunc f)    = funcs %~ (f:)
      sep (TLConsts cs) = consts %~ (cs++)
      sep (TLModel m)   = models %~ (m:)
      empty = ParsedDefs [] [] [] []

-- | Root parser for the TorXakis language.
txsP :: TxsParser ParsedDefs
txsP = do
    txsWhitespace
    ts <- many $  fmap TLADT    adtP
              <|> fmap TLFunc   fdeclP
              <|> fmap TLConsts constDeclsP
              <|> fmap TLModel  modelDeclP
    eof
    return $ asParsedDefs ts
