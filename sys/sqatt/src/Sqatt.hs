-- | Integration test utilities.

module Sqatt
  ( TorXakisExampleFiles(..)
  )
where

-- | Files needed to run a TorXakis example.
data TorXakisExampleFiles = TorXakisExampleFiles
  { txsModelFile    :: FilePath       -- ^ Path to the TorXakis model file.
  , txsCommandsFile :: FilePath       -- ^ Path to the file containing the commands
                                      --   that will be passed to the TorXakis server.
  , sutSourceFile   :: Maybe FilePath -- ^ Path to the SUT source code. This
                                      --   code will be compiled and run together with TorXakis. If
                                      --   this record is `Nothing` then the example is assumed to
                                      --   be autonomous (only TorXakis will be run)
  }
