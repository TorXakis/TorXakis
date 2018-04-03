{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lens.Sigs where

import           Lens.Micro (Lens')

import           FuncTable  (FuncTable)
import           Sigs       (Sigs, func)

funcTable :: Lens' (Sigs v) (FuncTable v)
funcTable g sigs = (\func' -> sigs {func = func'})<$> g (func sigs)
