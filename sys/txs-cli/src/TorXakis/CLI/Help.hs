-- | Help text for the TorXakis CLI.
{-# LANGUAGE QuasiQuotes #-}
module TorXakis.CLI.Help where

import           Text.RawString.QQ

helpText :: String
helpText = [r|
--------------------------------
TorXakis :: Model-Based Testing
--------------------------------

quit, q                         : stop TorXakis completely
exit, x                         : exit the current command run of TorXakis
help, h, ?                      : show help (this text)
info, i                         : show info on TorXakis

delay <n>                       : wait for <n> seconds
load <file path>                : load TorXakis model definitions to the session

stepper <model>                 : start stepping with model <model> where <model>
                                  is a model in model definitions which has been
                                  loaded with `load` command
step <action>                   : make a step identified by <action>
step [<n>]                      : make <n>/1 random steps
|]
