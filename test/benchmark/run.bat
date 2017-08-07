REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

@ECHO OFF
call torxakisPort 4567 benchmark.txs < run.cmd
more benchmarkresult.log