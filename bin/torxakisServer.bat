REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

@echo off
txsserver.exe %PORTNR% > .txs.%PORTNR%.log 2> .txs.%PORTNR%.err.log
exit