@echo off
txsserver.exe %PORTNR% > .txs.%PORTNR%.log 2> .txs.%PORTNR%.err.log
exit