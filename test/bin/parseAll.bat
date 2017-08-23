@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@echo off
echo exit > parseAll.tmp
for /f "usebackq" %%A in (`dir /S /B ^| findstr /E "\.txs"`) DO CALL :Body %%A
del parseAll.tmp
REM Don't "fall through" to :Body.
GOTO :EOF

:Body
echo %1
call torxakis.bat %1  < parseAll.tmp
TIMEOUT /T 3 /NOBREAK > nul
GOTO :EOF