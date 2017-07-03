@echo off
echo exit > parseAll.tmp
for /f "usebackq" %%A in (`dir /S /B ^| findstr /E "\.txs"`) DO CALL :Body %%A
del parseAll.tmp
REM Don't "fall through" to :Body.
GOTO :EOF

:Body
REM echo %1
call torxakis.bat %1  < parseAll.tmp
TIMEOUT /T 10 /NOBREAK > nul
GOTO :EOF
