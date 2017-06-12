@ECHO off

REM
REM Store original PATH and call location
REM
SET PATHBACKUP=%PATH%
SET CALLLOC=%cd%

REM
REM Store PATH to relevant programs
REM
FOR /F "tokens=* USEBACKQ" %%F IN (`where timeout.exe`) DO (
SET PATHSYSTEM=%%~dpF
)

SET PATHSCRIPT=%~dp0..\..\bin

FOR /F "tokens=* USEBACKQ" %%F IN (`stack.exe path --local-install-root`) DO (
SET PATHBIN=%%F\bin
)

FOR /F "tokens=* USEBACKQ" %%F IN (`where cvc4-*.exe`) DO (
SET PATHCVC4=%%~dpF
)

FOR /F "tokens=* USEBACKQ" %%F IN (`where z3.exe`) DO (
SET PATHZ3=%%~dpF
)

FOR /F "tokens=* USEBACKQ" %%F IN (`where stack.exe`) DO (
SET PATHSTACK=%%~dpF
)

FOR /F "tokens=* USEBACKQ" %%F IN (`where java.exe`) DO (
SET PATHJAVA=%%~dpF
)


REM
REM move to torxakis directory
REM
cd %~dp0\..\..

REM
REM Clean project
REM
echo --- Start Clean
stack clean
del %PATHBIN%\*.exe
del sys\BuildInfo.hs
cd test
make clean
echo --- End Clean

REM
REM test Copyright
REM
echo --- Start TestCopyright
make testCopyright
echo --- End TestCopyright

REM
REM Make Test Infrastructure (SUTs)
REM
echo --- Start Test Java
make test
cd ..
echo --- End Test Java

REM
REM Build TorXakis
REM
echo --- Start TorXakis Build
stack build
dir %PATHBIN%
echo --- End TorXakis Build

REM
REM Run Self Tests
REM
echo --- Start Self Test
set PATH=%PATHSTACK%;%PATHCVC4%;%PATHZ3%
echo %PATH%
stack test
echo --- End Self Test

REM
REM Run TorXakis Tests
REM
echo --- Start TestExamps
set PATH=%PATHSYSTEM%;%PATHSCRIPT%;%PATHBIN%;%PATHCVC4%;%PATHZ3%;%PATHJAVA%
echo %PATH%
call test\bin\TestExamps.bat %cd%
echo --- End TestExamps

REM
REM return to original PATH and call location
REM
SET PATH=%PATHBACKUP%
cd %CALLLOC%