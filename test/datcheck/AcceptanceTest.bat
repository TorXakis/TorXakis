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
echo %DATE%%TIME%
echo --- Start Clean
stack clean
del %PATHBIN%\*.exe
cd test
make clean
echo --- End Clean
echo %DATE%%TIME%

REM
REM test Copyright
REM
echo %DATE%%TIME%
echo --- Start TestCopyright
make testCopyright
echo --- End TestCopyright
echo %DATE%%TIME%

REM
REM Make Test Infrastructure (SUTs)
REM
echo --- Start Test Java
echo %DATE%%TIME%
make test
cd ..
echo --- End Test Java
echo %DATE%%TIME%

REM
REM Build TorXakis
REM
echo %DATE%%TIME%
echo --- Start TorXakis Install
stack install --test --fast
dir %PATHBIN%
echo --- End TorXakis Install
echo %DATE%%TIME%

REM
REM Run Self Tests
REM
echo --- Start Self Test
echo %DATE%%TIME%
set PATH=%PATHSTACK%;%PATHCVC4%;%PATHZ3%
echo %PATH%
REM stack test
echo --- End Self Test
echo %DATE%%TIME%

REM
REM Run TorXakis Tests
REM
echo %DATE%%TIME%
echo --- Start TestExamps
set PATH=%PATHSYSTEM%;%PATHSCRIPT%;%PATHBIN%;%PATHCVC4%;%PATHZ3%;%PATHJAVA%
echo %PATH%
call test\bin\TestExamps.bat %cd%
echo --- End TestExamps
echo %DATE%%TIME%

REM
REM return to original PATH and call location
REM
SET PATH=%PATHBACKUP%
cd %CALLLOC%
