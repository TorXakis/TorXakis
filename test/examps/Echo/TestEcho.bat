REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET TORXAKISPORT=torxakisport.bat
SET REL=examps\Echo
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start Echo Stepper Test
call %TORXAKIS% %EXAMPS%\Echo.txs < %TEST%\Echo_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Echo.Sut.Echo_Stepper.log
echo ------- End Echo Stepper Test

echo ------- Start Echo Simulator Test
start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Echo.txs < %TEST%\Echo_Simulator.txscmd
call %TORXAKIS% %EXAMPS%\Echo.txs < %TEST%\Echo_Tester.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Echo.Sut.Echo_Simulator.log
echo ------- End Echo Simulator Test
