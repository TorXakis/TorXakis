@ECHO OFF
SET TORXAKIS=torxakis.bat
SET TORXAKISPORT=torxakisport.bat
SET REL=examps\echo
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start Echo Stepper Test
call %TORXAKIS% %EXAMPS%\Echo.txs < %TEST%\Echo_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Echo.Sut.Echo_Step.log
echo ------- End Echo Stepper Test

echo ------- Start Echo Simulator Test
start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Echo.txs < %TEST%\Echo_Sim.txscmd
call %TORXAKIS% %EXAMPS%\Echo.txs < %TEST%\Echo_Test.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Echo.Sut.Echo_Sim.log
echo ------- End Echo Simulator Test
