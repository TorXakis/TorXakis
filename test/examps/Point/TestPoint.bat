@ECHO OFF
SET TORXAKIS=torxakis.bat
SET TORXAKISPORT=torxakisport.bat
SET REL=examps\Point
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start Point Stepper Test
call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Step.log
echo ------- End Point Stepper Test

REM echo ------- Start Point Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Point.txs < %TEST%\Point_Sim.txscmd
REM call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Test.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Sim.log
REM echo ------- End Point Simulator Test

echo ------- Start Point_Purpose Stepper Test
call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Purpose_Step.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Purpose_Step.log
echo ------- End Point_Purpose Stepper Test

REM echo ------- Start Point Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Point.txs < %TEST%\Point_Purpose_Sim.txscmd
REM call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Purpose_Test.txscmd
REM TIMEOUT /T 4 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Purpose_Sim.log
REM echo ------- End Point Simulator Test
