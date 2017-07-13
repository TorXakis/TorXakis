@ECHO OFF
setlocal 
set CALLLOC=%cd%

echo ----- Start StimulusResponse Tests
echo %DATE%%TIME%
cd %1\test\examps\stimulusresponse
call TestStimulusResponse.bat %1
echo ----- End StimulusResponse Tests
echo %DATE%%TIME%

echo ----- Start CustomersOrders Tests
echo %DATE%%TIME%
cd %1\test\examps\CustomersOrders
call TestCustomersOrders.bat %1
echo ----- End CustomersOrders Tests
echo %DATE%%TIME%
TIMEOUT /T 10 /NOBREAK

echo ----- Start LuckyPeople Tests
echo %DATE%%TIME%
cd %1\test\examps\LuckyPeople
call TestLuckyPeople.bat %1
echo ----- End LuckyPeople Tests
echo %DATE%%TIME%

echo ----- Start Adder Tests
echo %DATE%%TIME%
cd %1\test\examps\Adder
call TestAdder.bat %1
echo ----- End Adder Tests
echo %DATE%%TIME%

echo ----- Start DispatchProcess Tests
echo %DATE%%TIME%
cd %1\test\examps\DispatchProcess
call TestDispatchProcess.bat %1
echo ----- End DispatchProcess Tests
echo %DATE%%TIME%

echo ----- Start ControlLoop Tests
echo %DATE%%TIME%
cd %1\test\examps\ControlLoop
call TestControlLoop.bat %1
echo ----- End ControlLoop Tests
echo %DATE%%TIME%

echo ----- Start Echo Tests
echo %DATE%%TIME%
cd %1\test\examps\Echo
call TestEcho.bat %1
echo ----- End Echo Tests
echo %DATE%%TIME%

echo ----- Start Moving Arms Tests
echo %DATE%%TIME%
cd %1\test\examps\MovingArms
call TestMovingArms.bat %1
echo ----- End Moving Arms Tests
echo %DATE%%TIME%

echo ----- Start Point Tests
echo %DATE%%TIME%
cd %1\test\examps\Point
call TestPoint.bat %1
echo ----- End Point Tests
echo %DATE%%TIME%

echo ----- Start Queue Tests
echo %DATE%%TIME%
cd %1\test\examps\Queue
call TestQueue.bat %1
echo ----- End Queue Tests
echo %DATE%%TIME%

echo ----- Start ReadWriteConflict Tests
echo %DATE%%TIME%
cd %1\test\examps\ReadWriteConflict
call TestReadWriteConflict.bat %1
echo ----- End ReadWriteConflict Tests
echo %DATE%%TIME%

cd %CALLLOC%
endlocal