@ECHO OFF
setlocal 
set CALLLOC=%cd%

echo ----- Start StimulusResponse Tests
cd %1\test\examps\stimulusresponse
call TestStimulusResponse.bat %1
echo ----- End StimulusResponse Tests

echo ----- Start CustomersOrders Tests
cd %1\test\examps\CustomersOrders
call TestCustomersOrders.bat %1
echo ----- End CustomersOrders Tests
TIMEOUT /T 10 /NOBREAK

echo ----- Start LuckyPeople Tests
cd %1\test\examps\LuckyPeople
call TestLuckyPeople.bat %1
echo ----- End LuckyPeople Tests

echo ----- Start Adder Tests
cd %1\test\examps\Adder
call TestAdder.bat %1
echo ----- End Adder Tests

echo ----- Start DispatchProcess Tests
cd %1\test\examps\DispatchProcess
call TestDispatchProcess.bat %1
echo ----- End DispatchProcess Tests

echo ----- Start ControlLoop Tests
cd %1\test\examps\ControlLoop
call TestControlLoop.bat %1
echo ----- End ControlLoop Tests

echo ----- Start Echo Tests
cd %1\test\examps\Echo
call TestEcho.bat %1
echo ----- End Echo Tests

echo ----- Start Moving Arms Tests
cd %1\test\examps\MovingArms
call TestMovingArms.bat %1
echo ----- End Moving Arms Tests

echo ----- Start Point Tests
cd %1\test\examps\Point
call TestPoint.bat %1
echo ----- End Point Tests

echo ----- Start Queue Tests
cd %1\test\examps\Queue
call TestQueue.bat %1
echo ----- End Queue Tests

echo ----- Start ReadWriteConflict Tests
cd %1\test\examps\ReadWriteConflict
call TestReadWriteConflict.bat %1
echo ----- End ReadWriteConflict Tests

cd %CALLLOC%
endlocal