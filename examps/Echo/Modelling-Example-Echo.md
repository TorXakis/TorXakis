# Modelling Example: Echo
Let's say we want to build a simple system that that receives an input and repeats it as is (AKA echoes it). Before writing any code for it, we're going to model the solution and test it with [TorXakis][7].

The complete model file used in this example can be found at [the example folder](https://github.com/TorXakis/TorXakis/tree/develop/examps/Echo).
## Channels
This system is going to receive an input and echo it via the output.
For simplicity, let's limit the inputs to only be integers. We can [define the channels][2] needed for modelling this behaviour as:
```
CHANDEF  Chans ::=  In, Out :: Int
ENDDEF
```
## Process
We need to define the process of receiving the input and communicating it to the output channel. Here is our [process definition][4]:
```
PROCDEF  proc  [ In, Out :: Int ]  ( ) ::=
        In ? x
    >-> Out ! x
    >-> proc [ In, Out ] ()
ENDDEF
```
## Model
The model should specify that the system uses two channels, one incoming and one outgoing.
It should also specify that first an input and then an output are communicated.
The behaviour of the model should be the process that is defined above.
The [Model Definition][5] of this system in TorXakis is as follows:
```
MODELDEF  Model ::=
        CHAN IN     In
        CHAN OUT    Out

        BEHAVIOUR   proc [ In, Out ] ( )
ENDDEF
```
## Model Based Testing: Stepping through the Model
We don't have any SUT yet, but with TorXakis we can already step through the model to verify it.

1.  Start TorXakis: run [TorXakis][7] with the Echo model described above.

`$> torxakis Echo.txs`

2.  Set the Model for stepping: In TorXakis type the following commands:

`stepper Model`

3.  Step through the model: In TorXakis type the following command:

`step 10`

TorXakis will perform the two communication actions with random integer inputs and display behaviour of the model at every step.
After required number of steps are reached, TorXakis will conclude:
```
TXS >>  .....1: Act { { ( In, [ -59 ] ) } }
TXS >>  .....2: Act { { ( Out, [ -59 ] ) } }
TXS >>  .....3: Act { { ( In, [ -50 ] ) } }
TXS >>  .....4: Act { { ( Out, [ -50 ] ) } }
TXS >>  .....5: Act { { ( In, [ -67 ] ) } }
TXS >>  .....6: Act { { ( Out, [ -67 ] ) } }
TXS >>  .....7: Act { { ( In, [ -7 ] ) } }
TXS >>  .....8: Act { { ( Out, [ -7 ] ) } }
TXS >>  .....9: Act { { ( In, [ -77 ] ) } }
TXS >>  ....10: Act { { ( Out, [ -77 ] ) } }
TXS >>  PASS
```
## Simulator Definition
We still don't have the real implementation of the system, but with TorXakis we don't need it to mimic an actual test with an existing SUT.
TorXakis is capable of simulating the SUT based on the defined model. We just have to [define the connection][6] to be used by the Simulator:
```
CNECTDEF  Sim ::=
        SERVERSOCK

        CHAN IN   In                        HOST "localhost"  PORT 9999
        DECODE    In ! fromString(s)        <-  ? s

        CHAN OUT  Out                       HOST "localhost"  PORT 9999
        ENCODE    Out ? i                   ->  ! toString(i)
ENDDEF
```
Important points:
- Line 2: Since this connection will be used as a simulator it has to be a Server Socket, defined by SERVERSOCK as the connection type ([cnectType][6]).
- Line 4: **"In"** channel of the model is also the actual input of the simulator.
- Line 6: **"Out"** channel of the model is also the actual output of the simulator.

Now we have a (simulated) SUT, so we can connect to it and execute Model Based Testing!
## SUT Connection
TorXakis is going to communicate with the SUT by sending and receiving lines i.e. strings terminated by a line feed, over a socket at port 9999.
When TorXakis and the SUT run on the same machine (localhost) the [SUT connection][6] can be defined in TorXakis as follows:
```
CNECTDEF  Sut ::=
        CLIENTSOCK

        CHAN OUT  In                        HOST "localhost"  PORT 9999
        ENCODE    In ? i                    ->  ! toString(i)

        CHAN IN   Out                       HOST "localhost"  PORT 9999
        DECODE    Out ! fromString(s)       <-  ? s
ENDDEF
```
Important points:
- Line 2: Since this connection will be used to connect to the SUT it has to be a Client Socket, defined by CLIENTSOCK as the connection type ([cnectType][6]).
- Line 4: **"In"** channel of the model should be the _output_ of TorXakis, since this is how TorXakis will communicate test inputs to the SUT.
- Line 6: **"Out"** channel of the model becomes the _input_ of TorXakis.
## Model Based Testing: Testing the Simulated SUT
1.  Start TorXakis: run [TorXakis][7] with the Echo model described above.

`$> torxakis Echo.txs`

2.  Set the Model and Simulator for simulating the SUT: In TorXakis type the following command:

`simulator Model Sim`

TorXakis will start waiting for a connection, in order to start the Simulator.

3.  Start another instance of TorXakis: In another command window, run [TorXakis][7] with the Echo model described above on a different port.

`$> torxakisPort 9877 Echo.txs`

4.  Set the Model and SUT for testing: In TorXakis type the following command:

`tester Model Sut`

As soon as you enter this command, you'll see that Simulator also responds:

`TXS >>  Simulator started`

5.  Simulator works a bit slower than the Tester. To prevent unexpected delays to cause false negatives, we will increase delta times of tester.
In the command window of the tester, input following commands:
```
param param_Sim_deltaTime 5000
param param_Sut_deltaTime 5000
```

6.  Start Simulator with a high number of steps, in order to not run out of steps before testing is finished:

`sim 20`

7.  Test the simulated SUT: In TorXakis type the following command:

`test 10`

TorXakis will test the (simulated) SUT with random integer inputs and display behaviour of the SUT at every step.
After required number of steps are reached, TorXakis will conclude:
```
TXS >>  .....1: IN: Act { { ( In, [ 1 ] ) } }
TXS >>  .....2: OUT: Act { { ( Out, [ 1 ] ) } }
TXS >>  .....3: IN: Act { { ( In, [ 14 ] ) } }
TXS >>  .....4: OUT: Act { { ( Out, [ 14 ] ) } }
TXS >>  .....5: IN: Act { { ( In, [ -82 ] ) } }
TXS >>  .....6: OUT: Act { { ( Out, [ -82 ] ) } }
TXS >>  .....7: IN: Act { { ( In, [ -67 ] ) } }
TXS >>  .....8: OUT: Act { { ( Out, [ -67 ] ) } }
TXS >>  .....9: IN: Act { { ( In, [ -4 ] ) } }
TXS >>  ....10: OUT: Act { { ( Out, [ -4 ] ) } }
TXS >>  PASS
```
And simulator TorXakis instance will be acting as the SUT:
```
TXS >>  .....1: OUT: No Output (Quiescence)
TXS >>  .....2: IN: Act { { ( In, [ 1 ] ) } }
TXS >>  .....3: OUT: Act { { ( Out, [ 1 ] ) } }
TXS >>  .....4: IN: Act { { ( In, [ 14 ] ) } }
TXS >>  .....5: OUT: Act { { ( Out, [ 14 ] ) } }
TXS >>  .....6: OUT: No Output (Quiescence)
TXS >>  .....7: IN: Act { { ( In, [ -82 ] ) } }
TXS >>  .....8: OUT: Act { { ( Out, [ -82 ] ) } }
TXS >>  .....9: OUT: No Output (Quiescence)
TXS >>  ....10: IN: Act { { ( In, [ -67 ] ) } }
TXS >>  ....11: OUT: Act { { ( Out, [ -67 ] ) } }
TXS >>  ....12: OUT: No Output (Quiescence)
TXS >>  ....13: IN: Act { { ( In, [ -4 ] ) } }
TXS >>  ....14: OUT: Act { { ( Out, [ -4 ] ) } }
TXS >>  ....15: OUT: No Output (Quiescence)
TXS >>  ....16: OUT: No Output (Quiescence)
TXS >>  ....17: OUT: No Output (Quiescence)
TXS >>  ....18: OUT: No Output (Quiescence)
TXS >>  ....19: OUT: No Output (Quiescence)
TXS >>  ....20: OUT: No Output (Quiescence)
TXS >>  PASS
```
## XML-Based communication
TorXakis is capable of XML-based communication with SUT's and simulating them as well.
Let's [define another SUT connection][6] and a simulator, both of which use XML-based communication.
```
CNECTDEF  Xut ::=
        CLIENTSOCK

        CHAN OUT  In                        HOST "localhost"  PORT 9999
        ENCODE    In ? i                    ->  ! toXml(i)

        CHAN IN   Out                       HOST "localhost"  PORT 9999
        DECODE    Out ! fromXml(s)          <-  ? s
ENDDEF

CNECTDEF  Xim ::=
        SERVERSOCK

        CHAN IN   In                        HOST "localhost"  PORT 9999
        DECODE    In ! fromXml(s)           <-  ? s

        CHAN OUT  Out                       HOST "localhost"  PORT 9999
        ENCODE    Out ? i                   ->  ! toXml(i)
ENDDEF
```
Now we can simulate and test our proposed system as if it uses XML-Based communication:

1.  Start TorXakis: run [TorXakis][7] with the Echo model described above.

`$> torxakis Echo.txs`

2.  Set the Model and Simulator for simulating the SUT:

`simulator Model Xim`

3.  Start another instance of TorXakis: In another command window, run [TorXakis][7] with the Echo model described above on a different port.

`$> torxakisPort 9877 Echo.txs`

4.  Set the [Model][5] and SUT for testing:

`tester Model Xut`

5.  Remember to increase delta times of tester. In the command window of the tester, input following commands:
```
param param_Sim_deltaTime 5000
param param_Sut_deltaTime 5000
```

6.  Start Simulator with a high number of steps:

`sim 20`

7.  Test the simulated SUT:

`test 10`

Output of Tester [TorXakis][7] instance:
```
TXS >>  .....1: IN: Act { { ( In, [ -78 ] ) } }
TXS >>  .....2: OUT: Act { { ( Out, [ -78 ] ) } }
TXS >>  .....3: IN: Act { { ( In, [ -67 ] ) } }
TXS >>  .....4: OUT: Act { { ( Out, [ -67 ] ) } }
TXS >>  .....5: IN: Act { { ( In, [ -62 ] ) } }
TXS >>  .....6: OUT: Act { { ( Out, [ -62 ] ) } }
TXS >>  .....7: IN: Act { { ( In, [ -52 ] ) } }
TXS >>  .....8: OUT: Act { { ( Out, [ -52 ] ) } }
TXS >>  .....9: IN: Act { { ( In, [ -86 ] ) } }
TXS >>  ....10: OUT: Act { { ( Out, [ -86 ] ) } }
TXS >>  PASS
```

Output of Simulator [TorXakis][7] instance:
```
TXS >>  .....1: OUT: No Output (Quiescence)
TXS >>  .....2: OUT: No Output (Quiescence)
TXS >>  .....3: IN: Act { { ( In, [ -78 ] ) } }
TXS >>  .....4: OUT: Act { { ( Out, [ -78 ] ) } }
TXS >>  .....5: OUT: No Output (Quiescence)
TXS >>  .....6: IN: Act { { ( In, [ -67 ] ) } }
TXS >>  .....7: OUT: Act { { ( Out, [ -67 ] ) } }
TXS >>  .....8: OUT: No Output (Quiescence)
TXS >>  .....9: IN: Act { { ( In, [ -62 ] ) } }
TXS >>  ....10: OUT: Act { { ( Out, [ -62 ] ) } }
TXS >>  ....11: IN: Act { { ( In, [ -52 ] ) } }
TXS >>  ....12: OUT: Act { { ( Out, [ -52 ] ) } }
TXS >>  ....13: IN: Act { { ( In, [ -86 ] ) } }
TXS >>  ....14: OUT: Act { { ( Out, [ -86 ] ) } }
TXS >>  ....15: OUT: No Output (Quiescence)
TXS >>  ....16: OUT: No Output (Quiescence)
TXS >>  ....17: OUT: No Output (Quiescence)
TXS >>  ....18: OUT: No Output (Quiescence)
TXS >>  ....19: OUT: No Output (Quiescence)
TXS >>  ....20: OUT: No Output (Quiescence)
TXS >>  PASS
```
[1]: https://github.com/TorXakis/TorXakis/wiki/TypeDefs
[2]: https://github.com/TorXakis/TorXakis/wiki/ChanDefs
[3]: https://github.com/TorXakis/TorXakis/wiki/FuncDefs
[4]: https://github.com/TorXakis/TorXakis/wiki/ProcDefs
[5]: https://github.com/TorXakis/TorXakis/wiki/ModelDefs
[6]: https://github.com/TorXakis/TorXakis/wiki/CnectDefs
[7]: https://github.com/TorXakis/TorXakis/wiki/TorXakis
