### Testing with predetermined input
We can also tell TorXakis to use predetermined input data for testing. For this, we can make use of [Test Purpose](TestPurpose)s. We'll define a process that communicates Person data and resulting boolean at each step, then we'll add as many steps as we want for our predetermined input, and then we'll use this process in our Test Purpose.

Let's create a _PurposeExamples.txs_ file and define that process and write some test data with expected results:
```
PROCDEF examples [ In :: Person; Out :: Bool ] () HIT ::=
        In ! Person( Male, "Mickey", "Mouse", 13, 1 )
    >-> Out ! True
    >-> In ! Person( Male, "Donald", "Duck", 13, 3 )
    >-> Out ! True
    >-> In ! Person( Male, "Luuk", "Laar", 24, 12 )
    >-> Out ! True
    >-> In ! Person( Female, "Shakira", "Ripoll", 2, 2 )
    >-> Out ! True
    >-> In ! Person( Male, "Michael", "Buble", 9, 9 )
    >-> Out ! True
    >-> In ! Person( Female, "Imke", "Laar", 7, 7 )
    >-> Out ! True
    >-> In ! Person( Male, "Huey", "Duck", 17, 10 )
    >-> Out ! False
    >-> In ! Person( Male, "Dewey", "Duck", 17, 10 )
    >-> Out ! True
    >-> In ! Person( Male, "Louie", "Duck", 17, 10 )
    >-> Out ! False
    >-> In ! Person( Male, "Mickey", "Mouse", 13, 1 )
    >-> Out ! True
    >-> In ! Person( Male, "Donald", "Duck", 13, 3 )
    >-> Out ! True
    >-> In ! Person( Female, "April", "Duck", 15, 5 )
    >-> Out ! True
    >-> In ! Person( Female, "Beatrix", "Oranje", 31, 1 )
    >-> Out ! False
    >-> In ! Person( Female, "Maxima", "Zorreguieta", 17, 5 )
    >-> Out ! False
    >-> In ! Person( Female, "Amalia", "Oranje", 7, 12 )
    >-> Out ! False
    >-> In ! Person( Female, "Alexia", "Oranje", 26, 6 )
    >-> Out ! False
    >-> In ! Person( Female, "Ariane", "Oranje", 10, 4 )
    >-> Out ! False
    >-> In ! Person( Male, "Willem", "Oranje", 27, 4 )
    >-> Out ! True
ENDDEF
```
Mind the **HIT** keyword on first line. This keyword is necessary in order to use it as a **Goal** in the Test Purpose definition below.

Now we should make sure the generated input for our SUT is synchronized with what this process communicates to the channels. Let's define the Test Purpose for this:
```
PURPDEF PurposeExamples ::=
    CHAN IN    In 
    CHAN OUT   Out
    
    GOAL examples ::= examples [In,Out] ()
ENDDEF
```
Defining the Goal of the Test Purpose with *examples* process effectively forces the Persons in *examples* process to be communicated through *In* channel to *SUT* and expects the output in *Out* channel to match whatever is defined in *examples* process.

Now we can use this Test Purpose to test SUT with our predetermined inputs.

1.  Start the SUT: run the [Java program](Java_program) in a command window.

`$> java LuckyPeople`

2.  Start TorXakis: run the [TorXakis](TorXakis) with the LuckyPeople model and PurposeExamples test purpose in another command window.

`$> torxakis LuckyPeople.txs PurposeExamples.txs`

3.  Set the Model, Test Purpose and SUT for testing: In TorXakis type the following commands:

`tester Model PurposeExamples Sut`

4.  Test the SUT with predetermined inputs. We have 18 persons, which means we need 36 steps. But TorXakis occasionally adds some Quiescence steps in between, so let's run TorXakis for 50 steps instead. In TorXakis type the following command:

`test 50`

```
TXS >>  .....1: IN: Act { { ( In, [ Person(Male,"Mickey","Mouse",13,1) ] ) } }
TXS >>  .....2: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  .....3: OUT: No Output (Quiescence)
TXS >>  .....4: IN: Act { { ( In, [ Person(Male,"Donald","Duck",13,3) ] ) } }
TXS >>  .....5: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  .....6: OUT: No Output (Quiescence)
TXS >>  .....7: IN: Act { { ( In, [ Person(Male,"Luuk","Laar",24,12) ] ) } }
TXS >>  .....8: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  .....9: OUT: No Output (Quiescence)
TXS >>  ....10: IN: Act { { ( In, [ Person(Female,"Shakira","Ripoll",2,2) ] ) } }
TXS >>  ....11: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....12: OUT: No Output (Quiescence)
TXS >>  ....13: IN: Act { { ( In, [ Person(Male,"Michael","Buble",9,9) ] ) } }
TXS >>  ....14: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....15: IN: Act { { ( In, [ Person(Female,"Imke","Laar",7,7) ] ) } }
TXS >>  ....16: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....17: OUT: No Output (Quiescence)
TXS >>  ....18: IN: Act { { ( In, [ Person(Male,"Huey","Duck",17,10) ] ) } }
TXS >>  ....19: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....20: OUT: No Output (Quiescence)
TXS >>  ....21: IN: Act { { ( In, [ Person(Male,"Dewey","Duck",17,10) ] ) } }
TXS >>  ....22: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....23: IN: Act { { ( In, [ Person(Male,"Louie","Duck",17,10) ] ) } }
TXS >>  ....24: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....25: OUT: No Output (Quiescence)
TXS >>  ....26: IN: Act { { ( In, [ Person(Male,"Mickey","Mouse",13,1) ] ) } }
TXS >>  ....27: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....28: OUT: No Output (Quiescence)
TXS >>  ....29: IN: Act { { ( In, [ Person(Male,"Donald","Duck",13,3) ] ) } }
TXS >>  ....30: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....31: IN: Act { { ( In, [ Person(Female,"April","Duck",15,5) ] ) } }
TXS >>  ....32: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....33: IN: Act { { ( In, [ Person(Female,"Beatrix","Oranje",31,1) ] ) } }
TXS >>  ....34: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....35: OUT: No Output (Quiescence)
TXS >>  ....36: IN: Act { { ( In, [ Person(Female,"Maxima","Zorreguieta",17,5) ] ) } }
TXS >>  ....37: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....38: OUT: No Output (Quiescence)
TXS >>  ....39: IN: Act { { ( In, [ Person(Female,"Amalia","Oranje",7,12) ] ) } }
TXS >>  ....40: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....41: OUT: No Output (Quiescence)
TXS >>  ....42: IN: Act { { ( In, [ Person(Female,"Alexia","Oranje",26,6) ] ) } }
TXS >>  ....43: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....44: IN: Act { { ( In, [ Person(Female,"Ariane","Oranje",10,4) ] ) } }
TXS >>  ....45: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....46: IN: Act { { ( In, [ Person(Male,"Willem","Oranje",27,4) ] ) } }
TXS >>  ....47: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....48: OUT: No Output (Quiescence)
TXS >>  Goal examples: Hit
TXS >>  PASS
```

### Next: [Lucky based on gender](Modelling-Example-Lucky-People-(Lucky-Based-On-Gender))
