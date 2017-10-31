# Modelling Example - Lucky People
We want to test our [java program](Java_program) [LuckyPeople.java](https://github.com/TorXakis/TorXakis/blob/develop/examps/LuckyPeople/sut/LuckyPeople.java).
This program gets as input a sequence of persons and determines if those people are lucky. The detailed behaviour of this program can be found at the [assignment page](Modelling-Example-Lucky-People).
All the files used in this example can be found at [the example folder](https://github.com/TorXakis/TorXakis/tree/develop/examps/LuckyPeople).
## Types
We start with defining the types that are needed to model This program's (our System Under Test - SUT) behaviour.
The input of our SUT are persons with first name, last name, sex, day of birth and month of birth. Let's [define the types](TypeDefs) needed:
```
TYPEDEF Sex ::= Male | Female ENDDEF

TYPEDEF Person ::=
    Person { sex :: Sex 
           ; firstName, lastName :: String 
           ; dayOfBirth, monthOfBirth :: Int
           }
ENDDEF
```
## Channels
Our SUT receives a sequence of persons and determines if those people are lucky. Let's [define the channels](ChanDefs) needed for modelling this behaviour:
```
CHANDEF Channels ::=  In   :: Person
                    ; Out  :: Bool
ENDDEF
```
## Procedure
In our [model definition](ModelDefs), we need a way to define the behaviour of determining whether a Person is lucky or not.
Let's implement a [procedure definition](ProcDefs) for this:

`PROCDEF luckyPeople [ In :: Person; Out :: Bool ] ( last :: Sex; n :: Int ) ::=`

"*In*" and "*Out*" are our channels, while "*last*" and "*n*" are the parameters of this procedure. We added the parameters because we want to keep track of how many people of the same sex have been before current person, since it is one of the ways of determining whether a person is lucky.

We want to test our SUT with inputs that make sense, so we should tell TorXakis what makes a Person input valid. Let's add a function as a constraint for the input:

`In ? p [[ isValid_Person(p) ]]`

We're going to implement "*isValid_Person()*" function later. For now let's focus on the process.

Next, we want to communicate to the output whether this person is lucky or not. Let's use another function for it:

`>-> Out ! isLuckyPerson (p, last, n)`

"**>->**" is the [Sequence Operator](Sequence_Operator). It means that after the process on the left has communicated, the described process behaviour on the right is exposed.

After communicating the result to the output channel, it's time to go back and wait for next input. There's one catch, though: As we said above, we should keep track of how many people with the current sex have been processed so far.

```
>->(
            ( [[ sex(p) == last ]] =>> luckyPeople[In,Out] ( sex(p), n+1 ) )
        ##
            ( [[ sex(p) <> last ]] =>> luckyPeople[In,Out] ( sex(p), 1 ) )
    )
```

"**##**" is the [Choice Operator](Choice_Operator). It means that either one of those processes are executed, but never both.

"**=>>**" is the [Guard Operator](Guard_Operator). The process behaviour on the right is executed only if the expression on the left of it is *true*.

So the [process definition](ProcDefs) of Lucky People looks like this:
```
PROCDEF luckyPeople [ In :: Person; Out :: Bool ] ( last :: Sex; n :: Int ) ::=
        In ? p [[ isValid_Person(p) ]] 
    >-> Out ! isLuckyPerson (p, last, n) 
    >->(
                ( [[ sex(p) == last ]] =>> luckyPeople[In,Out] ( sex(p), n+1 ) )
            ##
                ( [[ sex(p) <> last ]] =>> luckyPeople[In,Out] ( sex(p), 1 ) )
        )
ENDDEF
```
Now we can use this [procedure definition](ProcDefs) in our [Model](ModelDefs).
## Model
The model should specify that the system uses two channels, one incoming for the Person and one outgoing for the result.
It should also specify that "*luckyPeople*" process is executed with the input and this process's output is communicated to the out channel as this model's behaviour.
```
MODELDEF Model ::=
    CHAN IN    In
    CHAN OUT   Out

    BEHAVIOUR
        luckyPeople[In, Out](Male,0)   -- first sex choice is arbitrary
ENDDEF
```

## Functions
We're not done yet. We need to implement the [function definitions](FuncDefs) that our "*luckyPeople*" process uses.
### isValid_Person
We want to validate the input in order to make sure that it represents data of a person that can actually exist.
```
FUNCDEF isValid_Person (p :: Person) :: Bool ::=
       strinre (firstName(p), REGEX ('[A-Z][a-z]*'))
    /\ strinre (lastName(p), REGEX ('[A-Z][a-z]*'))
    /\ (1 <= dayOfBirth(p)) /\ (dayOfBirth(p) <= 31)
    /\ (1 <= monthOfBirth(p)) /\ (monthOfBirth(p) <= 12)
ENDDEF
```
"*strinre*" function checks whether the given string matches the given Regular Expression. First two lines ensure that only the inputs with Capital case first name and surname are valid.
### isLuckyPerson
To make the "*luckyPerson*" process cleaner, we decided to extract the logic of determining the person's luckiness into a separate function. As we have seen in the [assignment page](Modelling-Example-Lucky-People), a person can be lucky based on 3 criteria: gender, name or birthday. So our [function definition](FuncDefs) should be something like this:
```
FUNCDEF isLuckyPerson (p :: Person; last :: Sex; n :: Int) :: Bool ::=
       isLuckyByGender(p, last, n)
    \/ isLuckyByName(p)
    \/ isLuckyByBirthday(p)
ENDDEF
```
Now let's define each of these criteria as separate [function definitions](FuncDefs).

### isLuckyByGender
A person is lucky by gender if previous 5 people were all from the opposite sex.
```
FUNCDEF isLuckyByGender (p :: Person; last :: Sex; n :: Int) :: Bool ::=
    ( sex(p) <> last ) /\ ( n >= 5 )
ENDDEF
```

### isLuckyByName
A person is lucky by name if both it's name and surname start with the same letter.
```
FUNCDEF isLuckyByName (p :: Person) :: Bool ::=
    at(firstName(p), 0 ) == at(lastName(p), 0 )
ENDDEF
```

### isLuckyByBirthday
A person is lucky by birthday if both day and month of it's birthday are the same.
```
FUNCDEF isLuckyByBirthday (p :: Person) :: Bool ::=
    dayOfBirth(p) == monthOfBirth(p)
ENDDEF
```

## SUT Connection
Our SUT communicates with the outside world by sending and receiving lines i.e. strings terminated by a line feed, over a socket at port 7777.
The SUT also expects one person per input line, fields separated by "@" sign. So we should make sure that the Person type in our TorXakis model is properly serialized before being communicated to the SUT. The return value from the SUT should also be deserialized into proper data structure (Bool).
Assuming that TorXakis and the SUT run on the same machine (localhost) the [SUT connection](CnectDefs) can be defined in TorXakis as follows:
```
CONSTDEF separator :: String ::= "@" ENDDEF

CNECTDEF  Sut ::=
    CLIENTSOCK

    CHAN  OUT  In                   HOST "localhost"  PORT 7777
    ENCODE     In  ? p              ->  ! toString(sex(p))        ++ separator ++
                                          firstName(p)            ++ separator ++
                                          lastName(p)             ++ separator ++
                                          toString(dayOfBirth(p)) ++ separator ++
                                          toString(monthOfBirth(p))

    CHAN  IN   Out                  HOST "localhost"  PORT 7777
    DECODE     Out  ! fromString(s) <-  ? s
ENDDEF

```
## Model Based Testing
1.  Start the SUT: run the [Java program](Java_program) in a command window.

`$> java LuckyPeople`

2.  Start TorXakis: run the [TorXakis](TorXakis) with the LuckyPeople model described above in another command window.

`$> torxakis LuckyPeople.txs`

3.  Set the Model and SUT for testing: In TorXakis type the following commands:

`tester Model Sut`

4.  Test the SUT: In TorXakis type the following command:

`test 50`

TorXakis will create *valid* random Person data, pass it to SUT and receive the output at each step, as long as the SUT responds as expected or the number of test steps are reached, then finally conclude:
```
TXS >>  .....1: IN: Act { { ( In, [ Person(Male,"W","Phz",22,9) ] ) } }
TXS >>  .....2: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  .....3: IN: Act { { ( In, [ Person(Male,"Pa","Hrd",2,2) ] ) } }
TXS >>  .....4: OUT: Act { { ( Out, [ True ] ) } }
...
TXS >>  ....47: IN: Act { { ( In, [ Person(Female,"Pk","Pq",3,11) ] ) } }
TXS >>  ....48: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....49: IN: Act { { ( In, [ Person(Female,"Pp","La",31,1) ] ) } }
TXS >>  ....50: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  PASS
```

## XML-Based communication
TorXakis is capable of XML-based communication with SUT's. We don't have an SUT which does that but TorXakis can help us there, too: TorXakis can also act as a simulator which behaves based on a model.
Let's [define another connection](CnectDefs) and a simulator, both of which use XML-based communication.
```
CNECTDEF  Xut ::=
    CLIENTSOCK

    CHAN  OUT  In                   HOST "localhost"  PORT 7777
    ENCODE     In  ? p              ->  ! toXml(p)
    
    CHAN  IN   Out                  HOST "localhost"  PORT 7777
    DECODE     Out  ! fromXml(s)    <-  ? s
ENDDEF

CNECTDEF  Xim ::=
    SERVERSOCK

    CHAN IN   In                    HOST "localhost"  PORT 7777
    DECODE    In ! fromXml(s)       <-  ? s

    CHAN OUT  Out                   HOST "localhost"  PORT 7777
    ENCODE    Out ? b               ->  ! toXml(b)
ENDDEF
```
Let's test our simulator.
1.  Start TorXakis as simulator: run the [TorXakis](TorXakis) with the LuckyPeople model described above in a command window.

`$> torxakis LuckyPeople.txs`

2.  Set the Model and Simulator for testing: In TorXakis type the following commands:

`simulator Model Xim`

TorXakis will hang, waiting for a tester to connect.

3.  Start another TorXakis instance as the Tester: run the [TorXakis](TorXakis) with the LuckyPeople model described above in another command window.

`$> torxakis LuckyPeople.txs`

4.  Set the Model and Sut for testing: In TorXakis type the following commands:

`tester Model Xut`

As soon as you enter this command, you'll see that Simulator also responds:

`TXS >>  Simulator started`

5.  Simulator works a bit slower than the Java implementation. To prevent unexpected delays to cause false negatives, increase delta times of tester.
In the command window of the tester, input following commands:
```
param param_Sim_deltaTime 4000
param param_Sut_deltaTime 4000
```

6.  Start Simulator with a high number of steps, in order to not run out of steps before testing is finished:

`sim 30`

7.  Test the Simulator: In TorXakis type the following command:

`test 10`

Now you'll see the tester TorXakis instance happily testing the simulator:
```
TXS >>  .....1: IN: Act { { ( In, [ Person(Male,"K","D",2,1) ] ) } }
TXS >>  .....2: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  .....3: IN: Act { { ( In, [ Person(Male,"B","L",8,12) ] ) } }
TXS >>  .....4: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  .....5: IN: Act { { ( In, [ Person(Male,"Phdppab","S",19,2) ] ) } }
TXS >>  .....6: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  .....7: IN: Act { { ( In, [ Person(Male,"Addp","Pz",17,8) ] ) } }
TXS >>  .....8: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  .....9: IN: Act { { ( In, [ Person(Female,"Hfhnad","Ln",4,8) ] ) } }
TXS >>  ....10: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  PASS
```

And simulator TorXakis instance acting as the SUT:
```
TXS >>  .....1: OUT: No Output (Quiescence)
TXS >>  .....2: OUT: No Output (Quiescence)
TXS >>  .....3: IN: Act { { ( In, [ Person(Male,"K","D",2,1) ] ) } }
TXS >>  .....4: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  .....5: OUT: No Output (Quiescence)
TXS >>  .....6: IN: Act { { ( In, [ Person(Male,"B","L",8,12) ] ) } }
TXS >>  .....7: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  .....8: IN: Act { { ( In, [ Person(Male,"Phdppab","S",19,2) ] ) } }
TXS >>  .....9: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....10: OUT: No Output (Quiescence)
TXS >>  ....11: IN: Act { { ( In, [ Person(Male,"Addp","Pz",17,8) ] ) } }
TXS >>  ....12: OUT: Act { { ( Out, [ False ] ) } }
TXS >>  ....13: IN: Act { { ( In, [ Person(Female,"Hfhnad","Ln",4,8) ] ) } }
TXS >>  ....14: OUT: Act { { ( Out, [ True ] ) } }
TXS >>  ....15: OUT: No Output (Quiescence)
...
TXS >>  ....30: OUT: No Output (Quiescence)
TXS >>  PASS
```

### Next: [Testing with predetermined input](Modelling-Example-Lucky-People-(Testing-With-Predetermined-Input))
