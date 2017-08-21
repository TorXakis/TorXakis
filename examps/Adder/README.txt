TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.


=======================================================================

README.txt  -  TorXakis Adder Example

=======================================================================

The directory 'examps/adder' contains a TorXakis MBT example of a
simple arithmetic adder. The adder adds and subtracts two integers.

There are multiple specification files which present various approaches
for testing Adder:
- Adder.txs
- AdderStAut.txs
- MAdder.txs
- PAdder.txs

Communication between TorXakis and SUT (Adder) occurs via sockets,
where the SUT acts as the server-side.

Inputs: Plus <n> <m>
        Minus  <n> <m>
Output: <n+m> or <n-m>, respectively


Prerequisites
=============

Java installed:
http://java.com


SUTs
====

Adder.java
----------
Java SUT with a single adder, communicating as server via stream-mode
socket, with <portnr> as argument.

usage:  $ javac Adder.java              # compile Adder
        $ java Adder <portnr>           # start Adder
test    <in separate window>
        $ telnet localhost <portnr>
        Plus 25 17
         42
        ...

NOTE: For Windows perhaps better with `putty' instead of `telnet'.


Models
======

Adder.txs
---------
Includes two models (Adder and Adder3), two SUT representations (Sut and Sut3), and one simulator
specification (Sim).

==> Adder Model and Sut Connection
TorXakis model for a single Adder, communicating via <localhost,7890>.

usage:  # To observe the behaviour of model,
        # you can use the stepper
        $ torxakis Adder.txs
        TXS >> stepper Adder
        TXS >> step 10
        TXS >> ...
        
        <start SUT in separate window>  # To execute TorXakis against the actual SUT,
                                        # SUT must be running and
                                        # listening on port 7890
                                        # before the command 'tester Adder Sut'
        $ java -cp Adder 7890

        $ torxakis Adder.txs
        TXS >> tester Adder Sut
        TXS >> test 10
        TXS >> ...
        TXS >> help              # TorXakis help for more possibilities


==> Adder3 Model and Sut3 Connection:
  TorXakis model for three parallel Adders,
  communicating via <localhost,7890> <localhost,7891> <localhost,7892>.

usage:  # To observe the behaviour of model,
        # you can use the stepper
        $ torxakis Adder.txs
        TXS >> stepper Adder3
        TXS >> step 20
        TXS >> ...
        
        <start 3 SUTs in 3 windows>     # To execute TorXakis against the actual SUT,
                                        # SUTs must be running and
                                        # listening on ports 7891,
                                        # 7892, and 7893, before the
                                        # command 'tester Adder3 Sut3'
        $ java -cp Adder 7891
        $ java -cp Adder 7892
        $ java -cp Adder 7893

        $ torxakis Adder.txs
        TXS >> tester Adder3 Sut3
        TXS >> test 20
        TXS >> ...
        TXS >> help              # TorXakis help for more possibilities

        
AdderStAut.txs
--------------
This example defines same Adder model using State Automation
instead of a Procedure. Sut and Sim are exactly same with Adder.txt.

usage:  # To observe the behaviour of model,
        # you can use the stepper
        $ torxakis AdderStAut.txs
        TXS >> stepper Adder
        TXS >> step 10
        TXS >> ...
        
        <start SUT in separate window>  # To execute TorXakis against the actual SUT,
                                        # SUT must be running and
                                        # listening on port 7890
                                        # before the command 'tester Adder Sut'
        $ java -cp Adder 7890

        $ torxakis Adder.txs
        TXS >> tester Adder Sut
        TXS >> test 10
        TXS >> ...
        TXS >> help              # TorXakis help for more possibilities

=======================================================================
