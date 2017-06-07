TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt


NOT VALID ANYMORE  --  MUST BE UPDATED



=======================================================================

README.txt  -  TorXakis Adder Example

=======================================================================

The directory 'examps/adder' contains a TorXakis MBT example of a
simple arithmetic adder. The adder adds and subtracts two integers.
 There is a specification file with two models, and there is a SUT.
 Communication between TorXakis and SUT occurs via sockets, where
the SUT acts as the server-side.

Inputs:  Plus <n> <m>
         Minus  <n> <m>
Output:  <n+m> or <n-m>, respectively


Prerequisites
=============

Java installed;
see:  http://java.com


SUTs
====

'Adder.java'
Java SUT with a single adder, communicating as server via stream-mode
socket, with <portnr> as argument.

usage:  $ javac Adder.java              # compile Adder
        $ java Adder <portnr>           # start Adder
test    <in separate window>
        $ telnet localhost <portnr>
        Plus 25 17
         42
        ...

note: for Windows perhaps better with `putty' instead of `telnet'


Models
======

- 'Adder.txs'
  TorXakis model for a single Adder,
  communicating via <localhost,7890>.

usage:  <start SUT in separate window>  # SUT must be running and
                                        # listening on port 7890
                                        # before the command 'sut Sut1'
        $ torxakis Adder.txs
        TXS >> spec Adder1       # initialize model with single adder
        TXS >> adap Adap1        # initialize adapter
        TXS >> sut  Sut1         # initialize sut
                                 # 'run ini1' combines these 3 commands
        TXS >> test <n>          # run <n> test steps
        TXS >> ...
        TXS >> help              # TorXakis help for more possibilities


- 'Adder.txs'
  TorXakis model for three parallel Adders,
  communicating via <localhost,7890> <localhost,7891> <localhost,7892>.

usage:  <start 3 SUTs in 3 windows>     # SUTs must be running and
                                        # listening on ports 7890,
                                        # 7891, and 7892, before the
                                        # command 'sut Sut1'
        $ torxakis Adder.txs        
        TXS >> spec Adder3       # initialize model with three adders
        TXS >> adap Adap3        # initialize adapter for 3 SUTs
        TXS >> sut  Sut3         # initialize 3 SUTs
                                 # 'run ini3' combines these 3 commands
        TXS >> test <n>          # run <n> test steps
        TXS >> ...
        TXS >> help              # TorXakis help for more possibilities


Additional Files
================

- 'ini1' and 'ini3'
  TorXakis commands files
  with commands to start the model, the adapter, and the sut;
  these commands can also be run separately

=======================================================================

