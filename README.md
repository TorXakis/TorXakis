![TorXakis logo](https://user-images.githubusercontent.com/661967/29917789-506f21fa-8e43-11e7-9804-596decacebe4.png "TorXakis")

[![Build Status](https://semaphoreci.com/api/v1/capitanbatata/torxakis/branches/develop/badge.svg)](https://semaphoreci.com/capitanbatata/torxakis)
[![Build status](https://ci.appveyor.com/api/projects/status/sv3e96co0019taf9?svg=true)](https://ci.appveyor.com/project/keremispirli/torxakis)
[![Code Climate](https://codeclimate.com/github/TorXakis/TorXakis/badges/gpa.svg)](https://codeclimate.com/github/TorXakis/TorXakis)

# TorXakis

TorXakis is a tool for Model Based Testing.

It is licensed under the [BSD3 license](LICENSE).

## For Users
User documentation at [our wiki](https://github.com/TorXakis/TorXakis/wiki).

## For Developers
TorXakis is written in [Haskell](https://www.haskell.org).

TorXakis uses [stack](https://www.haskellstack.org) to build.

TorXakis needs a [SMT](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories) Solver, such as 
[cvc4](http://cvc4.cs.stanford.edu/web/) and [Z3](https://github.com/Z3Prover/z3).
The SMT Solver needs to support [SMTLIB](http://smtlib.cs.uiowa.edu/) version 2.5,
[Algebraic Data Types](https://en.wikipedia.org/wiki/Algebraic_data_type) (as proposed for version 2.6), 
and [Strings](http://cvc4.cs.stanford.edu/wiki/Strings).
The SMT Solvers are assumed to be located on the [PATH](https://en.wikipedia.org/wiki/PATH_(variable)).

TorXakis uses [javac](https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html) and [GNU Make](https://www.gnu.org/software/make/) for development acceptance tests.

[Haddock Docs](https://torxakis.github.io/TorXakis/doc/index.html)
