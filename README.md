![TorXakis logo](https://git.io/vFvfj "TorXakis")

[![Build Status](https://semaphoreci.com/api/v1/capitanbatata/torxakis/branches/develop/badge.svg)](https://semaphoreci.com/capitanbatata/torxakis)
[![Build status](https://ci.appveyor.com/api/projects/status/sv3e96co0019taf9?svg=true)](https://ci.appveyor.com/project/keremispirli/torxakis)
[![Code Climate](https://codeclimate.com/github/TorXakis/TorXakis/badges/gpa.svg)](https://codeclimate.com/github/TorXakis/TorXakis)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

# TorXakis

TorXakis is a tool for Model Based Testing.

It is licensed under the [BSD3 license](LICENSE).

## For Users
User documentation at [our wiki](https://github.com/TorXakis/TorXakis/wiki).

## Installation

### Windows
For Windows systems an installer is provided in the [releases][13] section.

### Linux

For Linux systems we provide a [`snap`][12] package. To install `TorXakis`
run:

```sh
sudo snap install torxakis --edge
```

If you don't have the `snap` package manager installed on your Linux
distribution see the [installation instructions for your platform][14].

When running `TorXakis` as a snap, the configuration file for `TorXakis` should
go in the `~/snap/torxakis/current` directory. This is because snaps run in an
isolated environment, and they are not able to see any hidden files in the
users home directory.

If you are installing `TorXakis` using a downloaded snap (say
`torxakis_nightly_amd64.snap`), then you need to provide the path to the snap
along with the `--dangerous` flag, since you are not fetching the snap from the
official Ubuntu repository.

```sh
sudo snap install torxakis_nightly_amd64.snap --dangerous
```

## For Developers

TorXakis is written in [Haskell](https://www.haskell.org) and
uses [`stack`][9] as build tool. In addition
TorXakis needs an [SMT][1] solver, such as [cvc4][2] or [Z3][3]. The SMT Solver
needs to support [SMTLIB][4] version 2.5, [Algebraic Data Types][5] (as
proposed for version 2.6), and [Strings][6]. The SMT Solvers are assumed to be
located on the [PATH][7]. For development acceptance tests [javac][8] is needed.

The Haddock documentation is also
available [here](https://torxakis.github.io/TorXakis/doc/index.html).

### Building

Make sure that [`stack`][10] is installed, then clone this repository.
`TorXakis` requires a `stack` version `1.6.1` or greater. To build and install
`TorXakis` locally, if you are on a Windows system run:

```sh
stack setup
stack install
```

For Unix systems a different stack configuration is used:

```sh
stack setup --stack-yaml stack_linux.yaml
stack install --stack-yaml stack_linux.yaml
```

The reason for having two configuration files is that on Windows systems the
libraries are linked statically, and thus we cannot use the `integer-gmp`
library, since `TorXakis` is licensed under the [BSD3 license](LICENSE).

In the instructions that follow we will omit the `--stack-yaml
stack-linux.yaml` flag, but bear in mind that if you're in an Unix system you
need to add it (or set the `STACK_YAML` environment variable to
"stack-linux.yaml")

### Testing

When submitting a pull request, our continuous integration process will run a
series of tests. There are two levels of tests that we use in `TorXakis`:
unit-tests and integration tests. Unit-tests can be run when building using
`stack` by passing this `--test` flag:

```sh
stack install --test
```

To run the integration tests go to the `test/sqatt` folder, and run:

```sh
stack test
```

See the [README](test/sqatt/README.md) in the `sqatt` folder for more
information.

### Benchmarking

It is important to keep an eye on the performance of `TorXakis`. Currently
there is no continuous benchmarking process in place, however you can check
locally the performance by running the benchmarks for the (current) `develop`
version, and the version that you are working on. To run the benchmarks and
generate the reports on Unix systems you can run:

```sh
cd test/sqatt
stack bench --ba "--output  `date +%s`-report-`git rev-parse HEAD`.html --csv `date +%s`-report-`git rev-parse HEAD`.csv"
```

On Windows PowerShell you can run:

```posh
cd test\sqatt
stack bench --ba "--output
$(get-date).ToString("yyyyMMdd-hhmmss")-report-$((git rev-parse
HEAD).Substring(0,8)).html --csv $(get-date).ToString("yyyyMMdd-hhmmss")-report-$((git rev-parse HEAD).Substring(0,8)).csv"
```

See the [README](test/sqatt/README.md) in the `sqatt` folder for more
information.


### Repository structure

There are several folders in this repository, which serve different purposes:

- [`bin`](bin/): utility scripts to start `TorXakis` UI and server.
- [`ci`](ci/): scripts used in our continuous integration process.
- [`docs`](docs/): documentation files for `TorXakis`.
- [`examps`](examps/): example models and systems-under-test (SUT's) to play
  around with. This folder also contains some binary (.jpg and .pptx) files,
  which are used for documentation. Such files are tracked with [Git-LFS][15](*).
- [`snap`](snap/): files needed to create Linux [snaps][12].
- [`sys`](sys/): packages that make up `TorXakis` this is where the source code
  resides.
- [`test`](test/): utilities for quality assurance tests (integration-tests,
  license-checking, etc).

See the README files in each folders to get a more detailed explanation.

(*): Git-LFS is installed by default along with official git client, so you should
     be able to access these files without extra effort. If, for some reason, you
     don't have Git-LFS, then you can [install the official Git-LFS extension][15].

[1]: https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
[2]: http://cvc4.cs.stanford.edu/web/
[3]: https://github.com/Z3Prover/z3
[4]: http://smtlib.cs.uiowa.edu/
[5]: https://en.wikipedia.org/wiki/Algebraic_data_type
[6]: http://cvc4.cs.stanford.edu/wiki/Strings
[7]: https://en.wikipedia.org/wiki/PATH_(variable)
[8]: https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
[9]: https://www.haskellstack.org
[10]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[11]: https://github.com/TorXakis/TorXakis/issues/40
[12]: https://www.ubuntu.com/desktop/snappy
[13]: https://github.com/TorXakis/TorXakis/releases
[14]: https://docs.snapcraft.io/core/install
[15]: https://git-lfs.github.com/
