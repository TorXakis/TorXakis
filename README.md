![TorXakis logo](https://git.io/vFvfj "TorXakis")

[![Build Status](https://semaphoreci.com/api/v1/torxakis-admin/torxakis/branches/develop/badge.svg)](https://semaphoreci.com/torxakis-admin/torxakis)
[![Build status](https://ci.appveyor.com/api/projects/status/ppk53o7uh7e6aie9/branch/develop?svg=true)](https://ci.appveyor.com/project/torxakis-admin/torxakis/branch/develop)
[![Code Climate](https://codeclimate.com/github/TorXakis/TorXakis/badges/gpa.svg)](https://codeclimate.com/github/TorXakis/TorXakis)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

# TorXakis

`TorXakis` is a tool for Model Based Testing.

It is licensed under the [BSD3 license](LICENSE).

## For Users
User documentation at [our wiki](https://github.com/TorXakis/TorXakis/wiki).

## Installation

### Windows
For Windows systems an installer is provided in the [releases][13] section.

### Linux

We provide a `deb` package from Debian based systems (Debian, Ubuntu, etc), and an
`rpm` package for distros using the RPM package manager. Below we give
instructions on how to install `TorXakis` on Ubuntu.

Download the latest `deb` release of `TorXakis` (`torxakis_0.6.0_amd64.deb` in
the example below) and then run the following commands:

```sh
apt-get update
apt-get install ./torxakis_0.6.0_amd64.deb -y
```

The `deb` package was tested on Ubuntu version `16.04`, `17.10`, and `18.04`.

For installing `TorXakis` in RPM based distros please check your Linux distro
manual.

### macOS ###

For macOS systems we provide a homebrew package. To install `TorXakis` run:

```sh
brew tap torxakis/torxakis
brew install torxakis
```

For more detailed instructions see the [Homebrew tap for
TorXakis](https://github.com/TorXakis/homebrew-TorXakis).

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

On Linux, the following libraries should be installed:
- libgmp-dev

For Unix systems a different stack configuration is used:

```sh
stack setup --stack-yaml stack_linux.yaml
stack install --stack-yaml stack_linux.yaml
```

```stack install``` makes the resulting executable available globaly. For developing purposes, ```stack build``` can be used instead. The resulting executable can then be accessed using ```stack exec torxakis -- <torxakis_options> ```.

The reason for having two configuration files is that on Windows systems the
libraries are linked statically, and thus we cannot use the `integer-gmp`
library, since `TorXakis` is licensed under the [BSD3 license](LICENSE).

In the instructions that follow we will omit the `--stack-yaml
stack-linux.yaml` flag, but bear in mind that if you're in an Unix system you
need to add it (or set the `STACK_YAML` environment variable to
"stack-linux.yaml")

If you experience problems when building from source, sometimes this could be
due to Makefiles which do not handle parallel builds well. If problems appear
while building, try setting the `MAKEFLAGS` variable to `" -j1 "` before
running the build commands. For instance on Unix systems this can be achieved
by running:

```sh
export MAKEFLAGS=" -j1 "
stack setup
stack install
```

Note that the `MAKEFLAGS` variable does not affect the parallelism within
`stack` itself.

### Developing

By default, several compiler warnings are treated as errors. While developing
we'd like to avoid this: removing warnings requires some extra time. Some parts
of the code might not make it when submitting a pull request. Therefore, before
a new feature or fix is ready, it doesn't make sense to try to remove warnings
on code that will be deleted in the end. So to have the build system avoid
treating warnings as errors use the flag `develop` as follows:

```sh
stack <STACK ARGS> --flag '*:develop'
```

For instance:

```sh
stack test --stack-yaml stack_linux.yaml --file-watch txs-compiler  --ta "-m wrong" --flag '*:develop'
```

Note that currently only the package `txs-compiler` supports this flag. If you
find yourself needing this flag for other packages feel free to submit an
issue or pull request.

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
- [`ci/mk-package`](ci/mk-package): files needed to create Linux packages.
- [`docs`](docs/): documentation files for `TorXakis`.
- [`examps`](examps/): example models and systems-under-test (SUT's) to play
  around with. This folder also contains some binary (.jpg and .pptx) files,
  which are used for documentation. Such files are tracked with [Git-LFS][15](*).
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
[13]: https://github.com/TorXakis/TorXakis/releases
[15]: https://git-lfs.github.com/

## Acknowledgements
Part of the research and development of TorXakis is carried out 
* as part of the NWO-TTW project 13859: SUMBAT: Supersizing Model-Based Testing.
* as part of the Enable-S3 program under the responsibility of ESI (TNO) with Philips as the carrying industrial partner. The Enable-S3 research is supported by the Netherlands Ministry of Economic Affairs (Toeslag voor Topconsortia voor Kennis en Innovatie).
