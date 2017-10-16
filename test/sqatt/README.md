# Simple quality assurance tests for TorXakis (sqatt)

This folder contains the quality assurance tests for TorXakis.

## Integration tests
To run the integration tests execute:

```sh
stack test
```

Individual development acceptance tests can be selected by passing the `match`
flag to `hspec` via `stack`:

```sh
stack test --test-arguments=--match=PATTERN
```

where `PATTERN` is a pattern is used to select the name of the test to run. For
instance, to run **only** the stimulus response tests the following command can be
used:

```sh
stack test --test-arguments=--match=Stimulus
```

### Adding new tests

`sqatt` assumes that the TorXakis models and SUT source-code (if any) are
located in the `examps` folder, and the commands to be input to TorXakis during
the tests are stored in the `test/examps` folder. The `Examples.Paths` module
contains functions to build paths to the test files using these conventions.

To add a new test case to be included in the development acceptance tests the
following steps are needed:

- Create a new Haskell file in the `src/Examples` folder, which will describe
the test cases.
- Add tests cases by specifying:
    - The example name.
    - The paths to the _.txs_ files that contain the TorXakis model.
    - The path to the file that contains TorXakis commands.
    - Command line arguments, if any, for the `TorXakis` server command
      (`txserver`).
    - If needed: the path to the SUT source code (currently only Java is
    supported), or the path to the TorXakis commands that will be used to
    run TorXakis as a SUT simulator.
- Define a function named `exampleSet` containing the list of examples that
will be tested.
- Update the `src/Examples/All.hs` file to include the new example set in
the list of all examples.
- Update the `sqatt.cabal` file to account for the the new file.

Some variation is of course allowed in these steps, but for the sake of
maintainability it is recommended to follow them as close as possible.

When adding a new test
use [camel case](https://en.wikipedia.org/wiki/Camel_case) folder names. The
rationale behind this is that we want to be consistent with Haskell naming
conventions regarding module folders.

We will illustrate the process of adding new development-acceptance tests by
using an example. Suppose we want to create a new test, named `MyTest`, to test
a new TorXakis model that is stored in `examps/MyTest/MyTestModel.txs`. This
model will be used to test a toy SUT whose source code is located in
`examps/MyTest/MySUT.java`. Finally assume the commands to be input to TorXakis
during the tests are in `test/examps/MyTest/MyTestCmds.txscmd`. Note that these
files do not exist in the code-base, and are used as illustration only.

To define a new development-acceptance test that uses these files, start by
creating a file in the `src/Examples` folder named `MyTest.hs`. Make sure that
you enable the `OverloadedStrings` compiler extension, and import
`Examples.Paths` and `Sqatt`, while hiding `Prelude`'s `FilePath` (since it
conflicts with the one used by `Sqatt`):

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Examples.MyTest (exampleSet) where

import           Examples.Paths
import           Prelude        hiding (FilePath)
import           Sqatt
```

Next, we add a test case by defining a new `TxsExample` value:

```haskell
test0 :: TxsExample
test0 = TxsExample
  { exampleName = "What the test is checking"
  , txsModelFiles = [txsFilePath "MyTest" "MyTestModel"]
  , txsCommandsFile = [txsCmdPath "MyTest" "MyTestCmds"]
  , txsServerArgs = []
  , sutExample = 
      Just (JavaExample (javaFilePath "MyTest" "MySUT") ["Some", "SUT", "Args"])
  , expectedResult = Pass
  }

```

The record labels should be self explanatory. For more information please check
the existing `sqatt` tests (defined in `src/Examples`) . Also note that we are
repeating the string `"MyTest"`, which could be avoided. See the `src/Examples`
folder to see how this is currently done.

In the example above we specify a Java SUT. It is also possible to specify no
SUT or a TorXakis simulated one. The `src/Examples` folder contains examples of
these cases.

Once the example `test0` is defined, the next step is to add it to a
`TxsExampleSet`:

```haskell
exampleSet :: TxsExampleSet
exampleSet = TxsExampleSet "My Test" [test0]
```

If additional examples are needed, define them as explained above, and add them
to the list of the `exampleSet` value above.

To have this example set tested with `sqatt` add it to the `Examples.All`
module:

```haskell
-- ...

module Examples.All
  ( allExamples
  )
where

import qualified Examples.Adder            as Adder

-- ...

import qualified Examples.MyTest as MyTest

-- ...

allExamples :: [TxsExampleSet]
allExamples = [ Adder.exampleSet
              -- ...
              , MyTest.exampleSet
              -- ...
              ]
```

Finally, update the `sqatt.cabal` file by adding `Examples.MyTest` to the
`exposed-modules` section, and run:

```sh
stack test --test-arguments="--match=MyTest"
```

Notice that we're using the `match` flag to run only the newly added test.

The log files will be written in the TorXakis root directory in a folder named
`sqatt-logs` (currently this cannot be configured). The logs are structured
hierarchically as follows: test date and time, test set name, test name. To
avoid problems in Windows systems, in the log folders spaces will be replaced
by underscores (`_`) and colons (`:`) will be replaced by dashes (`-`).

### Long-running tests

Some example cases are known to take longer than others. We run these tests in
parallel with the rest, in order to prevent them from timing the whole build
out. These tests have "**#long**" tag in their names. If you add a test that
you expect to take longer, you can have them run in the Long Tests group by
adding _#long_ to their name.

[Customers and Orders test](src/Examples/CustomersOrders.hs) is an example to
such tests.

```haskell
test = TxsExample
  { exampleName = "Customers & Orders Test #long"
  , txsModelFiles = [txsFilePath exampDir customersOrdersText]
...
```

The parallelization of these tests are handled by our CI provider by running
two separate test jobs. Long running tests are run with "_--match=#long_" test
parameter while faster tests are run with "_--skip=#long_".

These jobs are scripted in [test.sh](../../ci/test.sh)
and [test_long.sh](../../ci/test_long.sh) respectively.

## Benchmarks

To run the benchmarks execute:

```sh
stack bench
```

Individual benchmarks can be selected by passing a pattern to the benchmark
program.

```sh
stack bench --ba "-m glob GLOB_PATTERN"
```

where `GLOB_PATTERN` is a Unix
style [glob-pattern](https://en.wikipedia.org/wiki/Glob_(programming)#Unix)
that is used to select the benchmark names. Benchmark names have the form
`<benchmark set>/<single benchmark name>`, for instance:

```text
- Sequence/100 actions
- Sequence/100 data actions
- Parallel/100 data actions
```

So, for instance, to run only the data actions we can run `stack` as follows:

```sh
stack bench --ba "-m glob */*data*"
```

If on the other hand, we would have wanted to run only the benchmark named
`"Sequence/100 data actions"` we could have run:

```sh
stack bench --ba "-m glob Sequence/*data*"
```

or even:

```sh
stack bench --ba "-m glob Seq*/*data*"
```

Note that globbing on the benchmarks works as though they were filepaths,
that's why `"*"` won't match any benchmarks.

To generate HTML output use the `--output` flag, for example:

```sh
stack bench --ba "-m glob Seq*/*data* --output report.html"
```

The benchmark program uses Criterion's default main method, which supports a
wide range of options. Run:

```sh
stack bench --ba "--help"
```

for the full list of options.

### Tests for the benchmarks

Since it is desirable that the benchmarks do not fail, `sqatt` includes a test
suite for the benchmarks, which check for their sanity. To run only this test
suite use:

```sh
stack test sqatt:benchmarks-sanity-test
```

This is also useful when debugging a failed benchmark, since when running the
benchmarks no files are logged.
