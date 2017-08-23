# TorXakis quality assurance

This folder contains the quality assurance tests for TorXakis.

To run the development acceptance tests execute:

```sh
stack test
```

Individual development acceptance tests can be selected by passing the `match`
flag to `hspec` via `stack`:

```sh
stack test --test-arguments=--match=PATTERN
```

where `PATTERN` is a pattern that allows to select the name of the test to run.
For instance, to run the stimulus response tests only the following command can
be used:

```sh
stack test --test-arguments=--match=Stimulus
```

## Adding new tests

When adding a new tests
use [camel case](https://en.wikipedia.org/wiki/Camel_case) folder names. The
rationale behind this is that we want to be consistent with Haskell naming
conventions regarding module folders.
