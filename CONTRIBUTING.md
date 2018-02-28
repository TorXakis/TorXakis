# How to contribute

Thank you for taking the time to contribute to `TorXakis`. We value your
effort, and to make sure that it remains a quality tool that can be extended
and maintained by the community we require that certain requirements are
satisfied before incorporating your changes.

## Adding new features

When adding a new feature, please make sure that it is:

- [ ] Implemented in the simplest possible way: we value simplicity. If
  possible run [`argon`](https://github.com/rubik/argon) on the new functions
  you created, and make sure the cyclomatic complexity is low. Please avoid
  long functions, and duplication like the plague. If introducing a new
  language extension, or esoteric library please justify its use, and document
  it properly.
- [ ] Well structured: it is important to keep an eye on the intra-module
  dependencies. Avoid complex dependency graphs, and deep dependency
  hierarchies. See [this
  article](http://evelinag.com/blog/2014/06-09-comparing-dependency-networks/#.WjK62PZrxhE)
  for some guidelines on structuring functional projects. See [this
  section](#visualizing-the-dependencies) for an explanation on how to
  visualize the dependencies.
- [ ] Properly documented: whenever applicable, there should be a description
  of the syntax and semantics of the new feature (in the form of SOS rules for
  instance), links to literature that contain the theoretical foundations
  behind the new feature, and extensive examples on how to use it.
- [ ] Properly tested: there should be [integration or unit
  tests](#integration-and-unit-tests) for the new feature. We prefer to use
  black-box-testing (i.e. use only the exported functions in the tests)
  whenever possible. If you need to test internal functions, make sure that
  those functions are exposed in an **internal** module. The convention is that
  the internal functions of module `X` should be placed in a file named
  `X/Internal.hs` (this means, in a file `Internal.hs` inside a folder named
  `X`).Try to cover a much as possible in tests, but without sacrificing
  simplicity and maintainability of code. When in doubt, choose simplicity and
  maintainability over coverage.
- [ ] Properly benchmarked: there should be benchmarks that measure the
  performance of the new feature, and how the new feature affects the existing
  benchmarks. Ideally it should be no performance degradation.
- [ ] Adhering to our [code style](#code-style). There is an [automatic
  pretty printer](https://github.com/commercialhaskell/hindent) that can be
  used to format the code.
- [ ] Free of weeds: make sure to [run `weeder`](#running-weeder) before
  submitting your pull request.

      
## Unit and Integration tests

_Unit Tests_ are used for testing the **smallest possible
meaningfully-functional** unit of code. In Haskell’s case, this unit of code
would be a function that is simple enough to handle only a small part of a
functionality.

_Integration Tests_ are big brothers of Unit Tests: They are written and run
the same way, but they cover a **multitude of units or functionalities** within
the package.

Typically a feature development tast would require _multiple unit tests_ and
_at least one integration test_ to be added or changed.

Make sure that you update the tests as necessary and run the full test suite
successfully before submitting your change request. Overall code coverage
should not decrease with your contribution.

**Change or removal of a test should always be justified by either test being
wrong or a behaviour/requirement being changed.**

## Coding style

We
use
[Johan Tibell](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md) style
guide because:

- It is a lightweight style guide.
- It is quite permissive, but also sensible.
- It includes useful guidelines for avoiding unnecessary lazy constructor
  fields.

In addition, use short names for variables, but descriptive names for function
names. For instance we prefer this:

```haskell
myFormula :: Int -> Int -> Int
myFormula x y = (x * y) + x
```

over 

```haskell
f :: Int -> Int -> Int
f number otherNumber = (number * otherNumber) + number
```

## Visualizing the dependencies

I is important to keep an eye on the intra-module dependencies. See [this
article](http://evelinag.com/blog/2014/06-09-comparing-dependency-networks/#.WjK62PZrxhE)
for some guidelines on structuring functional projects.

To visualize the dependency graph, make sure to install `graphmod`, e.g. via `stack`:

```sh
stack install graphmod
```

and then generate the dependency graph by typing:

```sh
find path/to/your/src -name "*.hs" | xargs graphmod -q > torxakis-intra-deps.dotq
dot torxakis-intra-deps.dot  -Tsvg -o torxakis-intra-deps.svg
```

## Running weeder

The [`weeder`](https://github.com/ndmitchell/weeder#readme) tool allows to
detect "weeds" like unused exports, unused dependencies, etc. To keep our
code base clean we recommend to run `weeder` before submitting a pull request.

To install `weeder` we recommend using `stack`:


```sh
stack install weeder
```

Then run the tool from the root of the `TorXakis` project:

```sh
weeder . --build
```

Try and remove the eventual "weeds" that might be reported.
