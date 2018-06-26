# The TorXakis compiler

This package contains the compiler that translates `TorXakis` models described
in text files to their corresponding `Haskell` data structures.

The [`compileLegacy`](src/TorXakis/Compiler.hs#L155) function, will parse a
string containing a `TorXakis` model into a type `(Id, TxsDefs, Sigs VarId)`.
If the model contains an error, an error will be thrown. The use of
`compileLegacy` is discouraged, and the function is kept for the sake of
backwards compatibility. Instead of this function, use `compileFile` which will
return either the tuple above, or an [`Error`](src/TorXakis/Compiler/Error.hs)
value.


## Architecture

The compiler that generates `Haskell` data structures out of text files can
divided in two parts: a parser that takes text and produces abstract syntax
trees (or AST) -see `TorXakis.Parser.Data`-, and a compiler that transforms
these AST's into a tuple `(Id, TxsDefs, Sigs VarId)`.

The `parseString` function in `TorXakis.Parser` will collect the AST's in a
structure called `ParsedDefs`, which group the `TorXakis` top-level definitions
together (e.g. `MODELDEF`'s, `FUNCDEF`'s, etc).

These top-level AST's will be handled further by `compileParsedDefs`, which will
make a tuple `(Id, TxsDefs, Sigs VarId)` from the given parsed-definitions
(`ParsedDefs`). This function has several stages:

1. Generate the `SortId`'s.
2. Generate a the `CstrId`'s.
3. Generate a map from locations of function declarations to `FuncId`'s.
4. Generate a map from locations of variable references to the location in
   which these entities (variables or functions) are declared.
5. Infer the types of all variable declarations.
6. Construct a map from the variable declarations to `VarId`'s.
7. Generate a map from function declarations to their definitions (signature
   and handler).
8. Generate a map from process declarations to their definitions.
9. Generate the `TorXakis` `Sigs`.
10. Generate the `TorXakis` `TxsDefs`.

At each stage, the maps that are required by the subsequent functions are made.

### Composite maps

Most of the functions used in the compiler require maps (dictionaries), like a
map from variable references to the entities they refer to, or a map from
`Text` to `SortId`. Maps are passed as a single value in a composite map.
Composite maps are defined in the `TorXakis.Compiler.MapsTo` module, and its
usage is illustrated in the test module `TorXakis.Compiler.MapsToSpec`. 

Several functions in the compiler define what the composite map should contains
by means of
[type-constraints](https://en.wikibooks.org/wiki/Haskell/Classes_and_types#Type_constraints).
For instance, to specify that a map should contain a map from `Text` to
`SortId` and a map from `Text` to `VarId` the following type-constraint can be
specified:

```haskell
myFunc :: (MapsTo Text SortId mm, MapsTo Text VarId mm) => mm -> Text -> Text
```

In addition it is possible to specify that a map **shall not contain** a given
map. For instance, the constraint:

```haskell
In (Text, SortId) (Contents mm) ~ 'False
```

specifies that the composite map `mm` **shall not contain** a map from `Text`
to `SortId`.

## Roadmap

The roadmap for future improvements in the TorXakis compiler can be found in
[this](https://github.com/TorXakis/TorXakis/projects/4) github project.
