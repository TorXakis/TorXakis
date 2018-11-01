# Scripts to create and test linux packages

The following instructions were tested on Ubuntu.

Before proceeding with packaging, make sure that `TorXakis` was built, since
these scripts take the binaries produced by `stack`.

From the root directory of this repository, run:

```sh
./ci/mk-package/setup
```

To create the package then run:

```sh
./ci/mk-package/package
```
