# Scripts to create and test linux packages

This folder contains the following scripts:

- `setup`: builds the
  [Docker](https://www.docker.com/products/container-runtime) containers that
  create Ubuntu packages.
- `package`: builds the Ubuntu packages.
- `test`: tests the installation of the packages created by `package`. These
  tests run inside containers, so no modifications are made to the host system,
  except for the `TorXakis` logs, which are placed in `ci/mk-package/.torxakis`.

After running `package` the `.deb` files will be placed inside `.package-build`.

Ubuntu packages are created inside Docker containers. To run the scripts in
this directory [install
Docker](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-18-04).

After installing Docker, either log in again or run:

```sh
newgrp docker
```

from the terminal session where the `setup`, `package`, and `test` scripts will
be run.

After docker is installed, run `setup` to create the docker containers.

```sh
./ci/mk-package/setup
```

This will create the Docker images that allow to create Ubuntu packages.

Next, create the Ubuntu packages by running:

```sh
./ci/mk-package/package $VERSION
```

where `$VERSION` must be a `TorXakis` `git` tag, e.g.:

```sh
./ci/mk-package/package 0.8.1.1
```

To test the packages run:

```sh
./ci/mk-package/test 0.8.1.1
```
