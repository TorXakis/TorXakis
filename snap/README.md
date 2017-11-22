# Snap package configuration for TorXakis

`TorXakis` provides a [`snap`](https://www.ubuntu.com/desktop/snappy) package,
which allows users of Linux systems to readily install `TorXakis`. This folder
contains the files necessary to create such package.

To create this snap make sure `snapcraft` is installed:

```sh
sudo snap install --beta --classic snapcraft 
```

Then at the root level of the `TorXakis` repository, make sure you build
`TorXakis` to create the binaries that will be packaged into the snap:

```sh
stack build --test --stack-yaml stack_linux.yaml
```

and then create the snap by running:

```sh
snapcraft
```

If you rebuild `TorXakis` remember to clean `torxakis-bin`:

```sh
snapcraft clean torxakis-bin 
```

The command above will make sure that the new executable packaged.

## Releasing a new version of TorXakis

To release a new version of `TorXakis` as a `snap` run:

```sh
snapcraft push <torxakis.snap>
```

Where `<torxakis.snap>` is the name of the file created by the `snapcraft`
command. For instance:

```sh
snapcraft push torxakis_nightly_amd64.snap
```

This command will output the revision number that was created, for instance:

```text
Revision 2 of 'torxakis' created.
```

To release the `TorXakis` version, whose revision number `n` is given as output
of the command above, run:

```sh
snapcraft release torxakis n edge
```

For now we are releasing TorXakis to the "edge" channel only.

For more information
see
[the documentation entry about publishing snaps](https://docs.snapcraft.io/build-snaps/publish).
