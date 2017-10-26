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
