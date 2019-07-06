
[Docker installation instructions](https://www.digitalocean.com/community/tutorials/how-to-install-and-use-docker-on-ubuntu-18-04)

Remember to run:

```sh
newgrp docker
```

To be able to use Docker without sudo without logging out.

Build:

```sh
docker build --tag=torxakis-packager:16.04 -f 16.04/Dockerfile .
```

Run:

```sh
docker run --rm \
  -e TXS_VERSION=0.8.1 \
  -e UBUNTU_VERSION=16.04 \
  -v /tmp/build:/build \
  -ti torxakis-packager:16.04 mk-package.sh
```


Version 0.8.1 will not build with latest stack. Stack configuration is wrong.


Test:
