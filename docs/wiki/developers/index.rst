Information for developers
==========================

Making Linux packages
---------------------

The ``ci/mk-package`` provides the scripts necessary to create linux ``deb``
and ``rpm`` packages and test them. These scripts are meant to be run from the
root folder of the ``TorXakis`` repository.

The ``setup.sh`` script will install the necessary packages.

The ``package.sh`` script will create the packages.

The ``test.sh`` script will run Ubuntu docker containers (note that this
requires docker to be installed in your machine), where the packages will be
installed and tested. The ``install-test.sh`` script is called from the docker
containers to perform the ``TorXakis`` installation there.


