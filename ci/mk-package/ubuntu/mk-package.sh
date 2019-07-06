#!/bin/bash

git --version
stack --version

echo "Cloning TorXakis"

cd /build

echo "Hello from docker" >> hello.txt

if [ -d /build/TorXakis ]
then
    echo "TorXakis already cloned"
else
    git clone https://github.com/TorXakis/TorXakis.git
fi

cd TorXakis

git fetch origin

git checkout $TXS_VERSION

git pull origin $TXS_VERSION

export STACK_ROOT="/build/.stack"

# We will be using a stack configuration file that is linux specific
export STACK_YAML="stack_linux.yaml"

stack build --fast

export TXS_TMP=/build
mkdir -p ${TXS_TMP}/usr/bin

TXS_BIN=`stack path --local-install-root --silent --stack-yaml stack_linux.yaml`/bin

cp ${TXS_BIN}/* ${TXS_TMP}/usr/bin/

export CACHE_DIR="/build"

echo "Looking for cvc4 in ${CACHE_DIR}/usr/bin/)"

if [ -f $CACHE_DIR/usr/bin/cvc4 ] && [ -e $CACHE_DIR/usr/bin/cvc4-1.7 ]
then
    echo "${CACHE_DIR}/usr/bin/cvc4 found in cache."
else
    echo "cvc4 not found in cache or different version than 1.7"
    rm -f $CACHE_DIR/usr/bin/cvc4*
    curl -L https://github.com/TorXakis/Dependencies/releases/download/cvc4_1.7/cvc4-1.7-x86_64-linux-opt -o $CACHE_DIR/usr/bin/cvc4
    chmod +x $CACHE_DIR/usr/bin/cvc4
    touch $CACHE_DIR/usr/bin/cvc4-1.7
fi

if [ -f $CACHE_DIR/usr/bin/z3 ] && [ -e $CACHE_DIR/usr/bin/z3-4.8.5 ]
then
    echo "$CACHE_DIR/usr/bin/z3 found in cache."
else
    echo "z3 not found in cache or different version than z3-4.8.5"
    rm $CACHE_DIR/usr/bin/z3 -rf
    curl -L https://github.com/TorXakis/Dependencies/releases/download/z3-4.8.5/z3-4.8.5-x64-ubuntu-14.04.zip -o z3.zip
    unzip -p z3.zip "*/bin/z3" > $CACHE_DIR/usr/bin/z3
    chmod +x $CACHE_DIR/usr/bin/z3
    touch $CACHE_DIR/usr/bin/z3-4.8.5
fi

cd /build

# Create the deb package
# TODO: change -v to $TXS_VERSION once the tag is updated
fpm -s dir \
    -t deb \
    -n torxakis \
    -v 0.8.1 \
    -C $TXS_TMP \
    -d "libgmp-dev" \
    -d "libexpat1" \
    -d "netbase" \
    -d "libgomp1" \
    -p torxakis_${TXS_VERSION}-ubuntu_${UBUNTU_VERSION}-amd64.deb \
    usr/bin
# -p torxakis_VERSION_ARCH.deb \

# # Create an rpm from the deb package
echo "Creating torxakis_${TXS_VERSION}-ubuntu_${UBUNTU_VERSION}-amd64.deb"

# fpm -t rpm -s deb torxakis_${TXS_VERSION}-ubuntu_${UBUNTU_VERSION}-amd64.deb

echo "Done!"
