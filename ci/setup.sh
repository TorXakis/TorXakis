# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

set -e

javac ./test/copyright/Copyright.java
java -cp ./test/copyright Copyright . 0 ./.semaphore-cache

sudo apt-get install -y libgmp3-dev
# Download and unpack the stack executable
if [ ! -d $CACHE_DIR/bin ]
then
    mkdir -p $CACHE_DIR/bin
fi

if [ -f $CACHE_DIR/bin/stack ]
then
    echo "$CACHE_DIR/bin/stack found in cache."
else
    curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C $CACHE_DIR/bin '*/stack'
fi

if [ -f $CACHE_DIR/bin/cvc4 ]
then
    echo "$CACHE_DIR/bin/cvc4 found in cache."
else
    curl -L -O https://github.com/TorXakis/Dependencies/releases/download/v0.3.0_linux/cvc4-1.5-x86_64-linux-opt
    # Making symbolic links to the cvc4 executable to get the build working
    # Using cvc4 as executable name to avoid having to update the configuration file in CI.
    ln -s cvc4-1.5-x86_64-linux-opt cvc4
    mv cvc4* $CACHE_DIR/bin
    chmod +x $CACHE_DIR/bin/cvc4*
fi

if [ -d $CACHE_DIR/z3 ] && [ -e $CACHE_DIR/z3/build-059bad909ad4 ]
then
    echo "$CACHE_DIR/z3 build 059bad909ad4 is found in cache."
else
    echo "z3 not found in cache or different version than 059bad909ad4"
    rm $CACHE_DIR/z3 -rf
    curl -L -O https://github.com/TorXakis/Dependencies/releases/download/z3-4.5.1/z3-4.5.1.059bad909ad4-x64-ubuntu-14.04_20170905.zip
    CURDIR=$(pwd)
    mkdir $CACHE_DIR/z3 && cd $CACHE_DIR/z3
    Z3ZIP=$(ls $CURDIR/z3*.zip)
    unzip $Z3ZIP
    chmod +x ./bin/z3
    touch ./build-059bad909ad4
    cd $CURDIR
fi
