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

if [ -f $CACHE_DIR/bin/cvc4 ] && [ -e $CACHE_DIR/bin/cvc4-20180306 ]
then
    echo "$CACHE_DIR/bin/cvc4 found in cache."
else
    echo "cvc4 not found in cache or different version than 20180306"
    rm -f $CACHE_DIR/bin/cvc4*
    curl -L https://github.com/TorXakis/Dependencies/releases/download/cvc4-20180306/cvc4-20180306-x86_64-linux-opt -o cvc4
    mv cvc4 $CACHE_DIR/bin
    chmod +x $CACHE_DIR/bin/cvc4
    touch $CACHE_DIR/bin/cvc4-20180306
fi

if [ -d $CACHE_DIR/z3 ] && [ -e $CACHE_DIR/z3/z3-4.6.0 ]
then
    echo "$CACHE_DIR/z3 build 4.6.0 is found in cache."
else
    echo "z3 not found in cache or different version than 4.6.0"
    rm $CACHE_DIR/z3 -rf
    curl -L -O https://github.com/TorXakis/Dependencies/releases/download/z3-4.6.0/z3-4.6.0-x64-ubuntu-14.04.zip
    CURDIR=$(pwd)
    mkdir $CACHE_DIR/z3 && cd $CACHE_DIR/z3
    Z3ZIP=$(ls $CURDIR/z3*.zip)
    unzip $Z3ZIP
    chmod +x ./bin/z3
    touch ./z3-4.6.0
    cd $CURDIR
fi
