# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.


set -e

javac ./test/copyright/Copyright.java
java -cp ./test/copyright Copyright . 0 ./.semaphore-cache

sudo apt-get install -y libgmp-dev
# Download and unpack the stack executable
if [ ! -d $CACHE_DIR/bin ]
then
    mkdir -p $CACHE_DIR/bin
fi

if [ -f $CACHE_DIR/bin/stack ] && [ -e $CACHE_DIR/bin/stack-2.1.1 ]
then
    echo "$CACHE_DIR/bin/stack found in cache."
else
    echo "stack not found in cache or different version than 2.1.1."
    rm -f $CACHE_DIR/bin/stack*
    curl -L https://github.com/commercialhaskell/stack/releases/download/v2.1.1/stack-2.1.1-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C $CACHE_DIR/bin '*/stack'
    touch $CACHE_DIR/bin/stack-2.1.1
fi

if [ -f $CACHE_DIR/bin/cvc4 ] && [ -e $CACHE_DIR/bin/cvc4-1.7 ]
then
    echo "$CACHE_DIR/bin/cvc4 found in cache."
else
    echo "cvc4 not found in cache or different version than 1.7"
    rm -f $CACHE_DIR/bin/cvc4*
    curl -L https://github.com/TorXakis/Dependencies/releases/download/cvc4_1.7/cvc4-1.7-x86_64-linux-opt -o cvc4
    mv cvc4 $CACHE_DIR/bin
    chmod +x $CACHE_DIR/bin/cvc4
    touch $CACHE_DIR/bin/cvc4-1.7
fi

if [ -d $CACHE_DIR/z3 ] && [ -e $CACHE_DIR/z3/z3-4.8.5 ]
then
    echo "$CACHE_DIR/z3 build z3-4.8.5found in cache."
else
    echo "z3 not found in cache or different version than z3-4.8.5"
    rm $CACHE_DIR/z3 -rf
    curl -L https://github.com/TorXakis/Dependencies/releases/download/z3-4.8.5/z3-4.8.5-x64-ubuntu-14.04.zip -o z3.zip
    unzip z3.zip
    Z3NAME=$(ls -d z3-*/)
    echo $Z3NAME
    mv $Z3NAME $CACHE_DIR/z3
    chmod +x $CACHE_DIR/z3/bin/z3
    touch $CACHE_DIR/z3/z3-4.8.5
fi
echo "Set up done"
