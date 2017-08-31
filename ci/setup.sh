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

if [ -f $CACHE_DIR/bin/cvc4-2017-06-13-win32-opt ]
then
    echo "$CACHE_DIR/bin/cvc4-2017-06-13-win32-opt found in cache."
else
    curl -L -O https://github.com/TorXakis/Dependencies/releases/download/v0.3.0_linux/cvc4-1.5-x86_64-linux-opt
    # rename cvc4 executable to get the build working todo: parametrize this in SMTInternal.hs
    mv cvc4-1.5-x86_64-linux-opt cvc4-2017-06-13-win32-opt
    mv cvc4-* $CACHE_DIR/bin
    chmod +x $CACHE_DIR/bin/cvc4-2017-06-13-win32-opt
fi

if [ -d $CACHE_DIR/z3 ]
then
    echo "$CACHE_DIR/z3 found in cache."
else
    curl -L -O https://github.com/TorXakis/Dependencies/releases/download/v0.3.0_linux/z3-4.5.1.0f1583309d08-x64-ubuntu-14.04.zip
    CURDIR=$(pwd)
    mkdir $CACHE_DIR/z3 && cd $CACHE_DIR/z3
    Z3ZIP=$(ls $CURDIR/z3*.zip)
    unzip $Z3ZIP
    chmod +x ./bin/z3
    cd $CURDIR
fi
