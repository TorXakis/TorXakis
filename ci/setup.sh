sudo apt-get install -y libgmp3-dev
# Download and unpack the stack executable
mkdir -p ~/.local/bin
export PATH=$HOME/.local/bin:$PATH
curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
curl -L -O https://github.com/TorXakis/Dependencies/releases/download/v0.3.0_linux/z3-4.5.1.0f1583309d08-x64-ubuntu-14.04.zip -O https://github.com/TorXakis/Dependencies/releases/download/v0.3.0_linux/cvc4-1.5-x86_64-linux-opt
# rename cvc4 executable to get the build working todo: parametrize this in SMTInternal.hs
mv cvc4-1.5-x86_64-linux-opt cvc4-2017-06-13-win32-opt
mv cvc4-* ~/.local/bin
chmod +x ~/.local/bin/cvc4-2017-06-13-win32-opt
CURDIR=$(pwd)
mkdir ~/.local/z3 && cd ~/.local/z3
Z3ZIP=$(ls $CURDIR/z3*.zip)
unzip $Z3ZIP
chmod +x ./bin/z3
export PATH=$HOME/.local/z3/bin:$PATH
cd $CURDIR
