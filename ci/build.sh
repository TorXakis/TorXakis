# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

# Stack should be installed at this stage.

echo $PATH
echo "stack"
stack --version
echo "cvc4"
cvc4 --version
echo "z3"
z3 -version

stack install --pedantic --install-ghc --haddock --test --stack-yaml stack_linux.yaml --work-dir $CACHE_DIR_REL/.stack-work --stack-root $CACHE_DIR/.stack --allow-different-user

echo "git diff"
git diff
echo "more stack-linux.yaml.lock"
more stack-linux.yaml.lock