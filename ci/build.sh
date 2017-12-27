# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

# Stack should be installed at this stage.
echo "Using stack `stack --version`"

stack install --pedantic --install-ghc --haddock --test --stack-yaml stack_linux.yaml --work-dir $CACHE_DIR_REL/.stack-work --stack-root $CACHE_DIR/.stack --allow-different-user
