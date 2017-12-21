# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

cd test/sqatt && stack test --test-arguments="--match=#long" --work-dir $CACHE_DIR_REL/test/.stack_work --stack-root $CACHE_DIR/.stack --stack-yaml stack_linux.yaml --allow-different-user && cd -
