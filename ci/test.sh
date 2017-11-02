# TorXakis - Model Based Testing
# Copyright (c) 2015-2017 TNO and Radboud University
# See LICENSE at root directory of this repository.

cd test/sqatt && stack test sqatt:examples-test --test-arguments="--skip=#long --skip=#model --match=Hit" --work-dir $CACHE_DIR_REL/test/.stack_work --stack-root $CACHE_DIR/.stack --allow-different-user && cd -
