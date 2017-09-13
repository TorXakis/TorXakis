export PATH=$HOME/.local/bin:$CACHE_DIR/z3/bin:$CACHE_DIR/bin:$PATH
stack install --install-ghc --haddock --test --fast --stack-yaml stack_linux.yaml --work-dir $CACHE_DIR_REL/.stack-work --stack-root $CACHE_DIR/.stack --allow-different-user
