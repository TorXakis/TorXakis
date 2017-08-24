export PATH=$HOME/.local/z3/bin:$HOME/.local/bin:$PATH
stack install --install-ghc --test --haddock --no-haddock-deps --fast --stack-yaml stack_linux.yaml
cd test/sqatt && stack test && cd -
