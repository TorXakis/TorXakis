stack install --test --haddock --no-haddock-deps --fast --stack-yaml stack_linux.yaml
cd test/sqatt && stack test && cd -
