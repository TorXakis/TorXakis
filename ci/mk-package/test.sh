#!/bin/bash

# Test that the the `TorXakis` `.deb` package can be installed and run in different ubuntu versions.

versions=("17.10" "16.04" "18.04")

for v in "${versions[@]}"
do
   echo
   echo "Testing with Ubuntu: $v"
   echo
    
   # Test with the given ubuntu version
   docker run --rm -v  $(pwd):/root --entrypoint "/root/ci/mk-package/install-test.sh" -it ubuntu:${v}
done



