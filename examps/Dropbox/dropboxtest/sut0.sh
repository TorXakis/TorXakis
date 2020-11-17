# TorXakis - Model Based Testing
# Copyright (c) 2015-2020 TNO and Radboud University
# See LICENSE at root directory of this repository.
rm testdir/* ; echo "$" | cat > testdir/testfile ; sleep 5  ; echo "sut0 ready:" 
socat tcp-l:7890 system:'cd .; bash'
