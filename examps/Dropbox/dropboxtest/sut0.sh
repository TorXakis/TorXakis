# TorXakis - Model Based Testing
# Copyright (c) 2015-2020 TNO and Radboud University
# See LICENSE at root directory of this repository.
rm testdir/*
echo "$" > testdir/testfile
sleep 7
echo "sut0 ready"
socat -v tcp-l:7890 system:'cd .; bash'
