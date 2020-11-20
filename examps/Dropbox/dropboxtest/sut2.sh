# TorXakis - Model Based Testing
# Copyright (c) 2015-2020 TNO and Radboud University
# See LICENSE at root directory of this repository.
sleep 7
echo "sut2 ready"
socat -v tcp-l:7892 system:'cd .; bash'
