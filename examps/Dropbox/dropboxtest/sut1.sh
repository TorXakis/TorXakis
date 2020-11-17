# TorXakis - Model Based Testing
# Copyright (c) 2015-2020 TNO and Radboud University
# See LICENSE at root directory of this repository.
sleep 7  ; echo "sut1 ready" 
socat tcp-l:7891 system:'cd .; bash'
