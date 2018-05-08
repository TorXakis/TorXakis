#!/bin/bash

# Install TorXakis.
# inside this we need to run this script:
apt-get update
cd root
apt-get install ./torxakis_0.6.0_amd64.deb -y

# Test with "Echo".
cat test/examps/Echo/Echo_Simulator.txscmd | torxakis examps/Echo/Echo.txs &> /dev/null &
cat test/examps/Echo/Echo_Tester.txscmd | torxakis examps/Echo/Echo.txs

# Test with "Moving Arms"
cat test/examps/MovingArms/MovingArms_Simulator.txscmd | \
    torxakis examps/MovingArms/MovingArms.txs examps/MovingArms/RestrictedAxisPurpose.txs \
    &> /dev/null &
cat test/examps/MovingArms/MovingArms_RestrictedAxisPurpose_eager3_Tester.txscmd | \
    torxakis examps/MovingArms/MovingArms.txs  examps/MovingArms/RestrictedAxisPurpose.txs
