#!/bin/bash
if   [[ `netstat -n | grep 9876` == "" ]]; then
  PN=9876
elif [[ `netstat -n | grep 9875` == "" ]]; then
  PN=9875
elif [[ `netstat -n | grep 9874` == "" ]]; then
  PN=9874
elif [[ `netstat -n | grep 9873` == "" ]]; then
  PN=9873
elif [[ `netstat -n | grep 9872` == "" ]]; then
  PN=9872
elif [[ `netstat -n | grep 9871` == "" ]]; then
  PN=9871
fi
if [[ $PN == "" ]]; then
  echo "too many torxakis processes running"
else
  txsserver $PN > .txs.$PN.log 2> .txs.$PN.err.log &
  txsui $PN $*
fi
