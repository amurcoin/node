#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Options: $AMURCOIN_OPTS
java $AMURCOIN_OPTS -jar /opt/amurcoin/amurcoin.jar /opt/amurcoin/template.conf &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
