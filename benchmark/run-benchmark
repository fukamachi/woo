#!/bin/sh

BENCHMARK_DIR=$(dirname $0)

REPEAT=${REPEAT:-3}
SERVER_PORT=${SERVER_PORT:-5000}
THREADS=${THREADS:-4}
CONNECTIONS=${CONNECTIONS:-10}

echo "$ $@"
$@ >>"$BENCHMARK_DIR/benchmark.log" 2>&1 &
SERVER_PID=$!

while true; do
    nc -z 127.0.0.1 $SERVER_PORT >/dev/null 2>&1 && break
    sleep 1
done

echo "Started a server ($@) at $SERVER_PID."

for i in `seq 1 $REPEAT`; do
    echo "\nRunning wrk ($i/$REPEAT)..."
    wrk -c "$CONNECTIONS" -t "$THREADS" -d 10 "http://127.0.0.1:$SERVER_PORT"
done

kill "$SERVER_PID"
