#!/bin/bash
set -e

echo "Starting the interoperability REST API server..."
python3 src/api/server.py &
SERVER_PID=$!

# Wait for server to start
sleep 2

echo "Running Prolog interoperability example..."
cd examples/interop
swipl -q -l prolog_client.pl

echo "Running Hy interoperability example..."
hy hy_client.hy

echo "Running Clojure interoperability example..."
cd ../..
clojure -M -m reversible-meta-synthesis.examples.interop-client

echo "Running Scheme interoperability example..."
cd examples/interop
guile scheme_client.scm

echo "Running interoperability tests..."
cd ../../tests/interop
swipl -q -l prolog_interop_test.pl -t "run_tests, halt"
hy hy_interop_test.hy
cd ../..
clojure -M:test -m reversible-meta-synthesis.interop-bridge-test
cd tests/interop
guile scheme_interop_test.scm

# Clean up
echo "Shutting down the server..."
kill $SERVER_PID
echo "Interoperability examples completed successfully!"
