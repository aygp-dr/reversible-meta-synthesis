#!/bin/bash
# Test all language implementations

set -e

echo "=========================================="
echo "Testing Reversible Meta-Synthesis Project"
echo "=========================================="

echo
echo "=== Testing Clojure ==="
clojure -M:run examples

echo
echo "=== Testing Hy ==="
hy examples/hy/append_example.hy

echo
echo "=== Testing Scheme ==="
guile --no-auto-compile examples/scheme/append-example.scm

echo
echo "=== Testing Prolog (if available) ==="
if command -v swipl >/dev/null 2>&1; then
    swipl -q -t "test_append, halt" -f reversible-interpreter.pl
else
    echo "SWI-Prolog not available, skipping test"
fi

echo
echo "=========================================="
echo "All tests completed successfully!"
echo "=========================================="