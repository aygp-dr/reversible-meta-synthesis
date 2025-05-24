#!/bin/bash
set -e

echo "Installing dependencies for reversible-meta-synthesis on Linux..."

# Prolog (SWI-Prolog)
if ! command -v swipl &> /dev/null; then
    echo "Installing SWI-Prolog..."
    sudo apt-get update
    sudo apt-get install -y swi-prolog
fi

# Hy (1.0.0)
if ! pip3 show hy | grep -q "Version: 1.0.0"; then
    echo "Installing Hy 1.0.0..."
    pip3 install 'hy==1.0.0'
fi

# Scheme (Guile)
if ! command -v guile &> /dev/null; then
    echo "Installing Guile Scheme..."
    sudo apt-get install -y guile-3.0
fi

echo "All dependencies installed successfully!"
