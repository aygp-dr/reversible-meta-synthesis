#!/bin/sh
set -e

echo "Installing dependencies for reversible-meta-synthesis on FreeBSD..."

# Prolog (SWI-Prolog)
if ! which swipl >/dev/null 2>&1; then
    echo "Installing SWI-Prolog..."
    pkg install -y swi-prolog
fi

# Hy (1.0.0)
if ! pip show hy | grep -q "Version: 1.0.0"; then
    echo "Installing Hy 1.0.0..."
    pkg install -y py39-pip
    pip install 'hy==1.0.0'
fi

# Scheme (Guile)
if ! which guile >/dev/null 2>&1; then
    echo "Installing Guile Scheme..."
    pkg install -y guile3
fi

echo "All dependencies installed successfully!"
