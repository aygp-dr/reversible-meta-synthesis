#!/bin/bash
set -e

echo "Installing dependencies for reversible-meta-synthesis on macOS..."

# Check for Homebrew
if ! command -v brew &> /dev/null; then
    echo "Homebrew not found. Installing..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Prolog (SWI-Prolog)
if ! command -v swipl &> /dev/null; then
    echo "Installing SWI-Prolog..."
    brew install swi-prolog
fi

# Hy (1.0.0)
if ! pip3 show hy | grep -q "Version: 1.0.0"; then
    echo "Installing Hy 1.0.0..."
    pip3 install 'hy==1.0.0'
fi

# Scheme (Guile)
if ! command -v guile &> /dev/null; then
    echo "Installing Guile Scheme..."
    brew install guile
fi

echo "All dependencies installed successfully!"
