# Reversible Meta-Synthesis

An implementation of "Inductive Program Synthesis by Using a Reversible Meta-Interpreter" by Masayuki Numao and Masamichi Shimura (1990).

## Overview

This repository implements the concepts from the seminal paper on using reversible meta-interpreters for inductive program synthesis. The key innovations in this approach include:

1. A reversible interpreter that can both execute programs and synthesize them from examples
2. Explanation-based learning to accelerate program synthesis
3. Decomposition of explanations based on composability for flexible knowledge transfer

## Implementations

Three implementations are provided:
- **Prolog**: The original language used in the paper, providing the most faithful implementation
- **Hy**: A Lisp dialect embedded in Python, bridging functional and imperative paradigms
- **Scheme**: A minimalist Lisp implementation focusing on core concepts

## Key Examples

The repository includes implementations of several examples from the paper:
- Basic append program synthesis
- Synthesizing merge3 from app3
- Synthesizing rzip from zip
- Synthesizing fullrev from reverse and flatten

## Getting Started

1. Clone this repository
2. Run the appropriate installation script for your platform:
   ```
   # Linux
   ./install-linux.sh
   
   # FreeBSD
   ./install-freebsd.sh
   
   # macOS
   ./install-macos.sh
   ```
3. Explore the examples in the `examples` directory

## Directory Structure

```
.
├── doc/                  # Documentation
├── examples/             # Example programs
│   ├── prolog/
│   ├── hy/
│   └── scheme/
├── src/                  # Source code
│   ├── prolog/
│   ├── hy/
│   └── scheme/
└── tests/                # Tests
    ├── prolog/
    ├── hy/
    └── scheme/
```

## References

Numao, M., & Shimura, M. (1990). Inductive Program Synthesis by Using a Reversible Meta-Interpreter. In M. Bruynooghe (Ed.), Proc. the Second Workshop on Meta-Programming in Logic, pp. 123-136, Leuven, Belgium.
