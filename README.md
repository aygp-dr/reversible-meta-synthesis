# Reversible Meta-Synthesis

An implementation of "Inductive Program Synthesis by Using a Reversible Meta-Interpreter" by Masayuki Numao and Masamichi Shimura (1990).

## Overview

This repository implements the concepts from the seminal paper on using reversible meta-interpreters for inductive program synthesis. The key innovations in this approach include:

1. A reversible interpreter that can both execute programs and synthesize them from examples
2. Explanation-based learning to accelerate program synthesis
3. Decomposition of explanations based on composability for flexible knowledge transfer

## Implementations

Four implementations are provided:
- **Prolog**: The original language used in the paper, providing the most faithful implementation
- **Clojure**: Modern Lisp on the JVM with rich data structures and concurrency support
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
2. Install the required languages:
   - **Clojure**: Install [Clojure CLI tools](https://clojure.org/guides/getting_started)
   - **Hy**: `pip install hy`
   - **Scheme**: Install [GNU Guile](https://www.gnu.org/software/guile/)
   - **Prolog**: Install [SWI-Prolog](https://www.swi-prolog.org/) (optional)

3. Test all implementations:
   ```bash
   ./test_all.sh
   ```

4. Or run examples individually:
   ```bash
   # Clojure
   clojure -M:run examples
   
   # Hy
   hy examples/hy/append_example.hy
   
   # Scheme
   guile examples/scheme/append-example.scm
   
   # Prolog (if available)
   swipl -q -t "test_append, halt" -f reversible-interpreter.pl
   ```

## Directory Structure

```
.
├── src/                           # Source code
│   ├── reversible_meta_synthesis/ # Clojure implementation  
│   ├── hy/                        # Hy implementation
│   ├── scheme/                    # Scheme implementation
│   ├── prolog/                    # Prolog implementation
│   └── api/                       # HTTP API server
├── examples/                      # Working examples
│   ├── clojure/
│   ├── hy/
│   ├── scheme/
│   └── prolog/
├── tests/                         # Test suites
├── reversible-interpreter.pl      # Standalone Prolog implementation
└── test_all.sh                   # Test runner for all languages
```

## References

Numao, M., & Shimura, M. (1990). Inductive Program Synthesis by Using a Reversible Meta-Interpreter. In M. Bruynooghe (Ed.), Proc. the Second Workshop on Meta-Programming in Logic, pp. 123-136, Leuven, Belgium.
