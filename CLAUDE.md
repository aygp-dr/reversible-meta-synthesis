# Reversible Meta-Synthesis Project Guidelines

## Build/Test Commands
- **Clojure**: `clojure -M:run [examples|tests|synthesis]`
- **Hy**: `hy examples/hy/append_example.hy`
- **Scheme**: `guile examples/scheme/append-example.scm`  
- **Prolog**: `swipl -q -t "test_append, halt" -f reversible-interpreter.pl` (if available)

## Code Style Guidelines
- **Clojure**: Follow standard Clojure conventions, use kebab-case for names
- **Hy**: Follow Lisp conventions with hyphenated names
- **Scheme**: Follow R5RS/R6RS conventions
- **Prolog**: Follow ISO Prolog conventions
- **Documentation**: Include docstrings/comments explaining the reversible interpreter concepts
- **Testing**: Each language has its own test structure in tests/ directory

## Git Workflow
- **Issues**: Create GitHub issue before starting work with clear start/end/validation states
- **Commits**: Use conventional commit format (feat, fix, docs, refactor, test, etc.)
- **Attribution**: Use `--trailer "Signed-off-by: Name <email>"` instead of body text
- **Issue References**: Use `--trailer "Fixes: #issue_number"` to reference issues
- **Pull Requests**: Link to related issues and include validation steps

## Project Structure
This is a multi-language implementation of a reversible meta-interpreter:

- `src/reversible_meta_synthesis/` - Clojure implementation
- `src/hy/` - Hy (Lisp-on-Python) implementation  
- `src/scheme/` - Scheme implementation
- `src/prolog/` - Prolog implementation
- `examples/` - Working examples for each language
- `tests/` - Test suites for each language
- `reversible-interpreter.pl` - Standalone Prolog implementation