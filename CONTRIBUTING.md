# Contributing to Reversible Meta-Synthesis

Thank you for considering contributing to this project! This document outlines the process for contributing and guidelines to follow.

## How to Contribute

1. **Fork the repository**
2. **Create a new branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. **Make your changes**
4. **Run tests**
   ```bash
   # For Prolog
   cd tests/prolog
   swipl -s run_tests.pl
   
   # For Hy
   cd tests/hy
   hy run_tests.hy
   
   # For Scheme
   cd tests/scheme
   guile run-tests.scm
   ```
5. **Push your changes to your fork**
6. **Create a pull request**

## Code Style Guidelines

### Prolog
- Use meaningful predicate and variable names
- Document predicates with comments indicating their purpose and arguments
- Follow the style used in the existing codebase

### Hy
- Follow PEP 8 conventions where applicable
- Use Lisp-style naming conventions (kebab-case) for functions and variables
- Document functions with docstrings

### Scheme
- Follow Scheme coding conventions (kebab-case)
- Keep functions small and focused
- Document functions with comments

## Testing

- Add tests for all new functionality
- Ensure all existing tests pass before submitting a pull request
- Include examples demonstrating new functionality

## Documentation

- Update documentation to reflect any changes in API or functionality
- Document any new features or examples
- Ensure diagrams are up-to-date with the current implementation

## Reporting Issues

If you find a bug or have a suggestion for improvement:

1. Check if the issue already exists in the issue tracker
2. If not, create a new issue with:
   - A clear title and description
   - Steps to reproduce the issue
   - Expected behavior
   - Actual behavior
   - Any error messages or logs

## License

By contributing to this project, you agree that your contributions will be licensed under the same license as the project.
