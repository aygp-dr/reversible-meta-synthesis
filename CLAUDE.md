# Reversible Meta-Synthesis Project Guidelines

## Build/Test Commands
- Setup: `pip install -e ".[dev]"`
- Run tests: `pytest`
- Run single test: `pytest path/to/test.py::test_function_name -v`
- Lint code: `ruff check .`
- Type check: `mypy .`

## Code Style Guidelines
- **Imports**: Group standard library, third-party, and local imports
- **Formatting**: Use Black with 88 character line length
- **Types**: Use type annotations for all functions and classes
- **Naming**: 
  - snake_case for variables and functions
  - PascalCase for classes
  - UPPER_CASE for constants
- **Documentation**: Google-style docstrings for all public functions
- **Error Handling**: Use explicit exception handling with specific exception types
- **Testing**: Write pytest-compatible tests with descriptive names

## Git Workflow
- **Issues**: Create GitHub issue before starting work with clear start/end/validation states
- **Commits**: Use conventional commit format (feat, fix, docs, refactor, test, etc.)
- **Attribution**: Use `--trailer "Signed-off-by: Name <email>"` instead of body text
- **Issue References**: Use `--trailer "Fixes: #issue_number"` to reference issues
- **Pull Requests**: Link to related issues and include validation steps

## Project Structure
Organize code into logical modules with clear separation of concerns. Follow a functional programming approach with immutable data structures where possible.