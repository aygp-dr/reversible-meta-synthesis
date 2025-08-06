# Makefile for reversible-meta-synthesis project

PROJECT_NAME := reversible-meta-synthesis
PROJECT_ROOT := $(shell pwd)
PROJECT_TMUX_SESSION := $(PROJECT_NAME)-dev
EMACS_CONFIG := $(PROJECT_NAME).el

# Language-specific directories
CLOJURE_SRC := src/reversible_meta_synthesis
HY_SRC := src/hy
SCHEME_SRC := src/scheme
PROLOG_SRC := src/prolog

# Default target
.PHONY: help
help:
	@echo "Reversible Meta-Synthesis Project"
	@echo "================================="
	@echo ""
	@echo "Available targets:"
	@echo "  make dev-env        - Start development environment with tmux and Emacs"
	@echo "  make test-all       - Run all tests"
	@echo "  make test-clojure   - Run Clojure tests"
	@echo "  make test-hy        - Run Hy tests"
	@echo "  make test-scheme    - Run Scheme tests"
	@echo "  make test-prolog    - Run Prolog tests"
	@echo "  make examples       - Run all examples"
	@echo "  make clean          - Clean generated files"
	@echo "  make stop-tmux      - Stop the tmux development session"

# Development environment
.PHONY: dev-env
dev-env: $(EMACS_CONFIG)
	@echo "Starting development environment..."
	@if tmux has-session -t $(PROJECT_TMUX_SESSION) 2>/dev/null; then \
		echo "Session $(PROJECT_TMUX_SESSION) already exists. Attaching..."; \
		tmux attach-session -t $(PROJECT_TMUX_SESSION); \
	else \
		tmux new-session -d -s $(PROJECT_TMUX_SESSION) "emacs -nw -Q -l $(EMACS_CONFIG)"; \
		echo "Created tmux session: $(PROJECT_TMUX_SESSION)"; \
		echo "TTY: $$(tmux list-panes -t $(PROJECT_TMUX_SESSION) -F '#{pane_tty}')"; \
		tmux attach-session -t $(PROJECT_TMUX_SESSION); \
	fi

# Stop tmux session
.PHONY: stop-tmux
stop-tmux:
	@if tmux has-session -t $(PROJECT_TMUX_SESSION) 2>/dev/null; then \
		tmux kill-session -t $(PROJECT_TMUX_SESSION); \
		echo "Stopped tmux session: $(PROJECT_TMUX_SESSION)"; \
	else \
		echo "No session named $(PROJECT_TMUX_SESSION) found"; \
	fi

# Get tmux TTY
.PHONY: tmux-tty
tmux-tty:
	@if tmux has-session -t $(PROJECT_TMUX_SESSION) 2>/dev/null; then \
		tmux list-panes -t $(PROJECT_TMUX_SESSION) -F "TTY: #{pane_tty}"; \
	else \
		echo "No session named $(PROJECT_TMUX_SESSION) found"; \
	fi

# Testing targets
.PHONY: test-all
test-all:
	./test_all.sh

.PHONY: test-clojure
test-clojure:
	clojure -M:run tests

.PHONY: test-hy
test-hy:
	hy tests/hy/run_tests.hy

.PHONY: test-scheme
test-scheme:
	guile tests/scheme/run-tests.scm

.PHONY: test-prolog
test-prolog:
	swipl -q -t "run_tests, halt" -f tests/prolog/run_tests.pl

# Examples
.PHONY: examples
examples: examples-clojure examples-hy examples-scheme examples-prolog

.PHONY: examples-clojure
examples-clojure:
	clojure -M:run examples

.PHONY: examples-hy
examples-hy:
	hy examples/hy/append_example.hy

.PHONY: examples-scheme
examples-scheme:
	guile examples/scheme/append-example.scm

.PHONY: examples-prolog
examples-prolog:
	swipl -q -t "test_append, halt" -f reversible-interpreter.pl

# Clean target
.PHONY: clean
clean:
	find . -name "*.pyc" -delete
	find . -name "__pycache__" -type d -delete
	find . -name ".cpcache" -type d -delete
	rm -f $(EMACS_CONFIG)