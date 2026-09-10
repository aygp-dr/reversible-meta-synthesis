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

# Toolchains (overridable for portability / CI)
CLOJURE ?= clojure
HY      ?= hy
GUILE   ?= guile
SWIPL   ?= swipl
PYTHON  ?= python3

# Default target
.PHONY: help
help:
	@echo "Reversible Meta-Synthesis Project"
	@echo "================================="
	@echo ""
	@echo "Available targets:"
	@echo "  make dev-env        - Start development environment with tmux and Emacs"
	@echo "  make deps           - Resolve Clojure deps (download only)"
	@echo "  make install        - Alias for deps"
	@echo "  make test           - Run every unit suite (skips absent toolchains)"
	@echo "  make test-clojure   - Run Clojure unit tests"
	@echo "  make test-hy        - Run Hy unit tests"
	@echo "  make test-scheme    - Run Scheme unit tests"
	@echo "  make test-prolog    - Run Prolog unit tests"
	@echo "  make test-all       - Run example programs in every language (NOT the unit suites)"
	@echo "  make examples       - Run all examples"
	@echo "  make clean          - Clean generated files"
	@echo "  make stop-tmux      - Stop the tmux development session"

# ---------------------------------------------------------------------------
# Org-standard build interface: deps / install / test
# ---------------------------------------------------------------------------

# Resolve Clojure dependencies without running anything (CI cache warmup).
.PHONY: deps
deps:
	$(CLOJURE) -P -M:test

# `install` aliases `deps`; this project builds no installable artifact.
.PHONY: install
install: deps

# Canonical entry point: run the real unit suites (never the examples).
# Each suite is skipped with a notice when its toolchain is absent so a
# partial environment does not hard-fail, but genuine test failures still
# propagate a non-zero exit.
.PHONY: test
test:
	@status=0; \
	echo "== Clojure =="; \
	if command -v $(CLOJURE) >/dev/null 2>&1; then $(MAKE) --no-print-directory test-clojure || status=1; \
	else echo "SKIP: $(CLOJURE) not installed"; fi; \
	echo "== Hy =="; \
	if command -v $(HY) >/dev/null 2>&1; then $(MAKE) --no-print-directory test-hy || status=1; \
	else echo "SKIP: $(HY) not installed"; fi; \
	echo "== Scheme =="; \
	if command -v $(GUILE) >/dev/null 2>&1; then $(MAKE) --no-print-directory test-scheme || status=1; \
	else echo "SKIP: $(GUILE) not installed"; fi; \
	echo "== Prolog =="; \
	if command -v $(SWIPL) >/dev/null 2>&1; then $(MAKE) --no-print-directory test-prolog || status=1; \
	else echo "SKIP: $(SWIPL) not installed"; fi; \
	exit $$status

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
# NOTE: test-all runs the EXAMPLE programs (a smoke test), not the unit
# suites. Use `make test` for the real unit suites.
.PHONY: test-all
test-all:
	./test_all.sh

.PHONY: test-clojure
test-clojure:
	bb test

.PHONY: test-hy
test-hy:
	PYTHONPATH=$(PROJECT_ROOT) $(HY) tests/hy/run_tests.hy

.PHONY: test-scheme
test-scheme:
	@if command -v $(GUILE) >/dev/null 2>&1; then \
		$(GUILE) --no-auto-compile tests/scheme/run-tests.scm; \
	else \
		echo "SKIP: $(GUILE) not installed"; \
	fi

.PHONY: test-prolog
test-prolog:
	@if command -v $(SWIPL) >/dev/null 2>&1; then \
		$(SWIPL) -q -t "run_tests, halt" -f tests/prolog/run_tests.pl; \
	else \
		echo "SKIP: $(SWIPL) not installed"; \
	fi

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
