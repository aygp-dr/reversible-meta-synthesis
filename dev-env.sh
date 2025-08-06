#!/usr/bin/env bash
# Development environment launcher for reversible-meta-synthesis project
# Provides tmux session with Emacs configured for Scheme development

set -euo pipefail

# Load environment variables
if [ -f .envrc ]; then
    source .envrc
fi

# Configuration
PROJECT_NAME="${PROJECT_NAME:-reversible-meta-synthesis}"
PROJECT_ROOT="${PROJECT_ROOT:-$(pwd)}"
TMUX_SESSION="${PROJECT_TMUX_SESSION:-${PROJECT_NAME}-dev}"
EMACS_CONFIG="${EMACS_CONFIG:-${PROJECT_NAME}.el}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Functions
print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_dependencies() {
    local missing_deps=()
    
    for cmd in tmux emacs guile3; do
        if ! command -v "$cmd" &> /dev/null; then
            missing_deps+=("$cmd")
        fi
    done
    
    if [ ${#missing_deps[@]} -ne 0 ]; then
        print_error "Missing dependencies: ${missing_deps[*]}"
        print_info "Please install missing dependencies first."
        exit 1
    fi
}

start_tmux_session() {
    if tmux has-session -t "$TMUX_SESSION" 2>/dev/null; then
        print_warn "Session '$TMUX_SESSION' already exists"
        read -p "Attach to existing session? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            tmux attach-session -t "$TMUX_SESSION"
        else
            print_info "Session info:"
            tmux list-sessions -F "#{session_name}: #{session_windows} windows" | grep "^$TMUX_SESSION:"
            tmux list-panes -t "$TMUX_SESSION" -F "  Pane #{pane_index}: TTY=#{pane_tty}"
        fi
    else
        print_info "Creating new tmux session: $TMUX_SESSION"
        
        # Create tmux session with Emacs
        tmux new-session -d -s "$TMUX_SESSION" -n "emacs" \
            "cd $PROJECT_ROOT && emacs -nw -Q -l $EMACS_CONFIG"
        
        # Create additional windows
        tmux new-window -t "$TMUX_SESSION:2" -n "repl" \
            "cd $PROJECT_ROOT && guile3"
        
        tmux new-window -t "$TMUX_SESSION:3" -n "shell" \
            "cd $PROJECT_ROOT && bash"
        
        # Select first window
        tmux select-window -t "$TMUX_SESSION:1"
        
        # Display session information
        print_info "Session created successfully!"
        print_info "Windows:"
        print_info "  1. emacs - Main editor with Scheme support"
        print_info "  2. repl  - Guile REPL"
        print_info "  3. shell - Project shell"
        
        # Get TTY information
        TTY=$(tmux list-panes -t "$TMUX_SESSION:1" -F "#{pane_tty}")
        print_info "Emacs TTY: $TTY"
        
        # Attach to session
        tmux attach-session -t "$TMUX_SESSION"
    fi
}

stop_tmux_session() {
    if tmux has-session -t "$TMUX_SESSION" 2>/dev/null; then
        read -p "Stop session '$TMUX_SESSION'? (y/n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            tmux kill-session -t "$TMUX_SESSION"
            print_info "Session '$TMUX_SESSION' stopped"
        fi
    else
        print_warn "No session named '$TMUX_SESSION' found"
    fi
}

show_session_info() {
    if tmux has-session -t "$TMUX_SESSION" 2>/dev/null; then
        print_info "Session: $TMUX_SESSION"
        tmux list-windows -t "$TMUX_SESSION" -F "  Window #{window_index}: #{window_name}"
        echo
        print_info "TTY information:"
        tmux list-panes -t "$TMUX_SESSION" -a -F "  Window #{window_index}.#{pane_index}: TTY=#{pane_tty}"
    else
        print_warn "No session named '$TMUX_SESSION' found"
    fi
}

# Main script
main() {
    case "${1:-start}" in
        start)
            check_dependencies
            start_tmux_session
            ;;
        stop)
            stop_tmux_session
            ;;
        info)
            show_session_info
            ;;
        restart)
            stop_tmux_session
            start_tmux_session
            ;;
        *)
            echo "Usage: $0 {start|stop|restart|info}"
            echo
            echo "Commands:"
            echo "  start   - Start development environment (default)"
            echo "  stop    - Stop tmux session"
            echo "  restart - Restart tmux session"
            echo "  info    - Show session information"
            exit 1
            ;;
    esac
}

main "$@"