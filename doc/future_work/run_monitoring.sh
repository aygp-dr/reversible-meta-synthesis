#!/bin/bash
# Script to set up and run monitoring for the Reversible Meta-Synthesis service

# Set environment variables
export PYTHONPATH=/opt/reversible-meta-synthesis
export WORKER_ID=${HOSTNAME}

# Configuration files
MONITOR_CONFIG=/etc/synthesis/monitoring_config.yaml
ALERTS_CONFIG=/etc/synthesis/alerts_config.yaml
DASHBOARD_CONFIG=/etc/synthesis/dashboard_config.yaml

# Create log directory
mkdir -p /var/log/synthesis

# Function to start a component
start_component() {
    echo "Starting $1..."
    if pgrep -f "python3 $2" > /dev/null; then
        echo "$1 is already running"
    else
        python3 $2 --config $3 $4 >> /var/log/synthesis/$1.log 2>&1 &
        echo "$1 started with PID $!"
    fi
}

# Function to stop a component
stop_component() {
    echo "Stopping $1..."
    pkill -f "python3 $2" || true
    echo "$1 stopped"
}

# Function to check component status
check_component() {
    if pgrep -f "python3 $1" > /dev/null; then
        echo "$2 is running"
    else
        echo "$2 is not running"
    fi
}

# Function to display help
display_help() {
    echo "Usage: $0 [start|stop|restart|status] [all|monitor|alert|dashboard]"
    echo ""
    echo "Commands:"
    echo "  start       Start monitoring components"
    echo "  stop        Stop monitoring components"
    echo "  restart     Restart monitoring components"
    echo "  status      Check status of monitoring components"
    echo ""
    echo "Components:"
    echo "  all         All monitoring components"
    echo "  monitor     Service monitor"
    echo "  alert       Alert manager"
    echo "  dashboard   Dashboard generator"
    echo ""
    echo "Examples:"
    echo "  $0 start all             # Start all monitoring components"
    echo "  $0 stop monitor          # Stop the service monitor"
    echo "  $0 restart alert         # Restart the alert manager"
    echo "  $0 status                # Check status of all components"
}

# Main script logic
case $1 in
    start)
        case $2 in
            all|"")
                start_component "service-monitor" "/opt/reversible-meta-synthesis/doc/future_work/monitor_synthesis_service.py" "$MONITOR_CONFIG" "--port 8000"
                start_component "alert-manager" "/opt/reversible-meta-synthesis/doc/future_work/alert_manager.py" "$ALERTS_CONFIG"
                start_component "dashboard-generator" "/opt/reversible-meta-synthesis/doc/future_work/dashboard_generator.py" "$DASHBOARD_CONFIG" "--action generate"
                ;;
            monitor)
                start_component "service-monitor" "/opt/reversible-meta-synthesis/doc/future_work/monitor_synthesis_service.py" "$MONITOR_CONFIG" "--port 8000"
                ;;
            alert)
                start_component "alert-manager" "/opt/reversible-meta-synthesis/doc/future_work/alert_manager.py" "$ALERTS_CONFIG"
                ;;
            dashboard)
                start_component "dashboard-generator" "/opt/reversible-meta-synthesis/doc/future_work/dashboard_generator.py" "$DASHBOARD_CONFIG" "--action generate"
                ;;
            *)
                echo "Unknown component: $2"
                display_help
                exit 1
                ;;
        esac
        ;;
    
    stop)
        case $2 in
            all|"")
                stop_component "service-monitor" "/opt/reversible-meta-synthesis/doc/future_work/monitor_synthesis_service.py"
                stop_component "alert-manager" "/opt/reversible-meta-synthesis/doc/future_work/alert_manager.py"
                stop_component "dashboard-generator" "/opt/reversible-meta-synthesis/doc/future_work/dashboard_generator.py"
                ;;
            monitor)
                stop_component "service-monitor" "/opt/reversible-meta-synthesis/doc/future_work/monitor_synthesis_service.py"
                ;;
            alert)
                stop_component "alert-manager" "/opt/reversible-meta-synthesis/doc/future_work/alert_manager.py"
                ;;
            dashboard)
                stop_component "dashboard-generator" "/opt/reversible-meta-synthesis/doc/future_work/dashboard_generator.py"
                ;;
            *)
                echo "Unknown component: $2"
                display_help
                exit 1
                ;;
        esac
        ;;
    
    restart)
        case $2 in
            all|"")
                $0 stop all
                sleep 2
                $0 start all
                ;;
            monitor|alert|dashboard)
                $0 stop $2
                sleep 2
                $0 start $2
                ;;
            *)
                echo "Unknown component: $2"
                display_help
                exit 1
                ;;
        esac
        ;;
    
    status)
        echo "Checking monitoring component status..."
        check_component "/opt/reversible-meta-synthesis/doc/future_work/monitor_synthesis_service.py" "Service monitor"
        check_component "/opt/reversible-meta-synthesis/doc/future_work/alert_manager.py" "Alert manager"
        check_component "/opt/reversible-meta-synthesis/doc/future_work/dashboard_generator.py" "Dashboard generator"
        ;;
    
    *)
        display_help
        exit 1
        ;;
esac

exit 0
