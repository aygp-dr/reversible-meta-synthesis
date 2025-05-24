#!/usr/bin/env python3
"""
Alert manager for the Reversible Meta-Synthesis service.
"""

import time
import logging
import json
import yaml
import argparse
import threading
import datetime
import requests
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from typing import Dict, List, Any, Optional

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('/var/log/synthesis-alerts.log')
    ]
)
logger = logging.getLogger("synthesis-alerts")

class AlertManager:
    """
    Alert manager for the Reversible Meta-Synthesis service.
    """
    
    def __init__(self, config_file: str):
        """
        Initialize the alert manager
        
        Args:
            config_file: Path to the configuration file
        """
        # Load configuration
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        
        # Initialize alert state
        self.active_alerts = {}
        self.resolved_alerts = {}
        
        # Initialize notification handlers
        self._init_notification_handlers()
        
        # Initialize monitoring state
        self.running = False
        self.monitor_thread = None
    
    def _init_notification_handlers(self):
        """Initialize notification handlers"""
        self.notification_handlers = {
            'email': self._send_email_notification,
            'slack': self._send_slack_notification,
            'pagerduty': self._send_pagerduty_notification
        }
    
    def start(self):
        """Start the alert manager"""
        if self.running:
            logger.warning("Alert manager is already running")
            return
        
        # Start monitoring thread
        self.running = True
        self.monitor_thread = threading.Thread(target=self._monitor_loop)
        self.monitor_thread.daemon = True
        self.monitor_thread.start()
        
        logger.info("Alert manager started")
    
    def stop(self):
        """Stop the alert manager"""
        self.running = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=10)
        logger.info("Alert manager stopped")
    
    def _monitor_loop(self):
        """Main monitoring loop"""
        while self.running:
            try:
                # Check for new alerts
                self._check_alerts()
                
                # Sleep before next iteration
                time.sleep(30)  # 30 seconds
            
            except Exception as e:
                logger.error(f"Error in monitoring loop: {e}", exc_info=True)
                time.sleep(60)  # Wait longer after error
    
    def _check_alerts(self):
        """Check for new alerts from Prometheus"""
        try:
            # Get alerts from Prometheus Alertmanager API
            alertmanager_url = self.config.get('alertmanager', {}).get('url', 'http://localhost:9093')
            response = requests.get(f"{alertmanager_url}/api/v2/alerts")
            
            if response.status_code == 200:
                current_alerts = response.json()
                
                # Process active alerts
                for alert in current_alerts:
                    alert_id = alert.get('fingerprint')
                    if alert_id not in self.active_alerts:
                        # New alert
                        self._handle_new_alert(alert)
                    else:
                        # Update existing alert
                        self._update_alert(alert)
                
                # Check for resolved alerts
                current_alert_ids = {alert.get('fingerprint') for alert in current_alerts}
                for alert_id in list(self.active_alerts.keys()):
                    if alert_id not in current_alert_ids:
                        # Alert has been resolved
                        self._handle_resolved_alert(self.active_alerts[alert_id])
        
        except Exception as e:
            logger.error(f"Error checking alerts: {e}")
    
    def _handle_new_alert(self, alert: Dict[str, Any]):
        """
        Handle a new alert
        
        Args:
            alert: Alert data
        """
        alert_id = alert.get('fingerprint')
        logger.info(f"New alert: {alert.get('labels', {}).get('alertname')} ({alert_id})")
        
        # Store alert
        self.active_alerts[alert_id] = alert
        
        # Send notifications
        self._send_alert_notifications(alert)
    
    def _update_alert(self, alert: Dict[str, Any]):
        """
        Update an existing alert
        
        Args:
            alert: Alert data
        """
        alert_id = alert.get('fingerprint')
        previous_alert = self.active_alerts.get(alert_id)
        
        # Check for severity changes
        if previous_alert and (
            previous_alert.get('labels', {}).get('severity') != 
            alert.get('labels', {}).get('severity')
        ):
            logger.info(f"Alert severity changed for {alert.get('labels', {}).get('alertname')} ({alert_id})")
            
            # Update alert
            self.active_alerts[alert_id] = alert
            
            # Send notifications for severity change
            self._send_alert_notifications(
                alert, 
                subject_prefix="[UPDATED]", 
                message_prefix="Alert severity has changed:"
            )
        else:
            # Just update the alert data
            self.active_alerts[alert_id] = alert
    
    def _handle_resolved_alert(self, alert: Dict[str, Any]):
        """
        Handle a resolved alert
        
        Args:
            alert: Alert data
        """
        alert_id = alert.get('fingerprint')
        logger.info(f"Resolved alert: {alert.get('labels', {}).get('alertname')} ({alert_id})")
        
        # Move from active to resolved
        self.resolved_alerts[alert_id] = alert
        del self.active_alerts[alert_id]
        
        # Send resolved notifications
        self._send_resolved_notifications(alert)
    
    def _send_alert_notifications(self, alert: Dict[str, Any], subject_prefix: str = "", message_prefix: str = ""):
        """
        Send notifications for an alert
        
        Args:
            alert: Alert data
            subject_prefix: Prefix for notification subject
            message_prefix: Prefix for notification message
        """
        severity = alert.get('labels', {}).get('severity', 'unknown')
        
        # Get notification config based on severity
        notification_config = self.config.get('notifications', {}).get(severity, {})
        
        if not notification_config:
            # Use default if no specific config for this severity
            notification_config = self.config.get('notifications', {}).get('default', {})
        
        # Prepare notification content
        alert_name = alert.get('labels', {}).get('alertname', 'Unknown Alert')
        instance = alert.get('labels', {}).get('instance', 'unknown')
        summary = alert.get('annotations', {}).get('summary', 'No summary available')
        description = alert.get('annotations', {}).get('description', 'No description available')
        
        subject = f"{subject_prefix} {severity.upper()} ALERT: {alert_name} on {instance}"
        
        message = f"{message_prefix}\n\n" if message_prefix else ""
        message += f"Alert: {alert_name}\n"
        message += f"Severity: {severity.upper()}\n"
        message += f"Instance: {instance}\n"
        message += f"Summary: {summary}\n"
        message += f"Description: {description}\n\n"
        
        # Add other labels
        message += "Additional Information:\n"
        for label, value in alert.get('labels', {}).items():
            if label not in ['alertname', 'severity', 'instance']:
                message += f"- {label}: {value}\n"
        
        # Send notifications using configured methods
        for method in notification_config.get('methods', []):
            if method in self.notification_handlers:
                self.notification_handlers[method](subject, message, severity)
            else:
                logger.warning(f"Unknown notification method: {method}")
    
    def _send_resolved_notifications(self, alert: Dict[str, Any]):
        """
        Send notifications for a resolved alert
        
        Args:
            alert: Alert data
        """
        severity = alert.get('labels', {}).get('severity', 'unknown')
        
        # Get notification config based on severity
        notification_config = self.config.get('notifications', {}).get(severity, {})
        
        if not notification_config:
            # Use default if no specific config for this severity
            notification_config = self.config.get('notifications', {}).get('default', {})
        
        # Check if we should send resolved notifications
        if not notification_config.get('send_resolved', False):
            return
        
        # Prepare notification content
        alert_name = alert.get('labels', {}).get('alertname', 'Unknown Alert')
        instance = alert.get('labels', {}).get('instance', 'unknown')
        
        subject = f"RESOLVED: {alert_name} on {instance}"
        
        message = f"The following alert has been resolved:\n\n"
        message += f"Alert: {alert_name}\n"
        message += f"Severity: {severity.upper()}\n"
        message += f"Instance: {instance}\n"
        
        # Send notifications using configured methods
        for method in notification_config.get('methods', []):
            if method in self.notification_handlers:
                self.notification_handlers[method](subject, message, severity, resolved=True)
            else:
                logger.warning(f"Unknown notification method: {method}")
    
    def _send_email_notification(self, subject: str, message: str, severity: str, resolved: bool = False):
        """
        Send email notification
        
        Args:
            subject: Email subject
            message: Email message
            severity: Alert severity
            resolved: Whether this is a resolved notification
        """
        try:
            email_config = self.config.get('email', {})
            if not email_config:
                logger.warning("Email notification requested but no email configuration found")
                return
            
            smtp_host = email_config.get('smtp_host')
            smtp_port = email_config.get('smtp_port', 587)
            smtp_user = email_config.get('smtp_user')
            smtp_password = email_config.get('smtp_password')
            
            from_email = email_config.get('from_email')
            
            # Get recipients based on severity
            recipients = email_config.get('recipients', {}).get(severity, [])
            if not recipients:
                # Use default recipients if no specific ones for this severity
                recipients = email_config.get('recipients', {}).get('default', [])
            
            if not recipients:
                logger.warning(f"No email recipients configured for severity: {severity}")
                return
            
            # Create email
            email = MIMEMultipart()
            email['Subject'] = subject
            email['From'] = from_email
            email['To'] = ', '.join(recipients)
            
            # Add body
            email.attach(MIMEText(message, 'plain'))
            
            # Send email
            with smtplib.SMTP(smtp_host, smtp_port) as server:
                if email_config.get('use_tls', True):
                    server.starttls()
                
                if smtp_user and smtp_password:
                    server.login(smtp_user, smtp_password)
                
                server.send_message(email)
            
            logger.info(f"Sent email notification to {len(recipients)} recipients")
        
        except Exception as e:
            logger.error(f"Error sending email notification: {e}", exc_info=True)
    
    def _send_slack_notification(self, subject: str, message: str, severity: str, resolved: bool = False):
        """
        Send Slack notification
        
        Args:
            subject: Notification subject
            message: Notification message
            severity: Alert severity
            resolved: Whether this is a resolved notification
        """
        try:
            slack_config = self.config.get('slack', {})
            if not slack_config:
                logger.warning("Slack notification requested but no Slack configuration found")
                return
            
            webhook_url = slack_config.get('webhook_url')
            if not webhook_url:
                logger.warning("No Slack webhook URL configured")
                return
            
            # Determine channel based on severity
            channel = slack_config.get('channels', {}).get(severity)
            if not channel:
                # Use default channel if no specific one for this severity
                channel = slack_config.get('channels', {}).get('default')
            
            # Determine color based on severity and resolved status
            color = '#00FF00' if resolved else {
                'critical': '#FF0000',
                'warning': '#FFFF00',
                'info': '#0000FF'
            }.get(severity, '#808080')
            
            # Create Slack message
            slack_message = {
                'channel': channel,
                'username': slack_config.get('username', 'Synthesis Alert Bot'),
                'icon_emoji': slack_config.get('icon_emoji', ':robot_face:'),
                'attachments': [
                    {
                        'fallback': subject,
                        'color': color,
                        'title': subject,
                        'text': message,
                        'fields': [
                            {
                                'title': 'Severity',
                                'value': severity.upper(),
                                'short': True
                            },
                            {
                                'title': 'Status',
                                'value': 'Resolved' if resolved else 'Active',
                                'short': True
                            }
                        ],
                        'footer': f"Synthesis Alert Manager • {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}"
                    }
                ]
            }
            
            # Send to Slack
            response = requests.post(
                webhook_url,
                json=slack_message,
                headers={'Content-Type': 'application/json'}
            )
            
            if response.status_code != 200:
                logger.warning(f"Failed to send Slack notification: {response.text}")
            else:
                logger.info("Sent Slack notification")
        
        except Exception as e:
            logger.error(f"Error sending Slack notification: {e}", exc_info=True)
    
    def _send_pagerduty_notification(self, subject: str, message: str, severity: str, resolved: bool = False):
        """
        Send PagerDuty notification
        
        Args:
            subject: Notification subject
            message: Notification message
            severity: Alert severity
            resolved: Whether this is a resolved notification
        """
        try:
            pagerduty_config = self.config.get('pagerduty', {})
            if not pagerduty_config:
                logger.warning("PagerDuty notification requested but no PagerDuty configuration found")
                return
            
            integration_key = pagerduty_config.get('integration_key')
            if not integration_key:
                logger.warning("No PagerDuty integration key configured")
                return
            
            # Only send critical alerts to PagerDuty by default
            if severity != 'critical' and not pagerduty_config.get('send_non_critical', False):
                logger.info(f"Skipping PagerDuty notification for non-critical alert: {severity}")
                return
            
            # Map severity to PagerDuty severity
            pd_severity = {
                'critical': 'critical',
                'warning': 'warning',
                'info': 'info'
            }.get(severity, 'warning')
            
            # Create unique incident key from alert name and instance
            alert_name = subject.split(':')[1].strip() if ':' in subject else subject
            incident_key = f"synthesis-alert-{alert_name}"
            
            # Create PagerDuty event
            pd_event = {
                'routing_key': integration_key,
                'event_action': 'resolve' if resolved else 'trigger',
                'dedup_key': incident_key,
                'payload': {
                    'summary': subject,
                    'severity': pd_severity,
                    'source': 'Synthesis Alert Manager',
                    'custom_details': {
                        'message': message
                    }
                }
            }
            
            # Send to PagerDuty
            response = requests.post(
                'https://events.pagerduty.com/v2/enqueue',
                json=pd_event,
                headers={'Content-Type': 'application/json'}
            )
            
            if response.status_code != 202:
                logger.warning(f"Failed to send PagerDuty notification: {response.text}")
            else:
                logger.info(f"Sent PagerDuty {'resolution' if resolved else 'alert'}")
        
        except Exception as e:
            logger.error(f"Error sending PagerDuty notification: {e}", exc_info=True)
    
    def send_custom_alert(self, alert_name: str, severity: str, instance: str, summary: str, description: str, labels: Dict[str, str] = None):
        """
        Send a custom alert
        
        Args:
            alert_name: Name of the alert
            severity: Alert severity (critical, warning, info)
            instance: Instance affected
            summary: Alert summary
            description: Alert description
            labels: Additional labels
        """
        # Create alert data
        alert = {
            'fingerprint': f"custom-{alert_name}-{instance}-{int(time.time())}",
            'labels': {
                'alertname': alert_name,
                'severity': severity,
                'instance': instance,
                **(labels or {})
            },
            'annotations': {
                'summary': summary,
                'description': description
            },
            'startsAt': datetime.datetime.now(datetime.timezone.utc).isoformat()
        }
        
        # Handle like a new alert
        self._handle_new_alert(alert)
        
        return alert['fingerprint']
    
    def resolve_custom_alert(self, alert_id: str):
        """
        Resolve a custom alert
        
        Args:
            alert_id: ID of the alert to resolve
        """
        if alert_id in self.active_alerts:
            alert = self.active_alerts[alert_id]
            self._handle_resolved_alert(alert)
            return True
        else:
            logger.warning(f"Attempted to resolve non-existent alert: {alert_id}")
            return False

# Example usage
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Alert manager for Reversible Meta-Synthesis service')
    parser.add_argument('--config', type=str, default='alerts_config.yaml', help='Path to configuration file')
    args = parser.parse_args()
    
    alert_manager = AlertManager(args.config)
    
    try:
        # Start alert manager
        alert_manager.start()
        
        # Example of sending a custom alert
        if False:  # Set to True to test
            alert_id = alert_manager.send_custom_alert(
                alert_name="TestAlert",
                severity="warning",
                instance="test-instance",
                summary="Test alert for demonstration",
                description="This is a test alert to demonstrate the alert manager's functionality."
            )
            
            print(f"Sent test alert with ID: {alert_id}")
            
            # Resolve after 30 seconds
            time.sleep(30)
            alert_manager.resolve_custom_alert(alert_id)
            print(f"Resolved test alert with ID: {alert_id}")
        
        # Keep running until interrupted
        while True:
            time.sleep(1)
    
    except KeyboardInterrupt:
        alert_manager.stop()
