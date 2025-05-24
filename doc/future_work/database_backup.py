#!/usr/bin/env python3
"""
Automated backup and disaster recovery for the Reversible Meta-Synthesis database.
"""

import os
import sys
import time
import logging
import subprocess
import datetime
import boto3
import argparse
import json
import yaml
from typing import Dict, List, Optional

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('/var/log/synthesis-db-backup.log')
    ]
)
logger = logging.getLogger("synthesis-db-backup")

class DatabaseBackupManager:
    """
    Manages automated backups and disaster recovery for the synthesis database.
    """
    
    def __init__(self, config_file: str):
        """
        Initialize the backup manager
        
        Args:
            config_file: Path to the configuration file
        """
        # Load configuration
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        
        # Create S3 client for backup storage
        self.s3 = boto3.client(
            's3',
            aws_access_key_id=self.config['s3']['access_key'],
            aws_secret_access_key=self.config['s3']['secret_key'],
            region_name=self.config['s3']['region']
        )
        
        # Create SNS client for notifications
        self.sns = boto3.client(
            'sns',
            aws_access_key_id=self.config['s3']['access_key'],
            aws_secret_access_key=self.config['s3']['secret_key'],
            region_name=self.config['s3']['region']
        )
        
        # Validate configuration
        self._validate_config()
        
        # Create backup directory if it doesn't exist
        os.makedirs(self.config['local']['backup_dir'], exist_ok=True)
    
    def _validate_config(self):
        """Validate the configuration"""
        required_keys = [
            's3.bucket',
            's3.prefix',
            's3.region',
            'database.host',
            'database.port',
            'database.name',
            'database.user',
            'database.password',
            'local.backup_dir',
            'retention.daily',
            'retention.weekly',
            'retention.monthly'
        ]
        
        for key in required_keys:
            parts = key.split('.')
            config = self.config
            for part in parts:
                if part not in config:
                    raise ValueError(f"Missing required configuration key: {key}")
                config = config[part]
    
    def perform_backup(self, backup_type: str = 'daily'):
        """
        Perform a database backup
        
        Args:
            backup_type: Type of backup (daily, weekly, monthly)
        """
        logger.info(f"Starting {backup_type} backup of {self.config['database']['name']}")
        
        try:
            # Create backup filename
            timestamp = datetime.datetime.now().strftime('%Y%m%d_%H%M%S')
            backup_file = os.path.join(
                self.config['local']['backup_dir'], 
                f"{self.config['database']['name']}_{backup_type}_{timestamp}.sql.gz"
            )
            
            # Create connection string for pg_dump
            conn_string = f"postgresql://{self.config['database']['user']}:{self.config['database']['password']}@{self.config['database']['host']}:{self.config['database']['port']}/{self.config['database']['name']}"
            
            # Run pg_dump to create backup
            logger.info(f"Creating backup file: {backup_file}")
            result = subprocess.run(
                [
                    'pg_dump',
                    '--format=custom',
                    '--compress=9',
                    '--file', backup_file,
                    conn_string
                ],
                capture_output=True,
                text=True
            )
            
            if result.returncode != 0:
                raise Exception(f"pg_dump failed: {result.stderr}")
            
            # Upload to S3
            s3_key = f"{self.config['s3']['prefix']}/{backup_type}/{os.path.basename(backup_file)}"
            logger.info(f"Uploading backup to S3: s3://{self.config['s3']['bucket']}/{s3_key}")
            
            self.s3.upload_file(
                backup_file,
                self.config['s3']['bucket'],
                s3_key,
                ExtraArgs={
                    'ServerSideEncryption': 'AES256',
                    'Metadata': {
                        'backup-type': backup_type,
                        'database': self.config['database']['name'],
                        'timestamp': timestamp
                    }
                }
            )
            
            # Remove local backup file if configured
            if not self.config.get('local', {}).get('keep_local', False):
                logger.info(f"Removing local backup file: {backup_file}")
                os.remove(backup_file)
            
            # Apply retention policy
            self._apply_retention_policy(backup_type)
            
            # Send success notification
            self._send_notification(
                f"Database backup successful: {self.config['database']['name']}",
                f"Successfully completed {backup_type} backup of {self.config['database']['name']} database.\n"
                f"Backup file: s3://{self.config['s3']['bucket']}/{s3_key}\n"
                f"Timestamp: {timestamp}"
            )
            
            logger.info(f"Backup completed successfully: {s3_key}")
            return True
        
        except Exception as e:
            logger.error(f"Backup failed: {e}", exc_info=True)
            
            # Send failure notification
            self._send_notification(
                f"Database backup failed: {self.config['database']['name']}",
                f"Failed to complete {backup_type} backup of {self.config['database']['name']} database.\n"
                f"Error: {str(e)}"
            )
            
            return False
    
    def _apply_retention_policy(self, backup_type: str):
        """
        Apply retention policy to backups
        
        Args:
            backup_type: Type of backup (daily, weekly, monthly)
        """
        logger.info(f"Applying retention policy for {backup_type} backups")
        
        # Get retention period for this backup type
        retention_days = self.config['retention'].get(backup_type, 7)
        
        # List backups
        prefix = f"{self.config['s3']['prefix']}/{backup_type}/"
        response = self.s3.list_objects_v2(
            Bucket=self.config['s3']['bucket'],
            Prefix=prefix
        )
        
        if 'Contents' not in response:
            logger.info(f"No backups found with prefix: {prefix}")
            return
        
        # Sort backups by date (oldest first)
        backups = sorted(
            response['Contents'],
            key=lambda x: x['LastModified']
        )
        
        # Keep only the latest N backups based on retention policy
        backups_to_keep = min(retention_days, len(backups))
        backups_to_delete = backups[:-backups_to_keep] if backups_to_keep > 0 else backups
        
        # Delete old backups
        for backup in backups_to_delete:
            logger.info(f"Deleting old backup: {backup['Key']}")
            self.s3.delete_object(
                Bucket=self.config['s3']['bucket'],
                Key=backup['Key']
            )
    
    def _send_notification(self, subject: str, message: str):
        """
        Send a notification
        
        Args:
            subject: Notification subject
message: Notification message
        """
        if 'notifications' in self.config and 'sns_topic_arn' in self.config['notifications']:
            try:
                self.sns.publish(
                    TopicArn=self.config['notifications']['sns_topic_arn'],
                    Subject=subject,
                    Message=message
                )
                logger.info(f"Sent notification: {subject}")
            except Exception as e:
                logger.error(f"Failed to send notification: {e}")
    
    def restore_backup(self, backup_key: str, target_db: Optional[str] = None):
        """
        Restore a database from backup
        
        Args:
            backup_key: S3 key of the backup to restore
            target_db: Name of the target database (defaults to original DB name)
        """
        logger.info(f"Starting restore from backup: {backup_key}")
        
        try:
            # Create temporary directory for restore
            with tempfile.TemporaryDirectory() as temp_dir:
                # Download backup file
                backup_file = os.path.join(temp_dir, os.path.basename(backup_key))
                logger.info(f"Downloading backup from S3: s3://{self.config['s3']['bucket']}/{backup_key}")
                
                self.s3.download_file(
                    self.config['s3']['bucket'],
                    backup_key,
                    backup_file
                )
                
                # Determine target database
                target_database = target_db or self.config['database']['name']
                
                # Create connection string for pg_restore
                conn_string = f"postgresql://{self.config['database']['user']}:{self.config['database']['password']}@{self.config['database']['host']}:{self.config['database']['port']}/{target_database}"
                
                # Run pg_restore to restore backup
                logger.info(f"Restoring backup to database: {target_database}")
                result = subprocess.run(
                    [
                        'pg_restore',
                        '--clean',
                        '--if-exists',
                        '--exit-on-error',
                        '--dbname', conn_string,
                        backup_file
                    ],
                    capture_output=True,
                    text=True
                )
                
                if result.returncode != 0:
                    raise Exception(f"pg_restore failed: {result.stderr}")
                
                # Send success notification
                self._send_notification(
                    f"Database restore successful: {target_database}",
                    f"Successfully restored {target_database} database from backup.\n"
                    f"Backup file: s3://{self.config['s3']['bucket']}/{backup_key}"
                )
                
                logger.info(f"Restore completed successfully: {target_database}")
                return True
        
        except Exception as e:
            logger.error(f"Restore failed: {e}", exc_info=True)
            
            # Send failure notification
            self._send_notification(
                f"Database restore failed: {target_db or self.config['database']['name']}",
                f"Failed to restore database from backup.\n"
                f"Backup file: s3://{self.config['s3']['bucket']}/{backup_key}\n"
                f"Error: {str(e)}"
            )
            
            return False
    
    def list_available_backups(self) -> List[Dict[str, Any]]:
        """
        List available backups
        
        Returns:
            List of available backups with metadata
        """
        logger.info(f"Listing available backups")
        
        # List backups
        prefix = f"{self.config['s3']['prefix']}/"
        paginator = self.s3.get_paginator('list_objects_v2')
        
        backups = []
        for page in paginator.paginate(Bucket=self.config['s3']['bucket'], Prefix=prefix):
            if 'Contents' in page:
                for obj in page['Contents']:
                    # Get backup metadata
                    try:
                        response = self.s3.head_object(
                            Bucket=self.config['s3']['bucket'],
                            Key=obj['Key']
                        )
                        
                        # Extract backup type from key
                        backup_parts = obj['Key'].split('/')
                        backup_type = backup_parts[-2] if len(backup_parts) >= 2 else 'unknown'
                        
                        backups.append({
                            'key': obj['Key'],
                            'size': obj['Size'],
                            'last_modified': obj['LastModified'].isoformat(),
                            'type': response.get('Metadata', {}).get('backup-type', backup_type),
                            'database': response.get('Metadata', {}).get('database', self.config['database']['name']),
                            'timestamp': response.get('Metadata', {}).get('timestamp', '')
                        })
                    
                    except Exception as e:
                        logger.warning(f"Failed to get metadata for backup {obj['Key']}: {e}")
        
        # Sort backups by date (newest first)
        backups.sort(key=lambda x: x['last_modified'], reverse=True)
        
        return backups
    
    def verify_backup(self, backup_key: str) -> bool:
        """
        Verify a backup file
        
        Args:
            backup_key: S3 key of the backup to verify
            
        Returns:
            True if backup is valid, False otherwise
        """
        logger.info(f"Verifying backup: {backup_key}")
        
        try:
            # Create temporary directory for verification
            with tempfile.TemporaryDirectory() as temp_dir:
                # Download backup file
                backup_file = os.path.join(temp_dir, os.path.basename(backup_key))
                logger.info(f"Downloading backup from S3: s3://{self.config['s3']['bucket']}/{backup_key}")
                
                self.s3.download_file(
                    self.config['s3']['bucket'],
                    backup_key,
                    backup_file
                )
                
                # Verify backup file
                logger.info(f"Verifying backup file: {backup_file}")
                result = subprocess.run(
                    [
                        'pg_restore',
                        '--list',
                        backup_file
                    ],
                    capture_output=True,
                    text=True
                )
                
                if result.returncode != 0:
                    raise Exception(f"Backup verification failed: {result.stderr}")
                
                logger.info(f"Backup verified successfully: {backup_key}")
                return True
        
        except Exception as e:
            logger.error(f"Backup verification failed: {e}", exc_info=True)
            return False
    
    def setup_continuous_archiving(self):
        """
        Set up continuous archiving using PostgreSQL's WAL (Write-Ahead Log)
        """
        logger.info(f"Setting up continuous archiving")
        
        try:
            # Create directory for WAL archives
            wal_dir = os.path.join(self.config['local']['backup_dir'], 'wal_archives')
            os.makedirs(wal_dir, exist_ok=True)
            
            # Create SQL script to configure continuous archiving
            sql_script = f"""
            ALTER SYSTEM SET wal_level = 'replica';
            ALTER SYSTEM SET archive_mode = 'on';
            ALTER SYSTEM SET archive_command = 'test ! -f {wal_dir}/%f && cp %p {wal_dir}/%f';
            ALTER SYSTEM SET archive_timeout = '1h';
            """
            
            # Write SQL script to temporary file
            with tempfile.NamedTemporaryFile(suffix='.sql', mode='w', delete=False) as f:
                f.write(sql_script)
                sql_file = f.name
            
            try:
                # Run SQL script
                conn_string = f"postgresql://{self.config['database']['user']}:{self.config['database']['password']}@{self.config['database']['host']}:{self.config['database']['port']}/{self.config['database']['name']}"
                
                result = subprocess.run(
                    [
                        'psql',
                        '-f', sql_file,
                        conn_string
                    ],
                    capture_output=True,
                    text=True
                )
                
                if result.returncode != 0:
                    raise Exception(f"Failed to configure WAL archiving: {result.stderr}")
                
                # Reload PostgreSQL configuration
                result = subprocess.run(
                    [
                        'psql',
                        '-c', 'SELECT pg_reload_conf();',
                        conn_string
                    ],
                    capture_output=True,
                    text=True
                )
                
                if result.returncode != 0:
                    raise Exception(f"Failed to reload PostgreSQL configuration: {result.stderr}")
                
                logger.info(f"Continuous archiving configured successfully")
                return True
            
            finally:
                # Remove temporary SQL file
                os.unlink(sql_file)
        
        except Exception as e:
            logger.error(f"Failed to set up continuous archiving: {e}", exc_info=True)
            return False
    
    def setup_streaming_replication(self, replica_host: str, replica_port: int = 5432):
        """
        Set up streaming replication to a replica server
        
        Args:
            replica_host: Hostname of the replica server
            replica_port: Port of the replica server
        """
        logger.info(f"Setting up streaming replication to {replica_host}:{replica_port}")
        
        try:
            # Create replication user if not exists
            replication_user = self.config.get('replication', {}).get('user', 'replication_user')
            replication_password = self.config.get('replication', {}).get('password', 'replication_password')
            
            conn_string = f"postgresql://{self.config['database']['user']}:{self.config['database']['password']}@{self.config['database']['host']}:{self.config['database']['port']}/{self.config['database']['name']}"
            
            # Create SQL script for primary server
            primary_script = f"""
            -- Create replication user
            CREATE USER {replication_user} WITH REPLICATION ENCRYPTED PASSWORD '{replication_password}';
            
            -- Configure pg_hba.conf for replication
            ALTER SYSTEM SET listen_addresses = '*';
            """
            
            # Write SQL script to temporary file
            with tempfile.NamedTemporaryFile(suffix='.sql', mode='w', delete=False) as f:
                f.write(primary_script)
                primary_sql_file = f.name
            
            try:
                # Run SQL script on primary
                result = subprocess.run(
                    [
                        'psql',
                        '-f', primary_sql_file,
                        conn_string
                    ],
                    capture_output=True,
                    text=True
                )
                
                if result.returncode != 0:
                    raise Exception(f"Failed to configure primary server: {result.stderr}")
                
                # Reload PostgreSQL configuration on primary
                result = subprocess.run(
                    [
                        'psql',
                        '-c', 'SELECT pg_reload_conf();',
                        conn_string
                    ],
                    capture_output=True,
                    text=True
                )
                
                if result.returncode != 0:
                    raise Exception(f"Failed to reload PostgreSQL configuration: {result.stderr}")
                
                # Create recovery.conf for replica
                recovery_conf = f"""
                # recovery.conf for PostgreSQL replica
                standby_mode = 'on'
                primary_conninfo = 'host={self.config['database']['host']} port={self.config['database']['port']} user={replication_user} password={replication_password}'
                trigger_file = '/tmp/pg_failover_trigger'
                """
                
                logger.info(f"Replica configuration (recovery.conf):\n{recovery_conf}")
                
                logger.info(f"Streaming replication configured successfully")
                logger.info(f"To complete setup, create a base backup and configure the replica server with recovery.conf")
                
                return True
            
            finally:
                # Remove temporary SQL file
                os.unlink(primary_sql_file)
        
        except Exception as e:
            logger.error(f"Failed to set up streaming replication: {e}", exc_info=True)
            return False
    
    def setup_automatic_backups(self):
        """
        Set up automatic backup schedule using cron
        """
        logger.info(f"Setting up automatic backup schedule")
        
        try:
            # Get backup intervals from config
            daily_time = self.config.get('schedule', {}).get('daily', '01:00')
            weekly_day = self.config.get('schedule', {}).get('weekly_day', '0')  # Sunday
            weekly_time = self.config.get('schedule', {}).get('weekly', '02:00')
            monthly_day = self.config.get('schedule', {}).get('monthly_day', '1')  # 1st of month
            monthly_time = self.config.get('schedule', {}).get('monthly', '03:00')
            
            # Parse times
            daily_hour, daily_minute = daily_time.split(':')
            weekly_hour, weekly_minute = weekly_time.split(':')
            monthly_hour, monthly_minute = monthly_time.split(':')
            
            # Create cron entries
            script_path = os.path.abspath(sys.argv[0])
            config_path = os.path.abspath(self.config.get('config_file', 'config.yaml'))
            
            daily_cron = f"{daily_minute} {daily_hour} * * * {script_path} --config {config_path} --type daily\n"
            weekly_cron = f"{weekly_minute} {weekly_hour} * * {weekly_day} {script_path} --config {config_path} --type weekly\n"
            monthly_cron = f"{monthly_minute} {monthly_hour} {monthly_day} * * {script_path} --config {config_path} --type monthly\n"
            
            # Write cron file
            cron_file = '/etc/cron.d/synthesis-db-backup'
            with open(cron_file, 'w') as f:
                f.write(f"# Automated backups for Reversible Meta-Synthesis database\n")
                f.write(f"SHELL=/bin/bash\n")
                f.write(f"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\n\n")
                f.write(daily_cron)
                f.write(weekly_cron)
                f.write(monthly_cron)
            
            # Set proper permissions
            os.chmod(cron_file, 0o644)
            
            logger.info(f"Automatic backup schedule configured successfully")
            logger.info(f"Daily backups: {daily_time}")
            logger.info(f"Weekly backups: Day {weekly_day} at {weekly_time}")
            logger.info(f"Monthly backups: Day {monthly_day} at {monthly_time}")
            
            return True
        
        except Exception as e:
            logger.error(f"Failed to set up automatic backup schedule: {e}", exc_info=True)
            return False

# Example usage
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Database backup and restore for Reversible Meta-Synthesis')
    parser.add_argument('--config', type=str, default='backup_config.yaml', help='Path to configuration file')
    parser.add_argument('--action', type=str, choices=['backup', 'restore', 'list', 'verify', 'setup-archiving', 'setup-replication', 'setup-schedule'], help='Action to perform')
    parser.add_argument('--type', type=str, choices=['daily', 'weekly', 'monthly'], default='daily', help='Type of backup')
    parser.add_argument('--backup-key', type=str, help='S3 key of backup (for restore/verify)')
    parser.add_argument('--target-db', type=str, help='Target database for restore')
    parser.add_argument('--replica-host', type=str, help='Hostname of replica server')
    parser.add_argument('--replica-port', type=int, default=5432, help='Port of replica server')
    args = parser.parse_args()
    
    # Create backup manager
    manager = DatabaseBackupManager(args.config)
    
    # Perform requested action
    if args.action == 'backup':
        success = manager.perform_backup(args.type)
        sys.exit(0 if success else 1)
    
    elif args.action == 'restore':
        if not args.backup_key:
            print("Error: --backup-key is required for restore action")
            sys.exit(1)
        
        success = manager.restore_backup(args.backup_key, args.target_db)
        sys.exit(0 if success else 1)
    
    elif args.action == 'list':
        backups = manager.list_available_backups()
        print(f"Available backups:")
        for backup in backups:
            print(f"- {backup['key']}")
            print(f"  Type: {backup['type']}")
            print(f"  Database: {backup['database']}")
            print(f"  Modified: {backup['last_modified']}")
            print(f"  Size: {backup['size'] / (1024*1024):.2f} MB")
            print()
    
    elif args.action == 'verify':
        if not args.backup_key:
            print("Error: --backup-key is required for verify action")
            sys.exit(1)
        
        success = manager.verify_backup(args.backup_key)
        sys.exit(0 if success else 1)
    
    elif args.action == 'setup-archiving':
        success = manager.setup_continuous_archiving()
        sys.exit(0 if success else 1)
    
    elif args.action == 'setup-replication':
        if not args.replica_host:
            print("Error: --replica-host is required for setup-replication action")
            sys.exit(1)
        
        success = manager.setup_streaming_replication(args.replica_host, args.replica_port)
        sys.exit(0 if success else 1)
    
    elif args.action == 'setup-schedule':
        success = manager.setup_automatic_backups()
        sys.exit(0 if success else 1)
    
    else:
        parser.print_help()
        sys.exit(1)
