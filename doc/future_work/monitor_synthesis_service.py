#!/usr/bin/env python3
"""
Real-time monitoring for the Reversible Meta-Synthesis service.
"""

import time
import os
import logging
import json
import yaml
import argparse
import threading
import datetime
from typing import Dict, List, Any, Optional
import socket
import psutil
import requests
from prometheus_client import start_http_server, Counter, Gauge, Histogram, Summary

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('/var/log/synthesis-monitoring.log')
    ]
)
logger = logging.getLogger("synthesis-monitoring")

class SynthesisServiceMonitor:
    """
    Real-time monitoring for the Reversible Meta-Synthesis service.
    """
    
    def __init__(self, config_file: str):
        """
        Initialize the service monitor
        
        Args:
            config_file: Path to the configuration file
        """
        # Load configuration
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        
        # Initialize metrics
        self._init_metrics()
        
        # Get hostname and worker ID
        self.hostname = socket.gethostname()
        self.worker_id = os.environ.get('WORKER_ID', self.hostname)
        
        # Initialize monitoring state
        self.running = False
        self.monitor_thread = None
    
    def _init_metrics(self):
        """Initialize Prometheus metrics"""
        # Synthesis job metrics
        self.synthesis_attempts = Counter(
            'synthesis_attempts_total',
            'Total number of synthesis attempts',
            ['language', 'explanation_level']
        )
        
        self.synthesis_success = Counter(
            'synthesis_success_total',
            'Total number of successful syntheses',
            ['language', 'explanation_level']
        )
        
        self.synthesis_job_duration = Histogram(
            'synthesis_job_duration_seconds',
            'Duration of synthesis jobs in seconds',
            ['language', 'explanation_level'],
            buckets=[1, 5, 10, 30, 60, 120, 300, 600, 1800, 3600]
        )
        
        # Resource usage metrics
        self.worker_memory_usage = Gauge(
            'synthesis_worker_memory_usage_bytes',
            'Memory usage of synthesis worker in bytes',
            ['worker_id']
        )
        
        self.worker_cpu_usage = Gauge(
            'synthesis_worker_cpu_usage',
            'CPU usage of synthesis worker as a percentage',
            ['worker_id']
        )
        
        # Program complexity metrics
        self.program_complexity = Histogram(
            'synthesis_program_complexity',
            'Complexity of synthesized programs',
            ['language', 'explanation_level'],
            buckets=[1, 2, 3, 5, 8, 13, 21, 34, 55]
        )
        
        # Explanation tree metrics
        self.explanation_tree_size = Histogram(
            'synthesis_explanation_tree_size',
            'Size of explanation trees in nodes',
            ['language', 'explanation_level'],
            buckets=[10, 20, 50, 100, 200, 500, 1000, 2000, 5000]
        )
        
        # Queue metrics
        self.queue_size = Gauge(
            'synthesis_queue_size',
            'Number of jobs in the synthesis queue'
        )
        
        self.queue_wait_time = Histogram(
            'synthesis_queue_wait_time_seconds',
            'Wait time in the synthesis queue in seconds',
            buckets=[1, 5, 15, 30, 60, 120, 300, 600]
        )
    
    def start(self, port: int = 8000):
        """
        Start the monitoring service
        
        Args:
            port: Port to expose Prometheus metrics
        """
        if self.running:
            logger.warning("Monitoring service is already running")
            return
        
        # Start Prometheus metrics server
        start_http_server(port)
        logger.info(f"Prometheus metrics server started on port {port}")
        
        # Start monitoring thread
        self.running = True
        self.monitor_thread = threading.Thread(target=self._monitor_loop)
        self.monitor_thread.daemon = True
        self.monitor_thread.start()
        
        logger.info("Synthesis service monitoring started")
    
    def stop(self):
        """Stop the monitoring service"""
        self.running = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=10)
        logger.info("Synthesis service monitoring stopped")
    
    def _monitor_loop(self):
        """Main monitoring loop"""
        while self.running:
            try:
                # Monitor system resources
                self._monitor_resources()
                
                # Monitor synthesis queue
                self._monitor_queue()
                
                # Monitor active jobs
                self._monitor_active_jobs()
                
                # Sleep before next iteration
                time.sleep(15)  # 15 seconds
            
            except Exception as e:
                logger.error(f"Error in monitoring loop: {e}", exc_info=True)
                time.sleep(60)  # Wait longer after error
    
    def _monitor_resources(self):
        """Monitor system resources"""
        try:
            # Get CPU and memory usage
            cpu_percent = psutil.cpu_percent(interval=1)
            memory_info = psutil.virtual_memory()
            
            # Update metrics
            self.worker_cpu_usage.labels(worker_id=self.worker_id).set(cpu_percent)
            self.worker_memory_usage.labels(worker_id=self.worker_id).set(memory_info.used)
            
            # Log if resources are constrained
            if cpu_percent > 80:
                logger.warning(f"High CPU usage: {cpu_percent}%")
            
            if memory_info.percent > 80:
                logger.warning(f"High memory usage: {memory_info.percent}%")
        
        except Exception as e:
            logger.error(f"Error monitoring resources: {e}")
    
    def _monitor_queue(self):
        """Monitor synthesis job queue"""
        try:
            # Get queue information from API
            api_url = self.config.get('api', {}).get('url', 'http://localhost:8080')
            response = requests.get(f"{api_url}/api/v1/queue/stats")
            
            if response.status_code == 200:
                queue_stats = response.json()
                
                # Update metrics
                self.queue_size.set(queue_stats.get('size', 0))
                
                # Track wait times for jobs in queue
                for job in queue_stats.get('jobs', []):
                    created_at = datetime.datetime.fromisoformat(job.get('created_at').replace('Z', '+00:00'))
                    wait_time = (datetime.datetime.now(datetime.timezone.utc) - created_at).total_seconds()
                    self.queue_wait_time.observe(wait_time)
                    
                    # Log long-waiting jobs
                    if wait_time > 300:  # 5 minutes
                        logger.warning(f"Job {job.get('job_id')} has been waiting for {wait_time:.1f} seconds")
        
        except Exception as e:
            logger.error(f"Error monitoring queue: {e}")
    
    def _monitor_active_jobs(self):
        """Monitor active synthesis jobs"""
        try:
            # Get active job information from API
            api_url = self.config.get('api', {}).get('url', 'http://localhost:8080')
            response = requests.get(f"{api_url}/api/v1/jobs/active")
            
            if response.status_code == 200:
                active_jobs = response.json()
                
                for job in active_jobs:
                    # Record job metrics
                    language = job.get('language', 'unknown')
                    explanation_level = str(job.get('explanation_level', 0))
                    
                    # Check if job has completed since last check
                    if job.get('status') == 'completed':
                        # Record success
                        self.synthesis_success.labels(
                            language=language,
                            explanation_level=explanation_level
                        ).inc()
                        
                        # Record duration
                        created_at = datetime.datetime.fromisoformat(job.get('created_at').replace('Z', '+00:00'))
                        completed_at = datetime.datetime.fromisoformat(job.get('completed_at').replace('Z', '+00:00'))
                        duration = (completed_at - created_at).total_seconds()
                        
                        self.synthesis_job_duration.labels(
                            language=language,
                            explanation_level=explanation_level
                        ).observe(duration)
                        
                        # Record program complexity
                        if 'program_complexity' in job:
                            self.program_complexity.labels(
                                language=language,
                                explanation_level=explanation_level
                            ).observe(job['program_complexity'])
                        
                        # Record explanation tree size
                        if 'explanation_tree_size' in job:
                            self.explanation_tree_size.labels(
                                language=language,
                                explanation_level=explanation_level
                            ).observe(job['explanation_tree_size'])
                    
                    # Record attempt regardless of outcome
                    if job.get('status') in ['completed', 'failed']:
                        self.synthesis_attempts.labels(
                            language=language,
                            explanation_level=explanation_level
                        ).inc()
        
        except Exception as e:
            logger.error(f"Error monitoring active jobs: {e}")
    
    def record_synthesis_job(self, job_data: Dict[str, Any]):
        """
        Record metrics for a synthesis job
        
        Args:
            job_data: Synthesis job data
        """
        try:
            # Extract job information
            language = job_data.get('language', 'unknown')
            explanation_level = str(job_data.get('explanation_level', 0))
            status = job_data.get('status')
            
            # Record attempt
            self.synthesis_attempts.labels(
                language=language,
                explanation_level=explanation_level
            ).inc()
            
            # Record success if applicable
            if status == 'completed':
                self.synthesis_success.labels(
                    language=language,
                    explanation_level=explanation_level
                ).inc()
            
            # Record duration
            if 'synthesis_time' in job_data:
                synthesis_time = job_data['synthesis_time'] / 1000  # Convert ms to seconds
                self.synthesis_job_duration.labels(
                    language=language,
                    explanation_level=explanation_level
                ).observe(synthesis_time)
            
            # Record program complexity
            if 'program_complexity' in job_data:
                self.program_complexity.labels(
                    language=language,
                    explanation_level=explanation_level
                ).observe(job_data['program_complexity'])
            
            # Record explanation tree size
            if 'explanation_tree_size' in job_data:
                self.explanation_tree_size.labels(
                    language=language,
                    explanation_level=explanation_level
                ).observe(job_data['explanation_tree_size'])
            
            logger.info(f"Recorded metrics for job {job_data.get('job_id')}")
        
        except Exception as e:
            logger.error(f"Error recording job metrics: {e}", exc_info=True)

# Example usage
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Real-time monitoring for Reversible Meta-Synthesis service')
    parser.add_argument('--config', type=str, default='monitoring_config.yaml', help='Path to configuration file')
    parser.add_argument('--port', type=int, default=8000, help='Port to expose Prometheus metrics')
    args = parser.parse_args()
    
    monitor = SynthesisServiceMonitor(args.config)
    
    try:
        # Start monitoring
        monitor.start(port=args.port)
        
        # Keep running until interrupted
        while True:
            time.sleep(1)
    
    except KeyboardInterrupt:
        monitor.stop()
