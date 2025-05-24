#!/usr/bin/env python3

import os
import sys
import json
import time
import logging
import subprocess
import tempfile
from typing import Dict, List, Any, Tuple, Optional

import boto3
import docker
from prometheus_client import Counter, Gauge, Histogram, start_http_server

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    handlers=[logging.StreamHandler()]
)
logger = logging.getLogger(__name__)

# Metrics
SYNTHESIS_ATTEMPTS = Counter('synthesis_attempts_total', 'Number of synthesis attempts', ['language', 'explanationLevel'])
SYNTHESIS_SUCCESS = Counter('synthesis_success_total', 'Number of successful syntheses', ['language', 'explanationLevel'])
SYNTHESIS_DURATION = Histogram('synthesis_duration_seconds', 'Duration of synthesis', ['language', 'explanationLevel'])
WORKER_MEMORY_USAGE = Gauge('worker_memory_usage_bytes', 'Memory usage of worker')
WORKER_CPU_USAGE = Gauge('worker_cpu_usage_percent', 'CPU usage of worker')

class SynthesisWorker:
    def __init__(self, config_file: str = "worker_config.json"):
        """Initialize the synthesis worker"""
        # Load configuration
        with open(config_file, 'r') as f:
            self.config = json.load(f)
        
        # Set up SQS client for job queue
        self.sqs = boto3.client('sqs')
        self.queue_url = self.config['queue_url']
        
        # Set up S3 client for storing results
        self.s3 = boto3.client('s3')
        self.results_bucket = self.config['results_bucket']
        
        # Set up Docker client for language environments
        self.docker_client = docker.from_env()
        
        # Start metrics server
        start_http_server(self.config.get('metrics_port', 8000))
        
        logger.info(f"Worker initialized with queue {self.queue_url}")
    
    def start(self):
        """Start the worker process"""
        logger.info("Starting synthesis worker")
        
        while True:
            try:
                # Poll for job
                job = self._get_next_job()
                
                if job:
                    # Process job
                    logger.info(f"Processing job {job['jobId']}")
                    self._process_job(job)
                else:
                    # No job available, wait before polling again
                    time.sleep(self.config.get('polling_interval', 5))
            
            except Exception as e:
                logger.exception(f"Error processing job: {e}")
                time.sleep(10)  # Wait longer after error
    
    def _get_next_job(self) -> Optional[Dict[str, Any]]:
        """Get the next job from the queue"""
        response = self.sqs.receive_message(
            QueueUrl=self.queue_url,
            AttributeNames=['All'],
            MaxNumberOfMessages=1,
            WaitTimeSeconds=20
        )
        
        if 'Messages' in response:
            message = response['Messages'][0]
            receipt_handle = message['ReceiptHandle']
            
            try:
                job = json.loads(message['Body'])
                
                # Update job status to running
                self._update_job_status(job['jobId'], 'running', 0, 'Starting synthesis')
                
                # Delete message from queue
                self.sqs.delete_message(
                    QueueUrl=self.queue_url,
                    ReceiptHandle=receipt_handle
                )
                
                return job
            
            except json.JSONDecodeError:
                logger.error(f"Invalid job format: {message['Body']}")
                
                # Delete invalid message
                self.sqs.delete_message(
                    QueueUrl=self.queue_url,
                    ReceiptHandle=receipt_handle
                )
                
                return None
        
        return None
    
    def _process_job(self, job: Dict[str, Any]):
        """Process a synthesis job"""
        job_id = job['jobId']
        language = job['language']
        examples = job['examples']
        explanation_level = job.get('explanationLevel', 1)
        constraints = job.get('constraints', {})
        
        # Track metrics
        SYNTHESIS_ATTEMPTS.labels(language=language, explanationLevel=explanation_level).inc()
        
        try:
            with SYNTHESIS_DURATION.labels(language=language, explanationLevel=explanation_level).time():
                # Update status
                self._update_job_status(job_id, 'running', 10, 'Preparing synthesis environment')
                
                # Prepare input data
                input_data = self._prepare_input_data(job)
                
                # Update status
                self._update_job_status(job_id, 'running', 20, 'Starting synthesis process')
                
                # Run synthesis in appropriate container
                result = self._run_synthesis(language, input_data, explanation_level, constraints)
                
                # Update status
                self._update_job_status(job_id, 'running', 90, 'Finalizing results')
                
                # Store results
                self._store_results(job_id, result)
                
                # Update job status to completed
                self._update_job_status(job_id, 'completed', 100, 'Synthesis completed successfully')
                
                # Track success
                SYNTHESIS_SUCCESS.labels(language=language, explanationLevel=explanation_level).inc()
        
        except Exception as e:
            logger.exception(f"Error processing job {job_id}: {e}")
            
            # Update job status to failed
            self._update_job_status(job_id, 'failed', 0, f"Synthesis failed: {str(e)}")
    
    def _prepare_input_data(self, job: Dict[str, Any]) -> Dict[str, Any]:
        """Prepare input data for synthesis"""
        # Copy relevant job properties
        input_data = {
            'examples': job['examples'],
            'explanationLevel': job.get('explanationLevel', 1),
            'constraints': job.get('constraints', {})
        }
        
        return input_data
    
    def _run_synthesis(
        self, 
        language: str, 
        input_data: Dict[str, Any],
        explanation_level: int,
        constraints: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Run synthesis in the appropriate language container"""
        # Get container image for language
        image_name = self._get_language_image(language)
        
        # Create temporary directory for input/output
        with tempfile.TemporaryDirectory() as temp_dir:
            # Write input data to file
            input_file = os.path.join(temp_dir, 'input.json')
            with open(input_file, 'w') as f:
                json.dump(input_data, f)
            
            # Run container
            logger.info(f"Running synthesis in {image_name} container")
            start_time = time.time()
            
            try:
                container = self.docker_client.containers.run(
                    image_name,
                    command=f"synthesize --input /data/input.json --output /data/output.json --explanation-level {explanation_level}",
                    volumes={temp_dir: {'bind': '/data', 'mode': 'rw'}},
                    environment={
                        'MAX_MEMORY': str(constraints.get('memoryLimit', '4g')),
                        'MAX_CPU': str(constraints.get('cpuLimit', '2')),
                        'TIME_LIMIT': str(constraints.get('timeLimit', '300'))
                    },
                    mem_limit=constraints.get('memoryLimit', '4g'),
                    cpu_quota=int(float(constraints.get('cpuLimit', 2)) * 100000),
                    detach=True
                )
                
                # Monitor container
                self._monitor_container(container)
                
                # Wait for container to finish
                exit_code = container.wait()['StatusCode']
                
                if exit_code != 0:
                    logs = container.logs().decode('utf-8')
                    raise Exception(f"Synthesis failed with exit code {exit_code}: {logs}")
                
                # Read output data
                output_file = os.path.join(temp_dir, 'output.json')
                if os.path.exists(output_file):
                    with open(output_file, 'r') as f:
                        result = json.load(f)
                else:
                    raise Exception("Synthesis completed but no output file was produced")
                
                # Add metrics
                result['metrics'] = {
                    'synthesisTime': time.time() - start_time,
                    'programComplexity': self._calculate_complexity(result['program']),
                    'resourceUsage': self._get_resource_usage(container)
                }
                
                return result
                
            finally:
                # Clean up container
                try:
                    container.remove(force=True)
                except:
                    pass
    
    def _monitor_container(self, container):
        """Monitor container resources and update status"""
        job_id = container.labels.get('job_id')
        
        # Update status every 5 seconds
        progress = 20
        while container.status == 'running':
            try:
                # Get container stats
                stats = container.stats(stream=False)
                
                # Calculate CPU and memory usage
                cpu_usage = self._calculate_cpu_usage(stats)
                memory_usage = self._calculate_memory_usage(stats)
                
                # Update metrics
                WORKER_CPU_USAGE.set(cpu_usage)
                WORKER_MEMORY_USAGE.set(memory_usage)
                
                # Increment progress (simple approach, could be more sophisticated)
                progress = min(progress + 2, 89)  # Max 89% until completion
                
                # Update job status
                if job_id:
                    self._update_job_status(
                        job_id, 
                        'running', 
                        progress, 
                        f"Synthesis in progress - CPU: {cpu_usage:.1f}%, Memory: {memory_usage / 1024 / 1024:.1f}MB"
                    )
                
                time.sleep(5)
                container.reload()  # Refresh container status
                
            except Exception as e:
                logger.warning(f"Error monitoring container: {e}")
                break
    
    def _calculate_cpu_usage(self, stats: Dict[str, Any]) -> float:
        """Calculate CPU usage percentage from container stats"""
        # Implementation depends on Docker stats format
        # This is a simplified version
        try:
            cpu_delta = stats['cpu_stats']['cpu_usage']['total_usage'] - stats['precpu_stats']['cpu_usage']['total_usage']
            system_delta = stats['cpu_stats']['system_cpu_usage'] - stats['precpu_stats']['system_cpu_usage']
            num_cpus = len(stats['cpu_stats']['cpu_usage']['percpu_usage'])
            
            if system_delta > 0:
                return (cpu_delta / system_delta) * num_cpus * 100.0
            return 0.0
        except:
            return 0.0
    
    def _calculate_memory_usage(self, stats: Dict[str, Any]) -> float:
        """Calculate memory usage in bytes from container stats"""
        try:
            return stats['memory_stats']['usage']
        except:
            return 0.0
    
    def _get_resource_usage(self, container) -> Dict[str, float]:
        """Get resource usage statistics from container"""
        try:
            stats = container.stats(stream=False)
            return {
                'cpu': self._calculate_cpu_usage(stats),
                'memory': self._calculate_memory_usage(stats)
            }
        except:
            return {'cpu': 0.0, 'memory': 0.0}
    
    def _calculate_complexity(self, program: str) -> float:
        """Calculate complexity metrics for the synthesized program"""
        # Simple implementation: length + structural complexity estimate
        try:
            lines = program.count('\n') + 1
            tokens = len(program.split())
            nested_depth = program.count('(') - program.count(')') if '(' in program else 0
            
            # Simple weighted formula
            complexity = (0.1 * lines) + (0.05 * tokens) + (0.2 * nested_depth)
            return max(1.0, complexity)
        except:
            return 1.0
    
    def _get_language_image(self, language: str) -> str:
        """Get Docker image name for the specified language"""
        image_map = {
            'prolog': 'reversible-meta-synthesis/prolog:latest',
            'hy': 'reversible-meta-synthesis/hy:latest',
            'scheme': 'reversible-meta-synthesis/scheme:latest',
            'clojure': 'reversible-meta-synthesis/clojure:latest',
            'python': 'reversible-meta-synthesis/python:latest',
            'haskell': 'reversible-meta-synthesis/haskell:latest'
        }
        
        if language not in image_map:
            raise ValueError(f"Unsupported language: {language}")
        
        return image_map[language]
    
    def _store_results(self, job_id: str, result: Dict[str, Any]):
        """Store synthesis results in S3"""
        result_json = json.dumps(result)
        
        self.s3.put_object(
            Bucket=self.results_bucket,
            Key=f"results/{job_id}.json",
            Body=result_json,
            ContentType='application/json'
        )
        
        logger.info(f"Stored results for job {job_id} in S3")
    
    def _update_job_status(self, job_id: str, status: str, progress: int, message: str):
        """Update job status in DynamoDB"""
        # Use boto3 to update DynamoDB
        dynamodb = boto3.resource('dynamodb')
        table = dynamodb.Table(self.config['status_table'])
        
        table.update_item(
            Key={'jobId': job_id},
            UpdateExpression="set #status = :s, progress = :p, statusMessage = :m, updatedAt = :t",
            ExpressionAttributeNames={
                '#status': 'status'
            },
            ExpressionAttributeValues={
                ':s': status,
                ':p': progress,
                ':m': message,
                ':t': int(time.time())
            }
        )
        
        logger.info(f"Updated status for job {job_id}: {status} - {progress}% - {message}")

if __name__ == "__main__":
    # Parse command line arguments
    import argparse
    parser = argparse.ArgumentParser(description='Synthesis Worker')
    parser.add_argument('--config', type=str, default='worker_config.json', help='Path to config file')
    args = parser.parse_args()
    
    # Start worker
    worker = SynthesisWorker(args.config)
    worker.start()
