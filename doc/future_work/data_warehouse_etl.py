#!/usr/bin/env python3
"""
ETL process for the Reversible Meta-Synthesis Data Warehouse.
"""

import os
import sys
import logging
import psycopg2
from psycopg2.extras import DictCursor
import datetime
import yaml
import argparse
from typing import Dict, List, Any, Optional
import pandas as pd
import json

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler('/var/log/synthesis-dw-etl.log')
    ]
)
logger = logging.getLogger("synthesis-dw-etl")

class DataWarehouseETL:
    """
    ETL process for the Reversible Meta-Synthesis Data Warehouse.
    """
    
    def __init__(self, config_file: str):
        """
        Initialize the ETL process
        
        Args:
            config_file: Path to the configuration file
        """
        # Load configuration
        with open(config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        
        # Validate configuration
        self._validate_config()
    
    def _validate_config(self):
        """Validate the configuration"""
        required_keys = [
            'source_db.host',
            'source_db.port',
            'source_db.name',
            'source_db.user',
            'source_db.password',
            'data_warehouse.host',
            'data_warehouse.port',
            'data_warehouse.name',
            'data_warehouse.user',
            'data_warehouse.password',
            'etl.batch_size',
            'etl.history_days'
        ]
        
        for key in required_keys:
            parts = key.split('.')
            config = self.config
            for part in parts:
                if part not in config:
                    raise ValueError(f"Missing required configuration key: {key}")
                config = config[part]
    
    def connect_source(self):
        """Create a connection to the source database"""
        return psycopg2.connect(
            host=self.config['source_db']['host'],
            port=self.config['source_db']['port'],
            dbname=self.config['source_db']['name'],
            user=self.config['source_db']['user'],
            password=self.config['source_db']['password'],
            cursor_factory=DictCursor
        )
    
    def connect_warehouse(self):
        """Create a connection to the data warehouse"""
        return psycopg2.connect(
            host=self.config['data_warehouse']['host'],
            port=self.config['data_warehouse']['port'],
            dbname=self.config['data_warehouse']['name'],
            user=self.config['data_warehouse']['user'],
            password=self.config['data_warehouse']['password'],
            cursor_factory=DictCursor
        )
    
    def run_full_etl(self):
        """Run the full ETL process"""
        logger.info("Starting full ETL process")
        
        try:
            # Initialize time dimension
            self._initialize_time_dimension()
            
            # Run master ETL procedure
            with self.connect_warehouse() as conn:
                with conn.cursor() as cur:
                    cur.execute("CALL etl_master()")
                    conn.commit()
            
            logger.info("Full ETL process completed successfully")
            return True
        
        except Exception as e:
            logger.error(f"ETL process failed: {e}", exc_info=True)
            return False
    
    def _initialize_time_dimension(self):
        """Initialize the time dimension table"""
        logger.info("Initializing time dimension")
        
        try:
            # Calculate date range
            end_date = datetime.date.today() + datetime.timedelta(days=365)  # One year in the future
            start_date = datetime.date.today() - datetime.timedelta(days=365*5)  # Five years in the past
            
            # Populate time dimension
            with self.connect_warehouse() as conn:
                with conn.cursor() as cur:
                    cur.execute(
                        "CALL populate_dim_time(%s, %s)",
                        (start_date, end_date)
                    )
                    conn.commit()
            
            logger.info(f"Time dimension initialized from {start_date} to {end_date}")
            return True
        
        except Exception as e:
            logger.error(f"Failed to initialize time dimension: {e}", exc_info=True)
            return False
    
    def extract_transform_jobs(self):
        """Extract and transform synthesis jobs data"""
        logger.info("Extracting synthesis jobs data")
        
        batch_size = self.config['etl']['batch_size']
        history_days = self.config['etl']['history_days']
        
        try:
            # Extract data from source
            with self.connect_source() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT 
                            sj.job_id,
                            sj.user_id,
                            sj.language,
                            sj.explanation_level,
                            sj.status,
                            sj.progress,
                            sj.created_at,
                            sj.completed_at,
                            sa.synthesis_time,
                            sa.cpu_usage,
                            sa.memory_usage,
                            sa.program_complexity,
                            sr.program,
                            sr.explanation_tree,
                            sr.metrics,
                            js.examples,
                            js.constraints,
                            ss.strategy_id,
                            ss.name as strategy_name
                        FROM 
                            synthesis_jobs sj
                        LEFT JOIN 
                            synthesis_analytics sa ON sj.job_id = sa.job_id
                        LEFT JOIN 
                            synthesis_results sr ON sj.job_id = sr.job_id
                        LEFT JOIN 
                            job_specifications js ON sj.job_id = js.job_id
                        LEFT JOIN 
                            synthesis_analytics sa2 ON sj.job_id = sa2.job_id
                        LEFT JOIN 
                            synthesis_strategies ss ON sa2.strategy_id = ss.strategy_id
                        WHERE 
                            sj.created_at >= CURRENT_TIMESTAMP - INTERVAL '%s days'
                        ORDER BY 
                            sj.created_at DESC
                        LIMIT %s
                    """, (history_days, batch_size))
                    
                    jobs = cur.fetchall()
            
            logger.info(f"Extracted {len(jobs)} synthesis jobs")
            
            # Transform data
            transformed_jobs = []
            for job in jobs:
                transformed_job = {
                    'job_id': job['job_id'],
                    'user_id': job['user_id'],
                    'language': job['language'],
                    'explanation_level': job['explanation_level'],
                    'status': job['status'],
                    'created_date': job['created_at'].date(),
                    'synthesis_time_seconds': job['synthesis_time'] / 1000 if job['synthesis_time'] else 0,
                    'cpu_usage_percent': job['cpu_usage'] or 0,
                    'memory_usage_mb': job['memory_usage'] / (1024*1024) if job['memory_usage'] else 0,
                    'program_complexity': job['program_complexity'] or 0,
                    'program_size_bytes': len(job['program']) if job['program'] else 0,
                    'success': job['status'] == 'completed',
                    'explanation_tree_nodes': self._count_explanation_nodes(job['explanation_tree']),
                    'example_count': len(job['examples']) if job['examples'] else 0,
                    'strategy_name': job['strategy_name']
                }
                transformed_jobs.append(transformed_job)
            
            return transformed_jobs
        
        except Exception as e:
            logger.error(f"Failed to extract and transform jobs data: {e}", exc_info=True)
            return []
    
    def _count_explanation_nodes(self, explanation_tree):
        """Count the number of nodes in an explanation tree"""
        if not explanation_tree:
            return 0
        
        try:
            return self._count_nodes_recursive(explanation_tree)
        except:
            return 0
    
    def _count_nodes_recursive(self, node):
        """Recursively count nodes in a tree"""
        if not node or not isinstance(node, dict):
            return 0
        
        count = 1  # Count this node
        
        # Count children
        children = node.get('children', [])
        for child in children:
            count += self._count_nodes_recursive(child)
        
        return count
    
    def extract_transform_patterns(self):
        """Extract and transform program patterns data"""
        logger.info("Extracting program patterns data")
        
        try:
            # Extract data from source
            with self.connect_source() as conn:
                with conn.cursor() as cur:
                    cur.execute("""
                        SELECT 
                            pattern_id,
                            language,
                            pattern_type,
                            pattern_structure,
                            description,
                            usage_count
                        FROM 
                            program_patterns
                        ORDER BY 
                            usage_count DESC
                    """)
                    
                    patterns = cur.fetchall()
            
            logger.info(f"Extracted {len(patterns)} program patterns")
            
            # Transform data
            transformed_patterns = []
            for pattern in patterns:
                # Calculate complexity level based on pattern structure
                complexity_level = self._calculate_pattern_complexity(pattern['pattern_structure'])
                
                transformed_pattern = {
                    'pattern_id': pattern['pattern_id'],
                    'pattern_type': pattern['pattern_type'],
                    'pattern_name': f"{pattern['language']}_{pattern['pattern_type']}",
                    'pattern_structure': pattern['pattern_structure'],
                    'complexity_level': complexity_level,
                    'description': pattern['description']
                }
                transformed_patterns.append(transformed_pattern)
            
            return transformed_patterns
        
        except Exception as e:
            logger.error(f"Failed to extract and transform patterns data: {e}", exc_info=True)
            return []
    
    def _calculate_pattern_complexity(self, pattern_structure):
        """Calculate complexity level of a pattern"""
        if not pattern_structure:
            return 1
        
        try:
            # Simple heuristic based on structure properties
            structure = pattern_structure
            
            # Count various complexity factors
            recursion = structure.get('recursion', 0)
            functions = structure.get('functions', 0)
            conditionals = structure.get('conditionals', 0)
            higher_order = structure.get('higher_order', 0)
            
            # Calculate weighted complexity
            complexity = (
                recursion * 2 +
                functions * 1 +
                conditionals * 1.5 +
                higher_order * 2
            )
            
            # Map to discrete levels
            if complexity < 2:
                return 1  # Simple
            elif complexity < 5:
                return 2  # Moderate
            elif complexity < 10:
                return 3  # Complex
            else:
                return 4  # Very complex
        
        except:
            return 1  # Default to simple
    
    def load_data(self, jobs, patterns):
        """Load transformed data into the data warehouse"""
        logger.info("Loading data into the data warehouse")
        
        try:
            # Load program patterns
            self._load_patterns(patterns)
            
            # Run ETL procedures
            with self.connect_warehouse() as conn:
                with conn.cursor() as cur:
                    cur.execute("CALL etl_master()")
                    conn.commit()
            
            logger.info("Data loaded successfully")
            return True
        
        except Exception as e:
            logger.error(f"Failed to load data: {e}", exc_info=True)
            return False
    
    def _load_patterns(self, patterns):
        """Load program patterns into the data warehouse"""
        if not patterns:
            return
        
        logger.info(f"Loading {len(patterns)} program patterns")
        
        try:
            with self.connect_warehouse() as conn:
                with conn.cursor() as cur:
                    for pattern in patterns:
                        # Check if pattern already exists
                        cur.execute("""
                            SELECT pattern_id 
                            FROM dim_program_pattern
                            WHERE pattern_name = %s AND pattern_type = %s
                        """, (pattern['pattern_name'], pattern['pattern_type']))
                        
                        existing = cur.fetchone()
                        
                        if existing:
                            # Update existing pattern
                            cur.execute("""
                                UPDATE dim_program_pattern
                                SET pattern_structure = %s,
                                    complexity_level = %s,
                                    description = %s
                                WHERE pattern_id = %s
                            """, (
                                json.dumps(pattern['pattern_structure']),
                                pattern['complexity_level'],
                                pattern['description'],
                                existing['pattern_id']
                            ))
                        else:
                            # Insert new pattern
                            cur.execute("""
                                INSERT INTO dim_program_pattern (
                                    pattern_type,
                                    pattern_name,
                                    pattern_structure,
                                    complexity_level,
                                    description
                                ) VALUES (%s, %s, %s, %s, %s)
                            """, (
                                pattern['pattern_type'],
                                pattern['pattern_name'],
                                json.dumps(pattern['pattern_structure']),
                                pattern['complexity_level'],
                                pattern['description']
                            ))
                
                conn.commit()
        
        except Exception as e:
            logger.error(f"Failed to load patterns: {e}", exc_info=True)
            raise
    
    def generate_warehouse_reports(self, output_dir: str):
        """Generate reports from the data warehouse"""
        logger.info("Generating data warehouse reports")
        
        try:
            os.makedirs(output_dir, exist_ok=True)
            
            # List of reports to generate
            reports = [
                ('synthesis_success_trends', 'view_synthesis_success_trends'),
                ('language_comparison', 'view_language_comparison'),
                ('explanation_level_effectiveness', 'view_explanation_level_effectiveness'),
                ('user_engagement', 'view_user_engagement'),
                ('pattern_effectiveness', 'view_pattern_effectiveness')
            ]
            
            # Generate each report
            for report_name, view_name in reports:
                self._generate_report(report_name, view_name, output_dir)
            
            logger.info(f"Reports generated successfully in {output_dir}")
            return True
        
        except Exception as e:
            logger.error(f"Failed to generate reports: {e}", exc_info=True)
            return False
    
    def _generate_report(self, report_name, view_name, output_dir):
        """Generate a single report from a view"""
        logger.info(f"Generating report: {report_name}")
        
        try:
            # Query the view
            with self.connect_warehouse() as conn:
                df = pd.read_sql(f"SELECT * FROM {view_name}", conn)
            
            # Save as CSV
            csv_path = os.path.join(output_dir, f"{report_name}.csv")
            df.to_csv(csv_path, index=False)
            
            # Save as JSON
            json_path = os.path.join(output_dir, f"{report_name}.json")
            df.to_json(json_path, orient='records', date_format='iso')
            
            logger.info(f"Report {report_name} generated with {len(df)} rows")
        
        except Exception as e:
            logger.error(f"Failed to generate report {report_name}: {e}", exc_info=True)
            raise

# Example usage
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='ETL process for Reversible Meta-Synthesis Data Warehouse')
    parser.add_argument('--config', type=str, default='dw_config.yaml', help='Path to configuration file')
    parser.add_argument('--action', type=str, choices=['full-etl', 'generate-reports'], default='full-etl', help='Action to perform')
    parser.add_argument('--output-dir', type=str, default='reports', help='Output directory for reports')
    args = parser.parse_args()
    
    etl = DataWarehouseETL(args.config)
    
    if args.action == 'full-etl':
        success = etl.run_full_etl()
        sys.exit(0 if success else 1)
    
    elif args.action == 'generate-reports':
        success = etl.generate_warehouse_reports(args.output_dir)
        sys.exit(0 if success else 1)
    
    else:
        parser.print_help()
        sys.exit(1)
