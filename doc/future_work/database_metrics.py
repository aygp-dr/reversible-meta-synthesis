#!/usr/bin/env python3
"""
Database metrics collection and monitoring for the Reversible Meta-Synthesis service.
"""

import time
import logging
import psycopg2
from psycopg2.extras import DictCursor
import prometheus_client as prometheus
from prometheus_client import Gauge, Counter, Histogram, Summary
from typing import Dict, List, Optional
import threading

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s'
)
logger = logging.getLogger("db-metrics")

class DatabaseMetricsCollector:
    """
    Collects and exposes metrics about the synthesis database performance.
    """
    
    def __init__(self, conn_string: str, poll_interval: int = 60):
        """
        Initialize the metrics collector
        
        Args:
            conn_string: PostgreSQL connection string
            poll_interval: Interval in seconds to poll for metrics
        """
        self.conn_string = conn_string
        self.poll_interval = poll_interval
        self.running = False
        self.poll_thread = None
        
        # Initialize Prometheus metrics
        
        # Database performance metrics
        self.db_size = Gauge(
            'synthesis_db_size_bytes',
            'Total size of the synthesis database in bytes',
            ['database']
        )
        
        self.table_sizes = Gauge(
            'synthesis_table_size_bytes',
            'Size of each table in the synthesis database',
            ['table', 'database']
        )
        
        self.index_sizes = Gauge(
            'synthesis_index_size_bytes',
            'Size of each index in the synthesis database',
            ['index', 'table', 'database']
        )
        
        self.connection_count = Gauge(
            'synthesis_db_connections',
            'Number of active database connections',
            ['database', 'state']
        )
        
        self.transaction_count = Counter(
            'synthesis_db_transactions_total',
            'Total number of database transactions',
            ['database', 'type']
        )
        
        self.query_duration = Histogram(
            'synthesis_db_query_duration_seconds',
            'Duration of database queries',
            ['database', 'table', 'operation'],
            buckets=(0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0)
        )
        
        self.cache_hit_ratio = Gauge(
            'synthesis_db_cache_hit_ratio',
            'Database cache hit ratio',
            ['database', 'type']
        )
        
        # Synthesis-specific metrics
        self.synthesis_job_count = Gauge(
            'synthesis_jobs_count',
            'Number of synthesis jobs',
            ['status']
        )
        
        self.synthesis_job_duration = Histogram(
            'synthesis_job_duration_seconds',
            'Duration of synthesis jobs',
            ['language', 'explanation_level'],
            buckets=(1.0, 5.0, 10.0, 30.0, 60.0, 120.0, 300.0, 600.0, 1800.0, 3600.0)
        )
        
        self.program_complexity = Gauge(
            'synthesis_program_complexity',
            'Complexity of synthesized programs',
            ['language', 'explanation_level']
        )
        
        self.explanation_tree_size = Gauge(
            'synthesis_explanation_tree_size',
            'Size of explanation trees in nodes',
            ['language', 'explanation_level']
        )
        
        # Replication metrics
        self.replication_lag = Gauge(
            'synthesis_db_replication_lag_seconds',
            'Replication lag in seconds',
            ['database', 'replica']
        )
        
        # Start the metrics server
        prometheus.start_http_server(8000)
    
    def start(self):
        """Start collecting metrics"""
        if self.running:
            logger.warning("Metrics collector is already running")
            return
        
        self.running = True
        self.poll_thread = threading.Thread(target=self._metrics_loop)
        self.poll_thread.daemon = True
        self.poll_thread.start()
        logger.info("Database metrics collector started")
    
    def stop(self):
        """Stop collecting metrics"""
        self.running = False
        if self.poll_thread:
            self.poll_thread.join(timeout=10)
            logger.info("Database metrics collector stopped")
    
    def _metrics_loop(self):
        """Main metrics collection loop"""
        while self.running:
            try:
                # Collect all metrics
                self._collect_database_size_metrics()
                self._collect_connection_metrics()
                self._collect_performance_metrics()
                self._collect_synthesis_metrics()
                self._collect_replication_metrics()
                
                # Log successful collection
                logger.debug("Collected database metrics successfully")
            
            except Exception as e:
                logger.error(f"Error collecting database metrics: {e}", exc_info=True)
            
            # Wait for next collection interval
            time.sleep(self.poll_interval)
    
    def connect(self):
        """Create a database connection"""
        return psycopg2.connect(
            self.conn_string,
            cursor_factory=DictCursor
        )
    
    def _collect_database_size_metrics(self):
        """Collect metrics about database size"""
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get database name
                cur.execute("SELECT current_database()")
                db_name = cur.fetchone()[0]
                
                # Get total database size
                cur.execute("""
                    SELECT pg_database_size(current_database())
                """)
                db_size = cur.fetchone()[0]
                self.db_size.labels(database=db_name).set(db_size)
                
                # Get table sizes
                cur.execute("""
                    SELECT 
                        relname as table_name,
                        pg_total_relation_size(relid) as total_size
                    FROM 
                        pg_catalog.pg_statio_user_tables
                    ORDER BY 
                        total_size DESC
                """)
                
                for row in cur.fetchall():
                    self.table_sizes.labels(
                        table=row['table_name'],
                        database=db_name
                    ).set(row['total_size'])
                
                # Get index sizes
                cur.execute("""
                    SELECT 
                        indexrelname as index_name,
                        relname as table_name,
                        pg_relation_size(indexrelid) as index_size
                    FROM 
                        pg_catalog.pg_statio_user_indexes
                    ORDER BY 
                        index_size DESC
                """)
                
                for row in cur.fetchall():
                    self.index_sizes.labels(
                        index=row['index_name'],
                        table=row['table_name'],
                        database=db_name
                    ).set(row['index_size'])
    
    def _collect_connection_metrics(self):
        """Collect metrics about database connections"""
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get database name
                cur.execute("SELECT current_database()")
                db_name = cur.fetchone()[0]
                
                # Get connection count by state
                cur.execute("""
                    SELECT 
                        state,
                        COUNT(*) as count
                    FROM 
                        pg_stat_activity
                    WHERE 
                        datname = current_database()
                    GROUP BY 
                        state
                """)
                
                for row in cur.fetchall():
                    state = row['state'] or 'unknown'
                    self.connection_count.labels(
                        database=db_name,
                        state=state
                    ).set(row['count'])
                
                # Get transaction counts
                cur.execute("""
                    SELECT 
                        'commit' as type,
                        SUM(xact_commit) as count
                    FROM 
                        pg_stat_database
                    WHERE 
                        datname = current_database()
                    UNION ALL
                    SELECT 
                        'rollback' as type,
                        SUM(xact_rollback) as count
                    FROM 
                        pg_stat_database
                    WHERE 
                        datname = current_database()
                """)
                
                for row in cur.fetchall():
                    # Save the current value
                    current_count = self.transaction_count.labels(
                        database=db_name,
                        type=row['type']
                    )._value.get()
                    
                    # Increment by the difference
                    if current_count is not None:
                        increment = max(0, row['count'] - current_count)
                        if increment > 0:
                            self.transaction_count.labels(
                                database=db_name,
                                type=row['type']
                            ).inc(increment)
                    else:
                        # First measurement, just set the counter
                        self.transaction_count.labels(
                            database=db_name,
                            type=row['type']
                        )._value.set(row['count'])
    
    def _collect_performance_metrics(self):
        """Collect metrics about database performance"""
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get database name
                cur.execute("SELECT current_database()")
                db_name = cur.fetchone()[0]
                
                # Get cache hit ratios
                cur.execute("""
                    SELECT 
                        'buffer' as type,
                        CASE 
                            WHEN blks_hit + blks_read > 0 
                            THEN blks_hit::float / (blks_hit + blks_read) 
                            ELSE 0 
                        END as hit_ratio
                    FROM 
                        pg_stat_database
                    WHERE 
                        datname = current_database()
                    UNION ALL
                    SELECT 
                        'index' as type,
                        CASE 
                            WHEN idx_blks_hit + idx_blks_read > 0 
                            THEN idx_blks_hit::float / (idx_blks_hit + idx_blks_read) 
                            ELSE 0 
                        END as hit_ratio
                    FROM 
                        pg_statio_user_tables
                    ORDER BY 
                        type
                """)
                
                for row in cur.fetchall():
                    self.cache_hit_ratio.labels(
                        database=db_name,
                        type=row['type']
                    ).set(row['hit_ratio'])
                
                # Get query durations from pg_stat_statements (if extension is enabled)
                try:
                    cur.execute("""
                        SELECT EXISTS (
                            SELECT 1 FROM pg_extension WHERE extname = 'pg_stat_statements'
                        )
                    """)
                    
                    has_pg_stat_statements = cur.fetchone()[0]
                    
                    if has_pg_stat_statements:
                        cur.execute("""
                            SELECT 
                                substring(query from 1 for 50) as query_sample,
                                calls,
                                total_time / calls as avg_time,
                                (regexp_matches(query, '^\\s*(\\w+)'))[1] as operation,
                                (regexp_matches(query, '\\s+FROM\\s+([\\w\\.]+)'))[1] as table_name
                            FROM 
                                pg_stat_statements
                            WHERE 
                                dbid = (SELECT oid FROM pg_database WHERE datname = current_database())
                                AND total_time > 0
                            ORDER BY 
                                total_time DESC
                            LIMIT 20
                        """)
                        
                        for row in cur.fetchall():
                            if row['operation'] and row['table_name']:
                                operation = row['operation'].lower()
                                table = row['table_name'].lower()
                                
                                # Record in histogram
                                self.query_duration.labels(
                                    database=db_name,
                                    table=table,
                                    operation=operation
                                ).observe(row['avg_time'] / 1000.0)  # Convert to seconds
                except Exception as e:
                    logger.warning(f"Could not collect query duration metrics: {e}")
    
    def _collect_synthesis_metrics(self):
        """Collect synthesis-specific metrics"""
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get job counts by status
                cur.execute("""
                    SELECT 
                        status,
                        COUNT(*) as count
                    FROM 
                        synthesis_jobs
                    GROUP BY 
                        status
                """)
                
                for row in cur.fetchall():
                    self.synthesis_job_count.labels(
                        status=row['status']
                    ).set(row['count'])
                
                # Get job durations
                cur.execute("""
                    SELECT 
                        language,
                        explanation_level,
                        AVG(EXTRACT(EPOCH FROM (completed_at - created_at))) as avg_duration,
                        COUNT(*) as count
                    FROM 
                        synthesis_jobs
                    WHERE 
                        status = 'completed'
                        AND completed_at IS NOT NULL
                    GROUP BY 
                        language, explanation_level
                """)
                
                for row in cur.fetchall():
                    # Record average in histogram by simulating observations
                    # This is an approximation since we don't have the raw data
                    if row['count'] > 0:
                        self.synthesis_job_duration.labels(
                            language=row['language'],
                            explanation_level=str(row['explanation_level'])
                        ).observe(row['avg_duration'])
                
                # Get program complexity
                cur.execute("""
                    SELECT 
                        j.language,
                        j.explanation_level,
                        AVG(calculate_program_complexity(r.program)) as avg_complexity
                    FROM 
                        synthesis_jobs j
                    JOIN 
                        synthesis_results r ON j.job_id = r.job_id
                    WHERE 
                        j.status = 'completed'
                    GROUP BY 
                        j.language, j.explanation_level
                """)
                
                for row in cur.fetchall():
                    self.program_complexity.labels(
                        language=row['language'],
                        explanation_level=str(row['explanation_level'])
                    ).set(row['avg_complexity'])
                
                # Get explanation tree sizes
                cur.execute("""
                    SELECT 
                        j.language,
                        j.explanation_level,
                        AVG(jsonb_array_length(r.explanation_tree->'children')) as avg_tree_size
                    FROM 
                        synthesis_jobs j
                    JOIN 
                        synthesis_results r ON j.job_id = r.job_id
                    WHERE 
                        j.status = 'completed'
                    GROUP BY 
                        j.language, j.explanation_level
                """)
                
                for row in cur.fetchall():
                    self.explanation_tree_size.labels(
                        language=row['language'],
                        explanation_level=str(row['explanation_level'])
                    ).set(row['avg_tree_size'])
    
    def _collect_replication_metrics(self):
        """Collect metrics about database replication"""
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get database name
                cur.execute("SELECT current_database()")
                db_name = cur.fetchone()[0]
                
                # Check if database is configured for replication
                cur.execute("""
                    SELECT 
                        count(*) > 0 as has_replication
                    FROM 
                        pg_catalog.pg_stat_replication
                """)
                
                has_replication = cur.fetchone()['has_replication']
                
                if has_replication:
                    # Get replication lag for each replica
                    cur.execute("""
                        SELECT 
                            application_name as replica_name,
                            EXTRACT(EPOCH FROM (now() - pg_last_xact_replay_timestamp())) as lag_seconds
                        FROM 
                            pg_catalog.pg_stat_replication
                    """)
                    
                    for row in cur.fetchall():
                        self.replication_lag.labels(
                            database=db_name,
                            replica=row['replica_name']
                        ).set(row['lag_seconds'])

class QueryPerformanceMonitor:
    """
    Monitors and logs slow queries in the synthesis database.
    """
    
    def __init__(self, conn_string: str, slow_query_threshold_ms: int = 500):
        """
        Initialize the query performance monitor
        
        Args:
            conn_string: PostgreSQL connection string
            slow_query_threshold_ms: Threshold in milliseconds to consider a query slow
        """
        self.conn_string = conn_string
        self.slow_query_threshold_ms = slow_query_threshold_ms
        self.running = False
        self.monitor_thread = None
        
        # Configure logging
        self.logger = logging.getLogger("slow-query-monitor")
        
        # Metrics for slow queries
        self.slow_query_count = Counter(
            'synthesis_db_slow_queries_total',
            'Total number of slow queries',
            ['database', 'query_type']
        )
    
    def start(self):
        """Start monitoring slow queries"""
        if self.running:
            self.logger.warning("Query performance monitor is already running")
            return
        
        self.running = True
        self.monitor_thread = threading.Thread(target=self._monitor_loop)
        self.monitor_thread.daemon = True
        self.monitor_thread.start()
        self.logger.info(f"Query performance monitor started (threshold: {self.slow_query_threshold_ms}ms)")
    
    def stop(self):
        """Stop monitoring slow queries"""
        self.running = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=10)
            self.logger.info("Query performance monitor stopped")
    
    def connect(self):
        """Create a database connection"""
        return psycopg2.connect(
            self.conn_string,
            cursor_factory=DictCursor
        )
    
    def _monitor_loop(self):
        """Main monitoring loop"""
        # Check if pg_stat_statements extension is available
        have_pg_stat_statements = False
        
        with self.connect() as conn:
            with conn.cursor() as cur:
                cur.execute("""
                    SELECT EXISTS (
                        SELECT 1 FROM pg_extension WHERE extname = 'pg_stat_statements'
                    )
                """)
                have_pg_stat_statements = cur.fetchone()[0]
        
        if not have_pg_stat_statements:
            self.logger.warning("pg_stat_statements extension not available. Slow query monitoring disabled.")
            return
        
        # Initialize last query IDs dictionary
        last_query_ids = {}
        
        # Main monitoring loop
        while self.running:
            try:
                with self.connect() as conn:
                    with conn.cursor() as cur:
                        # Get database name
                        cur.execute("SELECT current_database()")
                        db_name = cur.fetchone()[0]
                        
                        # Reset pg_stat_statements if this is the first run
                        if not last_query_ids:
                            try:
                                cur.execute("SELECT pg_stat_statements_reset()")
                                conn.commit()
                            except Exception as e:
                                self.logger.warning(f"Could not reset pg_stat_statements: {e}")
                        
                        # Get slow queries
                        cur.execute("""
                            SELECT 
                                queryid,
                                query,
                                calls,
                                total_time / calls as avg_time_ms,
                                max_time as max_time_ms,
                                mean_time as mean_time_ms,
                                (regexp_matches(query, '^\\s*(\\w+)'))[1] as query_type
                            FROM 
                                pg_stat_statements
                            WHERE 
                                dbid = (SELECT oid FROM pg_database WHERE datname = current_database())
                                AND (total_time / calls) > %s
                                OR max_time > %s
                            ORDER BY 
                                avg_time_ms DESC
                        """, (self.slow_query_threshold_ms, self.slow_query_threshold_ms))
                        
                        for row in cur.fetchall():
                            # Skip if we've already seen this query
                            if row['queryid'] in last_query_ids:
                                continue
                            
                            # Log slow query
                            query_type = row['query_type'].lower() if row['query_type'] else 'unknown'
                            self.logger.warning(
                                f"Slow query detected ({row['avg_time_ms']:.2f}ms avg, {row['max_time_ms']:.2f}ms max): "
                                f"{query_type.upper()} {row['query'].strip()[:100]}..."
                            )
                            
                            # Update metrics
                            self.slow_query_count.labels(
                                database=db_name,
                                query_type=query_type
                            ).inc()
                            
                            # Remember this query ID
                            last_query_ids[row['queryid']] = True
                
                # Sleep before next check
                time.sleep(60)
            
            except Exception as e:
                self.logger.error(f"Error monitoring query performance: {e}", exc_info=True)
                time.sleep(60)  # Sleep before retry

# Example usage
if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='Database metrics collection for Reversible Meta-Synthesis')
    parser.add_argument('--conn-string', type=str, required=True, help='PostgreSQL connection string')
    parser.add_argument('--poll-interval', type=int, default=60, help='Metrics polling interval in seconds')
    parser.add_argument('--slow-query-threshold', type=int, default=500, help='Slow query threshold in milliseconds')
    args = parser.parse_args()
    
    try:
        # Start metrics collector
        metrics_collector = DatabaseMetricsCollector(
            args.conn_string,
            poll_interval=args.poll_interval
        )
        metrics_collector.start()
        
        # Start query performance monitor
        query_monitor = QueryPerformanceMonitor(
            args.conn_string,
            slow_query_threshold_ms=args.slow_query_threshold
        )
        query_monitor.start()
        
        # Keep main thread running
        print(f"Database metrics collection started (poll interval: {args.poll_interval}s)")
        print(f"Slow query monitoring started (threshold: {args.slow_query_threshold}ms)")
        print("Press Ctrl+C to stop...")
        
        while True:
            time.sleep(1)
    
    except KeyboardInterrupt:
        print("Stopping...")
        metrics_collector.stop()
        query_monitor.stop()
        print("Stopped")
