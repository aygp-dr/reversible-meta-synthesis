#!/usr/bin/env python3
"""
Database optimization techniques for the Reversible Meta-Synthesis service.
This module provides specialized functions for working with synthesis data
efficiently in the database.
"""

import json
import uuid
import psycopg2
from psycopg2.extras import Json, DictCursor
from typing import Dict, List, Any, Tuple, Optional
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

class SynthesisDatabase:
    def __init__(self, connection_string: str):
        """Initialize the database connection"""
        self.conn_string = connection_string
        
    def connect(self):
        """Create a database connection"""
        return psycopg2.connect(
            self.conn_string,
            cursor_factory=DictCursor
        )
    
    def store_program_with_examples(self, 
                                   program_code: str, 
                                   language: str,
                                   examples: List[Dict[str, Any]],
                                   name: str,
                                   description: str = None,
                                   user_id: uuid.UUID = None,
                                   is_public: bool = False,
                                   tags: List[str] = None) -> uuid.UUID:
        """
        Store a program with its examples in the database
        
        Args:
            program_code: The source code of the program
            language: Programming language (prolog, hy, scheme, clojure, etc.)
            examples: List of input-output examples
            name: Name of the program
            description: Description of the program
            user_id: ID of the user who created the program
            is_public: Whether the program is publicly accessible
            tags: List of tags to categorize the program
            
        Returns:
            UUID of the created program
        """
        program_id = uuid.uuid4()
        
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Insert the program
                cur.execute(
                    """
                    INSERT INTO example_programs (
                        program_id, name, description, language, program_code, 
                        is_public, user_id, tags
                    ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
                    RETURNING program_id
                    """,
                    (
                        program_id, name, description, language, program_code,
                        is_public, user_id, tags or []
                    )
                )
                
                # Insert examples
                for example in examples:
                    cur.execute(
                        """
                        INSERT INTO io_examples (
                            example_id, program_id, input, output
                        ) VALUES (%s, %s, %s, %s)
                        """,
                        (
                            uuid.uuid4(), program_id, 
                            Json(example['input']), Json(example['output'])
                        )
                    )
                
                # Extract program patterns
                self._extract_and_store_patterns(cur, program_code, language, program_id)
                
                conn.commit()
                
        return program_id
    
    def _extract_and_store_patterns(self, 
                                   cursor, 
                                   program_code: str, 
                                   language: str,
                                   program_id: uuid.UUID) -> None:
        """
        Extract patterns from a program and store them in the database
        
        Args:
            cursor: Database cursor
            program_code: The source code of the program
            language: Programming language
            program_id: ID of the program
        """
        # This is a simplified implementation
        # In practice, would use language-specific parsers and pattern recognition
        
        # Determine pattern type based on simple heuristics
        pattern_type = self._determine_pattern_type(program_code, language)
        
        # Extract pattern structure
        pattern_structure = self._extract_pattern_structure(program_code, language)
        
        # Check if similar pattern exists
        cursor.execute(
            """
            SELECT pattern_id, usage_count 
            FROM program_patterns
            WHERE language = %s AND pattern_type = %s
            """,
            (language, pattern_type)
        )
        
        existing_pattern = cursor.fetchone()
        
        if existing_pattern:
            # Update existing pattern
            pattern_id = existing_pattern['pattern_id']
            cursor.execute(
                """
                UPDATE program_patterns
                SET usage_count = usage_count + 1,
                    updated_at = NOW()
                WHERE pattern_id = %s
                """,
                (pattern_id,)
            )
        else:
            # Create new pattern
            pattern_id = uuid.uuid4()
            cursor.execute(
                """
                INSERT INTO program_patterns (
                    pattern_id, language, pattern_type, 
                    pattern_structure, usage_count
                ) VALUES (%s, %s, %s, %s, 1)
                """,
                (
                    pattern_id, language, pattern_type,
                    Json(pattern_structure)
                )
            )
        
        # Store pattern example
        cursor.execute(
            """
            INSERT INTO pattern_examples (
                example_id, pattern_id, program_code
            ) VALUES (%s, %s, %s)
            """,
            (uuid.uuid4(), pattern_id, program_code)
        )
    
    def _determine_pattern_type(self, program_code: str, language: str) -> str:
        """
        Determine the pattern type of a program
        
        Args:
            program_code: The source code of the program
            language: Programming language
            
        Returns:
            Pattern type (e.g., recursive, iterative, higher-order)
        """
        # Simplified implementation based on simple heuristics
        # Would use language-specific parsers in practice
        
        if language in ('prolog', 'hy', 'scheme', 'clojure'):
            # Check for recursion patterns in lisp-like languages
            if program_code.count('(defun ') > 0 or program_code.count('(defn ') > 0:
                if program_code.count(' :- ') > 0 and program_code.count(program_code.split(' :- ')[0].strip()) > 1:
                    return 'recursive'
            
            # Check for higher-order functions
            higher_order_indicators = ['map', 'filter', 'reduce', 'apply', 'foldl', 'foldr']
            if any(indicator in program_code for indicator in higher_order_indicators):
                return 'higher-order'
            
            return 'simple'
        else:
            # Default for unknown languages
            return 'unknown'
    
    def _extract_pattern_structure(self, program_code: str, language: str) -> Dict[str, Any]:
        """
        Extract the structure of a program pattern
        
        Args:
            program_code: The source code of the program
            language: Programming language
            
        Returns:
            Structure of the pattern as a JSON-serializable dictionary
        """
        # Simplified implementation
        # Would use language-specific parsers in practice
        
        # Count occurrences of key syntax elements
        structure = {
            "length": len(program_code),
            "lines": program_code.count('\n') + 1,
            "functions": program_code.count('defun') + program_code.count('defn'),
            "recursion": program_code.count('self') + program_code.count(' :- '),
            "conditionals": program_code.count('if') + program_code.count('cond'),
            "loops": program_code.count('for') + program_code.count('while'),
            "higher_order": program_code.count('map') + program_code.count('filter') + program_code.count('reduce')
        }
        
        return structure
    
    def find_similar_programs(self, 
                             query_code: str, 
                             language: str,
                             limit: int = 5) -> List[Dict[str, Any]]:
        """
        Find programs similar to the given code using TF-IDF and cosine similarity
        
        Args:
            query_code: Program code to find similar programs for
            language: Programming language
            limit: Maximum number of results to return
            
        Returns:
            List of similar programs with similarity scores
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get all programs in the same language
                cur.execute(
                    """
                    SELECT program_id, name, program_code
                    FROM example_programs
                    WHERE language = %s
                    """,
                    (language,)
                )
                
                programs = cur.fetchall()
                
                if not programs:
                    return []
                
                # Create corpus for TF-IDF
                corpus = [program['program_code'] for program in programs]
                corpus.append(query_code)  # Add query code to the end
                
                # Create TF-IDF vectorizer
                vectorizer = TfidfVectorizer(
                    analyzer='word',
                    token_pattern=r'[^\s]+',  # Treat program code tokens appropriately
                    min_df=1
                )
                
                # Calculate TF-IDF matrix
                tfidf_matrix = vectorizer.fit_transform(corpus)
                
                # Calculate cosine similarity between query and all programs
                query_index = len(corpus) - 1  # Query is the last item
                cosine_similarities = cosine_similarity(
                    tfidf_matrix[query_index:query_index+1], 
                    tfidf_matrix[:query_index]
                ).flatten()
                
                # Create result list with program IDs and similarity scores
                similar_programs = []
                for idx, similarity in enumerate(cosine_similarities):
                    if similarity > 0.1:  # Threshold to filter irrelevant results
                        program = programs[idx]
                        similar_programs.append({
                            "program_id": program['program_id'],
                            "name": program['name'],
                            "similarity": float(similarity),
                            "program_code": program['program_code']
                        })
                
                # Sort by similarity (descending) and limit results
                similar_programs.sort(key=lambda x: x['similarity'], reverse=True)
                return similar_programs[:limit]
    
    def store_explanation_template(self, 
                                  name: str,
                                  description: str,
                                  language: str,
                                  decomposition_level: int,
                                  pattern: Dict[str, Any]) -> uuid.UUID:
        """
        Store an explanation template in the database
        
        Args:
            name: Name of the template
            description: Description of the template
            language: Programming language
            decomposition_level: Explanation decomposition level (0-3)
            pattern: Pattern structure as a JSON-serializable dictionary
            
        Returns:
            UUID of the created template
        """
        template_id = uuid.uuid4()
        
        with self.connect() as conn:
            with conn.cursor() as cur:
                cur.execute(
                    """
                    INSERT INTO explanation_templates (
                        template_id, name, description, language,
                        decomposition_level, pattern
                    ) VALUES (%s, %s, %s, %s, %s, %s)
                    RETURNING template_id
                    """,
                    (
                        template_id, name, description, language,
                        decomposition_level, Json(pattern)
                    )
                )
                
                conn.commit()
                
        return template_id
    
    def find_matching_explanation_template(self,
                                         language: str,
                                         decomposition_level: int,
                                         goal_pattern: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """
        Find a matching explanation template based on the goal pattern
        
        Args:
            language: Programming language
            decomposition_level: Explanation decomposition level (0-3)
            goal_pattern: Pattern to match
            
        Returns:
            Matching template or None if no match found
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get templates matching language and decomposition level
                cur.execute(
                    """
                    SELECT template_id, name, description, pattern
                    FROM explanation_templates
                    WHERE language = %s AND decomposition_level = %s
                    ORDER BY usage_count DESC
                    """,
                    (language, decomposition_level)
                )
                
                templates = cur.fetchall()
                
                # Find best matching template using pattern similarity
                best_match = None
                best_similarity = 0.0
                
                for template in templates:
                    similarity = self._calculate_pattern_similarity(
                        goal_pattern, 
                        template['pattern']
                    )
                    
                    if similarity > best_similarity and similarity > 0.7:  # Threshold
                        best_similarity = similarity
                        best_match = {
                            "template_id": template['template_id'],
                            "name": template['name'],
                            "description": template['description'],
                            "pattern": template['pattern'],
                            "similarity": similarity
                        }
                
                # Update usage count for matching template
                if best_match:
                    cur.execute(
                        """
                        UPDATE explanation_templates
                        SET usage_count = usage_count + 1
                        WHERE template_id = %s
                        """,
                        (best_match['template_id'],)
                    )
                    
                    conn.commit()
                
                return best_match
    
    def _calculate_pattern_similarity(self, 
                                     pattern1: Dict[str, Any], 
                                     pattern2: Dict[str, Any]) -> float:
        """
        Calculate similarity between two patterns
        
        Args:
            pattern1: First pattern
            pattern2: Second pattern
            
        Returns:
            Similarity score between 0.0 and 1.0
        """
        # Simplified implementation
        # Would use more sophisticated similarity metrics in practice
        
        # Find common keys
        common_keys = set(pattern1.keys()) & set(pattern2.keys())
        
        if not common_keys:
            return 0.0
        
        # Calculate similarity for each common key
        similarities = []
        for key in common_keys:
            if isinstance(pattern1[key], (int, float)) and isinstance(pattern2[key], (int, float)):
                # Numerical features - calculate relative difference
                max_val = max(abs(pattern1[key]), abs(pattern2[key]))
                if max_val > 0:
                    diff = abs(pattern1[key] - pattern2[key]) / max_val
                    similarities.append(1.0 - min(diff, 1.0))
                else:
                    similarities.append(1.0)  # Both zero
            elif isinstance(pattern1[key], str) and isinstance(pattern2[key], str):
                # String features - calculate string similarity
                if pattern1[key] == pattern2[key]:
                    similarities.append(1.0)
                else:
                    similarities.append(0.0)
            elif isinstance(pattern1[key], (list, tuple)) and isinstance(pattern2[key], (list, tuple)):
                # List features - calculate overlap
                set1 = set(pattern1[key])
                set2 = set(pattern2[key])
                if set1 or set2:
                    overlap = len(set1 & set2) / len(set1 | set2)
                    similarities.append(overlap)
                else:
                    similarities.append(1.0)  # Both empty
            elif isinstance(pattern1[key], dict) and isinstance(pattern2[key], dict):
                # Nested dictionaries - recursive similarity
                similarities.append(self._calculate_pattern_similarity(pattern1[key], pattern2[key]))
            else:
                # Different types
                similarities.append(0.0)
        
        # Return average similarity
        return sum(similarities) / len(similarities) if similarities else 0.0
    
    def get_synthesis_statistics(self) -> Dict[str, Any]:
        """
        Get overall synthesis statistics
        
        Returns:
            Dictionary with synthesis statistics
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Get overall counts
                cur.execute(
                    """
                    SELECT
                        (SELECT COUNT(*) FROM synthesis_jobs) AS total_jobs,
                        (SELECT COUNT(*) FROM synthesis_jobs WHERE status = 'completed') AS completed_jobs,
                        (SELECT COUNT(*) FROM synthesis_jobs WHERE status = 'failed') AS failed_jobs,
                        (SELECT COUNT(*) FROM users) AS total_users,
                        (SELECT COUNT(*) FROM example_programs) AS total_programs,
                        (SELECT COUNT(*) FROM explanation_templates) AS total_templates,
                        (SELECT COUNT(*) FROM program_patterns) AS total_patterns
                    """
                )
                
                stats = cur.fetchone()
                
                # Get synthesis performance by language
                cur.execute(
                    """
                    SELECT 
                        language,
                        COUNT(*) as job_count,
                        AVG(synthesis_time) as avg_synthesis_time,
                        MIN(synthesis_time) as min_synthesis_time,
                        MAX(synthesis_time) as max_synthesis_time,
                        AVG(program_complexity) as avg_program_complexity
                    FROM 
                        synthesis_analytics
                    GROUP BY 
                        language
                    """
                )
                
                language_stats = cur.fetchall()
                
                # Get synthesis performance by explanation level
                cur.execute(
                    """
                    SELECT 
                        explanation_level,
                        COUNT(*) as job_count,
                        AVG(synthesis_time) as avg_synthesis_time,
                        AVG(program_complexity) as avg_program_complexity
                    FROM 
                        synthesis_analytics
                    GROUP BY 
                        explanation_level
                    ORDER BY
                        explanation_level
                    """
                )
                
                level_stats = cur.fetchall()
                
                # Return combined statistics
                return {
                    "overall": dict(stats),
                    "by_language": [dict(ls) for ls in language_stats],
                    "by_explanation_level": [dict(ls) for ls in level_stats]
                }
    
    def optimize_database(self) -> None:
        """
        Perform database optimization operations
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Clean up old jobs
                cur.execute("CALL cleanup_old_jobs(30)")
                
                # Analyze tables for query optimization
                cur.execute("ANALYZE")
                
                # Vacuum to reclaim space
                conn.autocommit = True
                cur.execute("VACUUM ANALYZE")
                
                # Update database statistics
                cur.execute("VACUUM FULL")
    
    def create_database_partitions(self) -> None:
        """
        Create partitions for large tables to improve performance
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Create partitioned table for synthesis analytics by date
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS synthesis_analytics_partitioned (
                        analytics_id UUID NOT NULL,
                        job_id UUID,
                        user_id UUID,
                        language VARCHAR(20) NOT NULL,
                        explanation_level INTEGER NOT NULL,
                        synthesis_time INTEGER NOT NULL,
                        cpu_usage REAL NOT NULL,
                        memory_usage BIGINT NOT NULL,
                        program_complexity REAL NOT NULL,
                        strategy_id UUID,
                        created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
                    ) PARTITION BY RANGE (created_at);
                """)
                
                # Create monthly partitions for the past year and future year
                # In practice, would dynamically create these based on actual data
                for i in range(-12, 13):  # -12 months to +12 months
                    month_start = f"DATE_TRUNC('month', CURRENT_DATE + INTERVAL '{i} month')"
                    month_end = f"DATE_TRUNC('month', CURRENT_DATE + INTERVAL '{i+1} month')"
                    
                    partition_name = f"synthesis_analytics_y{datetime.now().year}_m{(datetime.now().month + i) % 12 + 1}"
                    
                    # Create partition
                    cur.execute(f"""
                        CREATE TABLE IF NOT EXISTS {partition_name} PARTITION OF synthesis_analytics_partitioned
                        FOR VALUES FROM ({month_start}) TO ({month_end});
                    """)
                    
                    # Create indexes on the partition
                    cur.execute(f"""
                        CREATE INDEX IF NOT EXISTS idx_{partition_name}_job_id ON {partition_name}(job_id);
                        CREATE INDEX IF NOT EXISTS idx_{partition_name}_user_id ON {partition_name}(user_id);
                        CREATE INDEX IF NOT EXISTS idx_{partition_name}_language ON {partition_name}(language);
                    """)
                
                conn.commit()
    
    def implement_database_caching(self) -> None:
        """
        Implement database caching for frequently accessed data
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Create materialized view for frequently accessed synthesis statistics
                cur.execute("""
                    CREATE MATERIALIZED VIEW IF NOT EXISTS cached_synthesis_stats AS
                    SELECT 
                        language,
                        explanation_level,
                        COUNT(*) as job_count,
                        AVG(synthesis_time) as avg_synthesis_time,
                        MIN(synthesis_time) as min_synthesis_time,
                        MAX(synthesis_time) as max_synthesis_time,
                        AVG(cpu_usage) as avg_cpu_usage,
                        AVG(memory_usage) as avg_memory_usage,
                        AVG(program_complexity) as avg_program_complexity,
                        COUNT(CASE WHEN sa.strategy_id IS NOT NULL THEN 1 END) as strategy_usage_count
                    FROM 
                        synthesis_analytics sa
                    GROUP BY 
                        language, explanation_level;
                """)
                
                # Create index on the materialized view
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_cached_synthesis_stats_language
                    ON cached_synthesis_stats(language);
                    
                    CREATE INDEX IF NOT EXISTS idx_cached_synthesis_stats_explanation_level
                    ON cached_synthesis_stats(explanation_level);
                """)
                
                # Create function to refresh the materialized view
                cur.execute("""
                    CREATE OR REPLACE FUNCTION refresh_cached_synthesis_stats()
                    RETURNS TRIGGER AS $$
                    BEGIN
                        REFRESH MATERIALIZED VIEW CONCURRENTLY cached_synthesis_stats;
                        RETURN NULL;
                    END;
                    $$ LANGUAGE plpgsql;
                """)
                
                # Create trigger to refresh the materialized view
                cur.execute("""
                    DROP TRIGGER IF EXISTS trigger_refresh_cached_synthesis_stats ON synthesis_analytics;
                    
                    CREATE TRIGGER trigger_refresh_cached_synthesis_stats
                    AFTER INSERT OR UPDATE OR DELETE ON synthesis_analytics
                    FOR EACH STATEMENT
                    EXECUTE FUNCTION refresh_cached_synthesis_stats();
                """)
                
                conn.commit()
    
    def optimize_query_performance(self) -> None:
        """
        Optimize query performance for common operations
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Create specialized indexes for common queries
                
                # Index for finding programs by example inputs/outputs
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_io_examples_input_jsonb
                    ON io_examples USING GIN(input jsonb_path_ops);
                    
                    CREATE INDEX IF NOT EXISTS idx_io_examples_output_jsonb
                    ON io_examples USING GIN(output jsonb_path_ops);
                """)
                
                # Hash index for exact matches on constant values
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_example_programs_language_hash
                    ON example_programs USING HASH(language);
                """)
                
                # Create function index for program complexity
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_example_programs_complexity
                    ON example_programs(calculate_program_complexity(program_code));
                """)
                
                # Create index for jsonb containment operations
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_synthesis_strategies_features_containment
                    ON synthesis_strategies USING GIN(problem_features jsonb_path_ops);
                """)
                
                conn.commit()
    
    def implement_data_retention_policies(self) -> None:
        """
        Implement data retention policies to manage database size
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Create table for archived jobs
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS archived_synthesis_jobs (
                        job_id UUID PRIMARY KEY,
                        user_id UUID NOT NULL,
                        language VARCHAR(20) NOT NULL,
                        explanation_level INTEGER NOT NULL DEFAULT 1,
                        status VARCHAR(20) NOT NULL,
                        progress INTEGER NOT NULL DEFAULT 0,
                        status_message TEXT,
                        created_at TIMESTAMP NOT NULL,
                        updated_at TIMESTAMP NOT NULL,
                        completed_at TIMESTAMP,
                        worker_id VARCHAR(50),
                        archived_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
                    );
                """)
                
                # Create table for archived results (compressed)
                cur.execute("""
                    CREATE TABLE IF NOT EXISTS archived_synthesis_results (
                        result_id UUID PRIMARY KEY,
                        job_id UUID NOT NULL,
                        program TEXT NOT NULL,
                        explanation_tree JSONB NOT NULL,
                        metrics JSONB NOT NULL,
                        created_at TIMESTAMP NOT NULL,
                        archived_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
                        compressed_data BYTEA
                    );
                """)
                
                # Create functions to compress and decompress data
                cur.execute("""
                    CREATE OR REPLACE FUNCTION compress_synthesis_result(
                        p_result_id UUID,
                        p_job_id UUID,
                        p_program TEXT,
                        p_explanation_tree JSONB,
                        p_metrics JSONB,
                        p_created_at TIMESTAMP
                    ) RETURNS BYTEA AS $$
                    DECLARE
                        v_data TEXT;
                        v_compressed BYTEA;
                    BEGIN
                        -- Serialize data as JSON
                        v_data := json_build_object(
                            'result_id', p_result_id,
                            'job_id', p_job_id,
                            'program', p_program,
                            'explanation_tree', p_explanation_tree,
                            'metrics', p_metrics,
                            'created_at', p_created_at
                        )::text;
                        
                        -- Compress using GZIP
                        v_compressed := COMPRESS(v_data::bytea, 'gzip');
                        
                        RETURN v_compressed;
                    END;
                    $$ LANGUAGE plpgsql;
                """)
                
                cur.execute("""
                    CREATE OR REPLACE FUNCTION decompress_synthesis_result(
                        p_compressed_data BYTEA
                    ) RETURNS TABLE (
                        result_id UUID,
                        job_id UUID,
                        program TEXT,
                        explanation_tree JSONB,
                        metrics JSONB,
                        created_at TIMESTAMP
                    ) AS $$
                    DECLARE
                        v_decompressed TEXT;
                        v_json JSONB;
                    BEGIN
                        -- Decompress data
                        v_decompressed := CONVERT_FROM(DECOMPRESS(p_compressed_data, 'gzip'), 'UTF8');
                        
                        -- Parse JSON
                        v_json := v_decompressed::jsonb;
                        
                        -- Return data
                        RETURN QUERY SELECT
                            (v_json->>'result_id')::UUID,
                            (v_json->>'job_id')::UUID,
                            v_json->>'program',
                            v_json->'explanation_tree',
                            v_json->'metrics',
                            (v_json->>'created_at')::TIMESTAMP;
                    END;
                    $$ LANGUAGE plpgsql;
                """)
                
                # Create stored procedure for archiving old data
                cur.execute("""
                    CREATE OR REPLACE PROCEDURE archive_old_synthesis_data(
                        p_retention_days INTEGER
                    ) AS $$
                    DECLARE
                        v_cutoff_date TIMESTAMP;
                        v_job_rec RECORD;
                        v_compressed_data BYTEA;
                    BEGIN
                        -- Calculate cutoff date
                        v_cutoff_date := CURRENT_TIMESTAMP - (p_retention_days || ' days')::INTERVAL;
                        
                        -- Find completed jobs older than cutoff date
                        FOR v_job_rec IN (
                            SELECT j.job_id, j.user_id, j.language, j.explanation_level,
                                   j.status, j.progress, j.status_message, j.created_at,
                                   j.updated_at, j.completed_at, j.worker_id,
                                   r.result_id, r.program, r.explanation_tree, r.metrics, r.created_at as result_created_at
                            FROM synthesis_jobs j
                            JOIN synthesis_results r ON j.job_id = r.job_id
                            WHERE j.status IN ('completed', 'failed')
                            AND j.created_at < v_cutoff_date
                        ) LOOP
                            -- Compress result data
                            v_compressed_data := compress_synthesis_result(
                                v_job_rec.result_id,
                                v_job_rec.job_id,
                                v_job_rec.program,
                                v_job_rec.explanation_tree,
                                v_job_rec.metrics,
                                v_job_rec.result_created_at
                            );
                            
                            -- Insert into archived tables
                            INSERT INTO archived_synthesis_jobs (
                                job_id, user_id, language, explanation_level, status,
                                progress, status_message, created_at, updated_at,
                                completed_at, worker_id
                            ) VALUES (
                                v_job_rec.job_id, v_job_rec.user_id, v_job_rec.language,
                                v_job_rec.explanation_level, v_job_rec.status, v_job_rec.progress,
                                v_job_rec.status_message, v_job_rec.created_at,
                                v_job_rec.updated_at, v_job_rec.completed_at, v_job_rec.worker_id
                            );
                            
                            INSERT INTO archived_synthesis_results (
                                result_id, job_id, program, explanation_tree, metrics,
                                created_at, compressed_data
                            ) VALUES (
                                v_job_rec.result_id, v_job_rec.job_id, v_job_rec.program,
                                v_job_rec.explanation_tree, v_job_rec.metrics,
                                v_job_rec.result_created_at, v_compressed_data
                            );
                            
                            -- Delete from original tables
                            DELETE FROM synthesis_results WHERE result_id = v_job_rec.result_id;
                            DELETE FROM synthesis_jobs WHERE job_id = v_job_rec.job_id;
                        END LOOP;
                        
                        COMMIT;
                    END;
                    $$ LANGUAGE plpgsql;
                """)
                
                conn.commit()
    
    def create_distributed_indices(self) -> None:
        """
        Create distributed indices for large-scale deployments
        """
        # This would typically be implemented using PostgreSQL extensions
        # such as Citus or using a distributed database system
        
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Check if Citus extension is available
                cur.execute("SELECT EXISTS(SELECT 1 FROM pg_available_extensions WHERE name = 'citus')")
                has_citus = cur.fetchone()[0]
                
                if has_citus:
                    # Create distributed tables
                    cur.execute("CREATE EXTENSION IF NOT EXISTS citus")
                    
                    # Distribute the synthesis_jobs table
                    cur.execute("""
                        SELECT create_distributed_table(
                            'synthesis_jobs', 'user_id'
                        );
                    """)
                    
                    # Distribute the synthesis_results table
                    cur.execute("""
                        SELECT create_distributed_table(
                            'synthesis_results', 'job_id'
                        );
                    """)
                    
                    # Distribute the example_programs table
                    cur.execute("""
                        SELECT create_distributed_table(
                            'example_programs', 'user_id'
                        );
                    """)
                    
                    conn.commit()
                else:
                    print("Citus extension not available for distributed indices")
    
    def create_search_indices(self) -> None:
        """
        Create specialized search indices for program code and descriptions
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Check if PostgreSQL full-text search extensions are available
                cur.execute("SELECT EXISTS(SELECT 1 FROM pg_available_extensions WHERE name = 'pg_trgm')")
                has_trgm = cur.fetchone()[0]
                
                if has_trgm:
                    # Create trigram indices for fuzzy text search
                    cur.execute("CREATE EXTENSION IF NOT EXISTS pg_trgm")
                    
                    # Create GIN index for program code search
                    cur.execute("""
                        CREATE INDEX IF NOT EXISTS idx_example_programs_code_search
                        ON example_programs USING GIN(program_code gin_trgm_ops);
                    """)
                    
                    # Create GIN index for description search
                    cur.execute("""
                        CREATE INDEX IF NOT EXISTS idx_example_programs_description_search
                        ON example_programs USING GIN(description gin_trgm_ops);
                    """)
                    
                    conn.commit()
                
                # Create full-text search vectors
                cur.execute("""
                    ALTER TABLE example_programs
                    ADD COLUMN IF NOT EXISTS search_vector tsvector
                    GENERATED ALWAYS AS (
                        setweight(to_tsvector('english', coalesce(name, '')), 'A') ||
                        setweight(to_tsvector('english', coalesce(description, '')), 'B') ||
                        setweight(to_tsvector('english', coalesce(program_code, '')), 'C')
                    ) STORED;
                """)
                
                # Create GIN index for full-text search
                cur.execute("""
                    CREATE INDEX IF NOT EXISTS idx_example_programs_search_vector
                    ON example_programs USING GIN(search_vector);
                """)
                
                conn.commit()
    
    def search_programs(self, 
                      query: str, 
                      language: str = None,
                      tags: List[str] = None,
                      limit: int = 10) -> List[Dict[str, Any]]:
        """
        Search for programs using full-text search
        
        Args:
            query: Search query
            language: Filter by programming language
            tags: Filter by tags
            limit: Maximum number of results
            
        Returns:
            List of matching programs
        """
        with self.connect() as conn:
            with conn.cursor() as cur:
                # Build the SQL query
                sql = """
                    SELECT 
                        p.program_id, 
                        p.name, 
                        p.description, 
                        p.language, 
                        p.tags,
                        p.is_public,
                        ts_rank(p.search_vector, to_tsquery('english', %s)) AS rank
                    FROM 
                        example_programs p
                    WHERE 
                        p.search_vector @@ to_tsquery('english', %s)
                """
                
                params = []
                
                # Preprocess the query for full-text search
                processed_query = ' & '.join(query.split())
                params.extend([processed_query, processed_query])
                
                # Add language filter if specified
                if language:
                    sql += " AND p.language = %s"
                    params.append(language)
                
                # Add tags filter if specified
                if tags and len(tags) > 0:
                    placeholders = ', '.join(['%s' for _ in tags])
                    sql += f" AND p.tags && ARRAY[{placeholders}]"
                    params.extend(tags)
                
                # Add public filter (always include public programs)
                sql += " AND p.is_public = TRUE"
                
                # Order by rank and limit results
                sql += " ORDER BY rank DESC LIMIT %s"
                params.append(limit)
                
                # Execute the query
                cur.execute(sql, params)
                
                # Return results
                results = []
                for row in cur.fetchall():
                    results.append({
                        "program_id": row['program_id'],
                        "name": row['name'],
                        "description": row['description'],
                        "language": row['language'],
                        "tags": row['tags'],
                        "rank": float(row['rank'])
                    })
                
                return results
