-- Database schema for the Reversible Meta-Synthesis service

-- Users and Authentication
CREATE TABLE users (
    user_id UUID PRIMARY KEY,
    username VARCHAR(50) NOT NULL UNIQUE,
    email VARCHAR(255) NOT NULL UNIQUE,
    password_hash VARCHAR(255) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    last_login TIMESTAMP,
    role VARCHAR(20) NOT NULL DEFAULT 'user'
);

CREATE TABLE api_keys (
    key_id UUID PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    api_key VARCHAR(64) NOT NULL UNIQUE,
    description VARCHAR(255),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP,
    last_used TIMESTAMP,
    is_active BOOLEAN NOT NULL DEFAULT TRUE
);

-- Synthesis Jobs
CREATE TABLE synthesis_jobs (
    job_id UUID PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES users(user_id),
    language VARCHAR(20) NOT NULL,
    explanation_level INTEGER NOT NULL DEFAULT 1,
    status VARCHAR(20) NOT NULL,
    progress INTEGER NOT NULL DEFAULT 0,
    status_message TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    completed_at TIMESTAMP,
    worker_id VARCHAR(50)
);

-- Job specifications are stored in a separate table due to their size
CREATE TABLE job_specifications (
    job_id UUID PRIMARY KEY REFERENCES synthesis_jobs(job_id) ON DELETE CASCADE,
    examples JSONB NOT NULL,
    constraints JSONB
);

-- Synthesis Results
CREATE TABLE synthesis_results (
    result_id UUID PRIMARY KEY,
    job_id UUID NOT NULL UNIQUE REFERENCES synthesis_jobs(job_id) ON DELETE CASCADE,
    program TEXT NOT NULL,
    explanation_tree JSONB NOT NULL,
    metrics JSONB NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Example Programs & Patterns
CREATE TABLE example_programs (
    program_id UUID PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    language VARCHAR(20) NOT NULL,
    program_code TEXT NOT NULL,
    is_public BOOLEAN NOT NULL DEFAULT FALSE,
    user_id UUID REFERENCES users(user_id) ON DELETE SET NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    tags VARCHAR(50)[] NOT NULL DEFAULT '{}'
);

-- Input-Output Examples
CREATE TABLE io_examples (
    example_id UUID PRIMARY KEY,
    program_id UUID REFERENCES example_programs(program_id) ON DELETE CASCADE,
    input JSONB NOT NULL,
    output JSONB NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Explanation Templates (stored in a specialized structure for quick retrieval)
CREATE TABLE explanation_templates (
    template_id UUID PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    language VARCHAR(20) NOT NULL,
    decomposition_level INTEGER NOT NULL,
    pattern JSONB NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    usage_count INTEGER NOT NULL DEFAULT 0
);

-- Synthesis Strategies (learned from past syntheses)
CREATE TABLE synthesis_strategies (
    strategy_id UUID PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    problem_features JSONB NOT NULL, -- Features that trigger this strategy
    steps JSONB NOT NULL, -- Sequence of synthesis steps
    success_rate REAL NOT NULL DEFAULT 0,
    average_synthesis_time INTEGER, -- in milliseconds
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    usage_count INTEGER NOT NULL DEFAULT 0
);

-- Synthesis Usage Analytics
CREATE TABLE synthesis_analytics (
    analytics_id UUID PRIMARY KEY,
    job_id UUID REFERENCES synthesis_jobs(job_id) ON DELETE SET NULL,
    user_id UUID REFERENCES users(user_id) ON DELETE SET NULL,
    language VARCHAR(20) NOT NULL,
    explanation_level INTEGER NOT NULL,
    synthesis_time INTEGER NOT NULL, -- in milliseconds
    cpu_usage REAL NOT NULL, -- percentage
    memory_usage BIGINT NOT NULL, -- in bytes
    program_complexity REAL NOT NULL,
    strategy_id UUID REFERENCES synthesis_strategies(strategy_id) ON DELETE SET NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Knowledge Base for Program Patterns
CREATE TABLE program_patterns (
    pattern_id UUID PRIMARY KEY,
    language VARCHAR(20) NOT NULL,
    pattern_type VARCHAR(50) NOT NULL, -- e.g., recursive, iterative, higher-order
    pattern_structure JSONB NOT NULL, -- structure of the pattern
    description TEXT,
    usage_count INTEGER NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Program Pattern Examples
CREATE TABLE pattern_examples (
    example_id UUID PRIMARY KEY,
    pattern_id UUID REFERENCES program_patterns(pattern_id) ON DELETE CASCADE,
    program_code TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for performance optimization

-- For job lookup and filtering
CREATE INDEX idx_synthesis_jobs_user_id ON synthesis_jobs(user_id);
CREATE INDEX idx_synthesis_jobs_status ON synthesis_jobs(status);
CREATE INDEX idx_synthesis_jobs_language ON synthesis_jobs(language);
CREATE INDEX idx_synthesis_jobs_created_at ON synthesis_jobs(created_at);

-- For example programs
CREATE INDEX idx_example_programs_language ON example_programs(language);
CREATE INDEX idx_example_programs_is_public ON example_programs(is_public);
CREATE INDEX idx_example_programs_user_id ON example_programs(user_id);
CREATE INDEX idx_example_programs_tags ON example_programs USING GIN(tags);

-- For explanation templates
CREATE INDEX idx_explanation_templates_language ON explanation_templates(language);
CREATE INDEX idx_explanation_templates_decomposition_level ON explanation_templates(decomposition_level);

-- For synthesis strategies
CREATE INDEX idx_synthesis_strategies_problem_features ON synthesis_strategies USING GIN(problem_features jsonb_path_ops);

-- For analytics
CREATE INDEX idx_synthesis_analytics_user_id ON synthesis_analytics(user_id);
CREATE INDEX idx_synthesis_analytics_language ON synthesis_analytics(language);
CREATE INDEX idx_synthesis_analytics_created_at ON synthesis_analytics(created_at);

-- Full-text search for program code and descriptions
CREATE EXTENSION IF NOT EXISTS pg_trgm;
CREATE INDEX idx_example_programs_code_search ON example_programs USING GIN(program_code gin_trgm_ops);
CREATE INDEX idx_example_programs_description_search ON example_programs USING GIN(description gin_trgm_ops);

-- Specialized Views

-- View for active synthesis jobs
CREATE VIEW active_synthesis_jobs AS
SELECT sj.job_id, sj.user_id, u.username, sj.language, sj.explanation_level, 
       sj.status, sj.progress, sj.status_message, sj.created_at, sj.worker_id
FROM synthesis_jobs sj
JOIN users u ON sj.user_id = u.user_id
WHERE sj.status IN ('queued', 'running')
ORDER BY sj.created_at;

-- View for synthesis performance analytics
CREATE VIEW synthesis_performance AS
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

-- View for user synthesis history
CREATE VIEW user_synthesis_history AS
SELECT 
    u.user_id,
    u.username,
    COUNT(sj.job_id) as total_jobs,
    COUNT(CASE WHEN sj.status = 'completed' THEN 1 END) as completed_jobs,
    COUNT(CASE WHEN sj.status = 'failed' THEN 1 END) as failed_jobs,
    AVG(sa.synthesis_time) as avg_synthesis_time,
    MAX(sj.created_at) as last_synthesis
FROM 
    users u
LEFT JOIN 
    synthesis_jobs sj ON u.user_id = sj.user_id
LEFT JOIN 
    synthesis_analytics sa ON sj.job_id = sa.job_id
GROUP BY 
    u.user_id, u.username;

-- Functions and Procedures

-- Function to calculate program complexity
CREATE OR REPLACE FUNCTION calculate_program_complexity(program_code TEXT)
RETURNS REAL AS $$
DECLARE
    line_count INTEGER;
    token_count INTEGER;
    nested_depth INTEGER;
    complexity REAL;
BEGIN
    -- Count lines
    line_count := array_length(string_to_array(program_code, E'\n'), 1);
    
    -- Count tokens (simplistic approach)
    token_count := array_length(regexp_split_to_array(program_code, E'\\s+'), 1);
    
    -- Calculate nesting depth (simplistic approach for lisp-like languages)
    nested_depth := length(program_code) - length(replace(program_code, '(', ''));
    
    -- Calculate complexity with weights
    complexity := (0.1 * line_count) + (0.05 * token_count) + (0.2 * nested_depth);
    
    RETURN GREATEST(1.0, complexity);
END;
$$ LANGUAGE plpgsql;

-- Procedure to clean up old jobs
CREATE OR REPLACE PROCEDURE cleanup_old_jobs(retention_days INTEGER)
LANGUAGE plpgsql AS $$
BEGIN
    -- Archive completed jobs
    INSERT INTO archived_synthesis_jobs
    SELECT * FROM synthesis_jobs
    WHERE status IN ('completed', 'failed')
    AND created_at < NOW() - (retention_days || ' days')::INTERVAL;
    
    -- Delete from main table
    DELETE FROM synthesis_jobs
    WHERE status IN ('completed', 'failed')
    AND created_at < NOW() - (retention_days || ' days')::INTERVAL;
    
    COMMIT;
END;
$$;

-- Trigger to update metrics when a synthesis job completes
CREATE OR REPLACE FUNCTION update_strategy_metrics()
RETURNS TRIGGER AS $$
BEGIN
    IF NEW.strategy_id IS NOT NULL THEN
        -- Update strategy success rate and performance metrics
        UPDATE synthesis_strategies
        SET 
            usage_count = usage_count + 1,
            success_rate = (success_rate * usage_count + CASE WHEN NEW.job_id IN (SELECT job_id FROM synthesis_jobs WHERE status = 'completed') THEN 1.0 ELSE 0.0 END) / (usage_count + 1),
            average_synthesis_time = (average_synthesis_time * usage_count + NEW.synthesis_time) / (usage_count + 1),
            updated_at = NOW()
        WHERE 
            strategy_id = NEW.strategy_id;
    END IF;
    
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_update_strategy_metrics
AFTER INSERT ON synthesis_analytics
FOR EACH ROW
EXECUTE FUNCTION update_strategy_metrics();

-- Function to find similar synthesis problems
CREATE OR REPLACE FUNCTION find_similar_problems(
    p_input_features JSONB,
    p_language VARCHAR,
    p_limit INTEGER DEFAULT 5
)
RETURNS TABLE (
    job_id UUID,
    similarity REAL,
    examples JSONB,
    program TEXT
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        sj.job_id,
        -- Calculate similarity between problem features
        -- This is a simplified approach; in reality would use more sophisticated similarity metrics
        (jsonb_array_length(p_input_features) - jsonb_array_length(p_input_features - js.examples)) / 
        jsonb_array_length(p_input_features)::REAL AS similarity,
        js.examples,
        sr.program
    FROM 
        synthesis_jobs sj
    JOIN 
        job_specifications js ON sj.job_id = js.job_id
    JOIN 
        synthesis_results sr ON sj.job_id = sr.job_id
    WHERE 
        sj.language = p_language
        AND sj.status = 'completed'
    ORDER BY 
        similarity DESC
    LIMIT p_limit;
END;
$$ LANGUAGE plpgsql;
