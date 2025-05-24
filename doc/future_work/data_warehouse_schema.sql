-- Data Warehouse Schema for Reversible Meta-Synthesis Analytics

-- Time dimension
CREATE TABLE dim_time (
    time_id SERIAL PRIMARY KEY,
    date_actual DATE NOT NULL,
    day_of_week INTEGER NOT NULL,
    day_name VARCHAR(10) NOT NULL,
    day_of_month INTEGER NOT NULL,
    day_of_year INTEGER NOT NULL,
    week_of_year INTEGER NOT NULL,
    month_actual INTEGER NOT NULL,
    month_name VARCHAR(10) NOT NULL,
    quarter_actual INTEGER NOT NULL,
    year_actual INTEGER NOT NULL,
    is_weekend BOOLEAN NOT NULL,
    UNIQUE(date_actual)
);

-- Language dimension
CREATE TABLE dim_language (
    language_id SERIAL PRIMARY KEY,
    language_name VARCHAR(50) NOT NULL,
    language_version VARCHAR(20),
    language_family VARCHAR(50),
    paradigm VARCHAR(50),
    UNIQUE(language_name, language_version)
);

-- Explanation level dimension
CREATE TABLE dim_explanation_level (
    level_id SERIAL PRIMARY KEY,
    level_value INTEGER NOT NULL,
    level_name VARCHAR(50) NOT NULL,
    level_description TEXT,
    UNIQUE(level_value)
);

-- User dimension
CREATE TABLE dim_user (
    user_id UUID PRIMARY KEY,
    username VARCHAR(50) NOT NULL,
    user_type VARCHAR(20) NOT NULL,
    organization VARCHAR(100),
    country VARCHAR(50),
    created_at TIMESTAMP NOT NULL,
    is_active BOOLEAN NOT NULL,
    UNIQUE(username)
);

-- Program pattern dimension
CREATE TABLE dim_program_pattern (
    pattern_id SERIAL PRIMARY KEY,
    pattern_type VARCHAR(50) NOT NULL,
    pattern_name VARCHAR(100) NOT NULL,
    pattern_structure JSONB,
    complexity_level INTEGER NOT NULL,
    description TEXT
);

-- Synthesis job fact table
CREATE TABLE fact_synthesis_job (
    job_id UUID PRIMARY KEY,
    user_id UUID NOT NULL REFERENCES dim_user(user_id),
    language_id INTEGER NOT NULL REFERENCES dim_language(language_id),
    explanation_level_id INTEGER NOT NULL REFERENCES dim_explanation_level(level_id),
    time_id INTEGER NOT NULL REFERENCES dim_time(time_id),
    pattern_id INTEGER REFERENCES dim_program_pattern(pattern_id),
    
    -- Facts
    synthesis_time_seconds INTEGER NOT NULL,
    cpu_usage_percent REAL NOT NULL,
    memory_usage_mb REAL NOT NULL,
    explanation_tree_nodes INTEGER NOT NULL,
    program_complexity REAL NOT NULL,
    program_size_bytes INTEGER NOT NULL,
    status VARCHAR(20) NOT NULL,
    success BOOLEAN NOT NULL,
    
    -- Additional attributes
    query_count INTEGER NOT NULL,
    example_count INTEGER NOT NULL,
    decomposition_usage JSONB,
    strategy_name VARCHAR(100),
    
    UNIQUE(job_id)
);

-- Synthesis performance over time
CREATE TABLE fact_synthesis_performance (
    time_id INTEGER NOT NULL REFERENCES dim_time(time_id),
    language_id INTEGER NOT NULL REFERENCES dim_language(language_id),
    explanation_level_id INTEGER NOT NULL REFERENCES dim_explanation_level(level_id),
    
    -- Facts
    job_count INTEGER NOT NULL,
    success_rate REAL NOT NULL,
    avg_synthesis_time REAL NOT NULL,
    min_synthesis_time REAL NOT NULL,
    max_synthesis_time REAL NOT NULL,
    avg_cpu_usage REAL NOT NULL,
    avg_memory_usage REAL NOT NULL,
    avg_program_complexity REAL NOT NULL,
    
    PRIMARY KEY (time_id, language_id, explanation_level_id)
);

-- Example usage patterns
CREATE TABLE fact_example_usage (
    time_id INTEGER NOT NULL REFERENCES dim_time(time_id),
    language_id INTEGER NOT NULL REFERENCES dim_language(language_id),
    pattern_id INTEGER NOT NULL REFERENCES dim_program_pattern(pattern_id),
    
    -- Facts
    usage_count INTEGER NOT NULL,
    success_rate REAL NOT NULL,
    avg_synthesis_time REAL NOT NULL,
    
    PRIMARY KEY (time_id, language_id, pattern_id)
);

-- User activity
CREATE TABLE fact_user_activity (
    time_id INTEGER NOT NULL REFERENCES dim_time(time_id),
    user_id UUID NOT NULL REFERENCES dim_user(user_id),
    
    -- Facts
    job_count INTEGER NOT NULL,
    success_rate REAL NOT NULL,
    avg_synthesis_time REAL NOT NULL,
    distinct_languages INTEGER NOT NULL,
    
    PRIMARY KEY (time_id, user_id)
);

-- ETL Procedures

-- Populate Time Dimension
CREATE OR REPLACE PROCEDURE populate_dim_time(start_date DATE, end_date DATE)
LANGUAGE plpgsql AS $$
DECLARE
    curr_date DATE := start_date;
BEGIN
    WHILE curr_date <= end_date LOOP
        INSERT INTO dim_time (
            date_actual,
            day_of_week,
            day_name,
            day_of_month,
            day_of_year,
            week_of_year,
            month_actual,
            month_name,
            quarter_actual,
            year_actual,
            is_weekend
        )
        VALUES (
            curr_date,
            EXTRACT(DOW FROM curr_date),
            TO_CHAR(curr_date, 'Day'),
            EXTRACT(DAY FROM curr_date),
            EXTRACT(DOY FROM curr_date),
            EXTRACT(WEEK FROM curr_date),
            EXTRACT(MONTH FROM curr_date),
            TO_CHAR(curr_date, 'Month'),
            EXTRACT(QUARTER FROM curr_date),
            EXTRACT(YEAR FROM curr_date),
            CASE WHEN EXTRACT(DOW FROM curr_date) IN (0, 6) THEN TRUE ELSE FALSE END
        )
        ON CONFLICT (date_actual) DO NOTHING;
        
        curr_date := curr_date + INTERVAL '1 day';
    END LOOP;
END;
$$;

-- ETL for Synthesis Jobs
CREATE OR REPLACE PROCEDURE etl_synthesis_jobs()
LANGUAGE plpgsql AS $$
BEGIN
    -- Ensure time dimension has entries for all job dates
    INSERT INTO dim_time (
        date_actual,
        day_of_week,
        day_name,
        day_of_month,
        day_of_year,
        week_of_year,
        month_actual,
        month_name,
        quarter_actual,
        year_actual,
        is_weekend
    )
    SELECT DISTINCT
        DATE(created_at),
        EXTRACT(DOW FROM created_at),
        TO_CHAR(created_at, 'Day'),
        EXTRACT(DAY FROM created_at),
        EXTRACT(DOY FROM created_at),
        EXTRACT(WEEK FROM created_at),
        EXTRACT(MONTH FROM created_at),
        TO_CHAR(created_at, 'Month'),
        EXTRACT(QUARTER FROM created_at),
        EXTRACT(YEAR FROM created_at),
        CASE WHEN EXTRACT(DOW FROM created_at) IN (0, 6) THEN TRUE ELSE FALSE END
    FROM
        synthesis_jobs
    WHERE
        DATE(created_at) NOT IN (SELECT date_actual FROM dim_time)
    ORDER BY
        1;
    
    -- Ensure language dimension has all languages
    INSERT INTO dim_language (language_name, language_version, language_family, paradigm)
    SELECT DISTINCT
        language,
        NULL,
        CASE
            WHEN language IN ('prolog') THEN 'Logic Programming'
            WHEN language IN ('clojure', 'scheme', 'hy') THEN 'Lisp Family'
            WHEN language IN ('python', 'haskell') THEN language
            ELSE 'Other'
        END,
        CASE
            WHEN language IN ('prolog') THEN 'Logic'
            WHEN language IN ('clojure', 'scheme', 'hy', 'haskell') THEN 'Functional'
            WHEN language IN ('python') THEN 'Multi-paradigm'
            ELSE 'Other'
        END
    FROM
        synthesis_jobs
    WHERE
        language NOT IN (SELECT language_name FROM dim_language)
    ORDER BY
        1;
    
    -- Ensure explanation level dimension has all levels
    INSERT INTO dim_explanation_level (level_value, level_name, level_description)
    SELECT DISTINCT
        explanation_level,
        CASE
            WHEN explanation_level = 0 THEN 'Program-Level'
            WHEN explanation_level = 1 THEN 'Clause-Level'
            WHEN explanation_level = 2 THEN 'Clause-Structure-Level'
            WHEN explanation_level = 3 THEN 'Term-Level'
            ELSE 'Unknown'
        END,
        CASE
            WHEN explanation_level = 0 THEN 'Whole program explanation'
            WHEN explanation_level = 1 THEN 'Decomposed at clause level'
            WHEN explanation_level = 2 THEN 'Decomposed at clause structure level'
            WHEN explanation_level = 3 THEN 'Decomposed at term level'
            ELSE 'Unknown level'
        END
    FROM
        synthesis_jobs
    WHERE
        explanation_level NOT IN (SELECT level_value FROM dim_explanation_level)
    ORDER BY
        1;
    
    -- Ensure user dimension has all users
    INSERT INTO dim_user (user_id, username, user_type, organization, country, created_at, is_active)
    SELECT DISTINCT
        u.user_id,
        u.username,
        u.role,
        NULL,
        NULL,
        u.created_at,
        TRUE
    FROM
        users u
    JOIN
        synthesis_jobs sj ON u.user_id = sj.user_id
    WHERE
        u.user_id NOT IN (SELECT user_id FROM dim_user)
    ORDER BY
        1;
    
    -- Insert synthesis job facts
    INSERT INTO fact_synthesis_job (
        job_id,
        user_id,
        language_id,
        explanation_level_id,
        time_id,
        pattern_id,
        synthesis_time_seconds,
        cpu_usage_percent,
        memory_usage_mb,
        explanation_tree_nodes,
        program_complexity,
        program_size_bytes,
        status,
        success,
        query_count,
        example_count,
        decomposition_usage,
        strategy_name
    )
    SELECT
        sj.job_id,
        sj.user_id,
        dl.language_id,
        del.level_id,
        dt.time_id,
        NULL, -- pattern_id (to be updated later)
        sa.synthesis_time / 1000, -- convert ms to seconds
        sa.cpu_usage,
        sa.memory_usage / (1024*1024), -- convert bytes to MB
        COALESCE(
            (SELECT COUNT(*) FROM JSONB_ARRAY_ELEMENTS(sr.explanation_tree->'children')),
            0
        ),
        sa.program_complexity,
        LENGTH(sr.program),
        sj.status,
        CASE WHEN sj.status = 'completed' THEN TRUE ELSE FALSE END,
        1, -- query_count (placeholder)
        COALESCE(
(SELECT JSONB_ARRAY_LENGTH(js.examples) FROM job_specifications js WHERE js.job_id = sj.job_id),
            0
        ),
        NULL, -- decomposition_usage (to be updated later)
        NULL  -- strategy_name (to be updated later)
    FROM
        synthesis_jobs sj
    JOIN
        synthesis_analytics sa ON sj.job_id = sa.job_id
    LEFT JOIN
        synthesis_results sr ON sj.job_id = sr.job_id
    JOIN
        dim_language dl ON sj.language = dl.language_name
    JOIN
        dim_explanation_level del ON sj.explanation_level = del.level_value
    JOIN
        dim_time dt ON DATE(sj.created_at) = dt.date_actual
    LEFT JOIN
        dim_user du ON sj.user_id = du.user_id
    WHERE
        sj.job_id NOT IN (SELECT job_id FROM fact_synthesis_job)
    ORDER BY
        sj.created_at;
    
    -- Update pattern_id for synthesis jobs where applicable
    UPDATE fact_synthesis_job fsj
    SET pattern_id = dpp.pattern_id
    FROM
        synthesis_results sr
    JOIN
        program_patterns pp ON sr.pattern_id = pp.pattern_id
    JOIN
        dim_program_pattern dpp ON pp.pattern_type = dpp.pattern_type
    WHERE
        fsj.job_id = sr.job_id
        AND fsj.pattern_id IS NULL;
    
    -- Populate decomposition_usage
    UPDATE fact_synthesis_job fsj
    SET decomposition_usage = sr.metrics->'decomposition_usage'
    FROM
        synthesis_results sr
    WHERE
        fsj.job_id = sr.job_id
        AND fsj.decomposition_usage IS NULL
        AND sr.metrics ? 'decomposition_usage';
    
    -- Populate strategy_name
    UPDATE fact_synthesis_job fsj
    SET strategy_name = ss.name
    FROM
        synthesis_analytics sa
    JOIN
        synthesis_strategies ss ON sa.strategy_id = ss.strategy_id
    WHERE
        fsj.job_id = sa.job_id
        AND fsj.strategy_name IS NULL;
END;
$$;

-- ETL for Synthesis Performance
CREATE OR REPLACE PROCEDURE etl_synthesis_performance()
LANGUAGE plpgsql AS $$
BEGIN
    -- Insert/update synthesis performance facts
    INSERT INTO fact_synthesis_performance (
        time_id,
        language_id,
        explanation_level_id,
        job_count,
        success_rate,
        avg_synthesis_time,
        min_synthesis_time,
        max_synthesis_time,
        avg_cpu_usage,
        avg_memory_usage,
        avg_program_complexity
    )
    SELECT
        dt.time_id,
        dl.language_id,
        del.level_id,
        COUNT(fsj.job_id),
        AVG(CASE WHEN fsj.success THEN 1.0 ELSE 0.0 END),
        AVG(fsj.synthesis_time_seconds),
        MIN(fsj.synthesis_time_seconds),
        MAX(fsj.synthesis_time_seconds),
        AVG(fsj.cpu_usage_percent),
        AVG(fsj.memory_usage_mb),
        AVG(fsj.program_complexity)
    FROM
        fact_synthesis_job fsj
    JOIN
        dim_time dt ON fsj.time_id = dt.time_id
    JOIN
        dim_language dl ON fsj.language_id = dl.language_id
    JOIN
        dim_explanation_level del ON fsj.explanation_level_id = del.level_id
    GROUP BY
        dt.time_id,
        dl.language_id,
        del.level_id
    ON CONFLICT (time_id, language_id, explanation_level_id)
    DO UPDATE SET
        job_count = EXCLUDED.job_count,
        success_rate = EXCLUDED.success_rate,
        avg_synthesis_time = EXCLUDED.avg_synthesis_time,
        min_synthesis_time = EXCLUDED.min_synthesis_time,
        max_synthesis_time = EXCLUDED.max_synthesis_time,
        avg_cpu_usage = EXCLUDED.avg_cpu_usage,
        avg_memory_usage = EXCLUDED.avg_memory_usage,
        avg_program_complexity = EXCLUDED.avg_program_complexity;
END;
$$;

-- ETL for Example Usage
CREATE OR REPLACE PROCEDURE etl_example_usage()
LANGUAGE plpgsql AS $$
BEGIN
    -- Insert/update example usage facts
    INSERT INTO fact_example_usage (
        time_id,
        language_id,
        pattern_id,
        usage_count,
        success_rate,
        avg_synthesis_time
    )
    SELECT
        dt.time_id,
        dl.language_id,
        dpp.pattern_id,
        COUNT(fsj.job_id),
        AVG(CASE WHEN fsj.success THEN 1.0 ELSE 0.0 END),
        AVG(fsj.synthesis_time_seconds)
    FROM
        fact_synthesis_job fsj
    JOIN
        dim_time dt ON fsj.time_id = dt.time_id
    JOIN
        dim_language dl ON fsj.language_id = dl.language_id
    JOIN
        dim_program_pattern dpp ON fsj.pattern_id = dpp.pattern_id
    WHERE
        fsj.pattern_id IS NOT NULL
    GROUP BY
        dt.time_id,
        dl.language_id,
        dpp.pattern_id
    ON CONFLICT (time_id, language_id, pattern_id)
    DO UPDATE SET
        usage_count = EXCLUDED.usage_count,
        success_rate = EXCLUDED.success_rate,
        avg_synthesis_time = EXCLUDED.avg_synthesis_time;
END;
$$;

-- ETL for User Activity
CREATE OR REPLACE PROCEDURE etl_user_activity()
LANGUAGE plpgsql AS $$
BEGIN
    -- Insert/update user activity facts
    INSERT INTO fact_user_activity (
        time_id,
        user_id,
        job_count,
        success_rate,
        avg_synthesis_time,
        distinct_languages
    )
    SELECT
        dt.time_id,
        du.user_id,
        COUNT(fsj.job_id),
        AVG(CASE WHEN fsj.success THEN 1.0 ELSE 0.0 END),
        AVG(fsj.synthesis_time_seconds),
        COUNT(DISTINCT fsj.language_id)
    FROM
        fact_synthesis_job fsj
    JOIN
        dim_time dt ON fsj.time_id = dt.time_id
    JOIN
        dim_user du ON fsj.user_id = du.user_id
    GROUP BY
        dt.time_id,
        du.user_id
    ON CONFLICT (time_id, user_id)
    DO UPDATE SET
        job_count = EXCLUDED.job_count,
        success_rate = EXCLUDED.success_rate,
        avg_synthesis_time = EXCLUDED.avg_synthesis_time,
        distinct_languages = EXCLUDED.distinct_languages;
END;
$$;

-- ETL master procedure
CREATE OR REPLACE PROCEDURE etl_master()
LANGUAGE plpgsql AS $$
BEGIN
    -- Run all ETL procedures in sequence
    CALL etl_synthesis_jobs();
    CALL etl_synthesis_performance();
    CALL etl_example_usage();
    CALL etl_user_activity();
END;
$$;

-- Analytical views

-- View for synthesis success rate trends
CREATE VIEW view_synthesis_success_trends AS
SELECT
    dt.year_actual,
    dt.quarter_actual,
    dt.month_actual,
    dt.month_name,
    dl.language_name,
    del.level_name,
    SUM(fsp.job_count) AS total_jobs,
    AVG(fsp.success_rate) AS avg_success_rate,
    AVG(fsp.avg_synthesis_time) AS avg_synthesis_time
FROM
    fact_synthesis_performance fsp
JOIN
    dim_time dt ON fsp.time_id = dt.time_id
JOIN
    dim_language dl ON fsp.language_id = dl.language_id
JOIN
    dim_explanation_level del ON fsp.explanation_level_id = del.level_id
GROUP BY
    dt.year_actual,
    dt.quarter_actual,
    dt.month_actual,
    dt.month_name,
    dl.language_name,
    del.level_name
ORDER BY
    dt.year_actual,
    dt.quarter_actual,
    dt.month_actual,
    dl.language_name,
    del.level_name;

-- View for language comparison
CREATE VIEW view_language_comparison AS
SELECT
    dl.language_name,
    dl.paradigm,
    COUNT(fsj.job_id) AS total_jobs,
    AVG(CASE WHEN fsj.success THEN 1.0 ELSE 0.0 END) AS success_rate,
    AVG(fsj.synthesis_time_seconds) AS avg_synthesis_time,
    AVG(fsj.program_complexity) AS avg_program_complexity,
    AVG(fsj.program_size_bytes) AS avg_program_size
FROM
    fact_synthesis_job fsj
JOIN
    dim_language dl ON fsj.language_id = dl.language_id
GROUP BY
    dl.language_name,
    dl.paradigm
ORDER BY
    total_jobs DESC;

-- View for explanation level effectiveness
CREATE VIEW view_explanation_level_effectiveness AS
SELECT
    del.level_name,
    dl.language_name,
    COUNT(fsj.job_id) AS total_jobs,
    AVG(CASE WHEN fsj.success THEN 1.0 ELSE 0.0 END) AS success_rate,
    AVG(fsj.synthesis_time_seconds) AS avg_synthesis_time,
    AVG(fsj.program_complexity) AS avg_program_complexity
FROM
    fact_synthesis_job fsj
JOIN
    dim_explanation_level del ON fsj.explanation_level_id = del.level_id
JOIN
    dim_language dl ON fsj.language_id = dl.language_id
GROUP BY
    del.level_name,
    dl.language_name
ORDER BY
    del.level_name,
    dl.language_name;

-- View for user engagement
CREATE VIEW view_user_engagement AS
SELECT
    du.username,
    du.user_type,
    COUNT(DISTINCT dt.year_actual || '-' || dt.month_actual) AS active_months,
    SUM(fua.job_count) AS total_jobs,
    AVG(fua.success_rate) AS avg_success_rate,
    AVG(fua.avg_synthesis_time) AS avg_synthesis_time,
    MAX(fua.distinct_languages) AS max_distinct_languages
FROM
    fact_user_activity fua
JOIN
    dim_user du ON fua.user_id = du.user_id
JOIN
    dim_time dt ON fua.time_id = dt.time_id
WHERE
    dt.date_actual >= CURRENT_DATE - INTERVAL '12 months'
GROUP BY
    du.username,
    du.user_type
ORDER BY
    total_jobs DESC;

-- View for pattern effectiveness
CREATE VIEW view_pattern_effectiveness AS
SELECT
    dpp.pattern_name,
    dpp.pattern_type,
    dl.language_name,
    SUM(feu.usage_count) AS total_usage,
    AVG(feu.success_rate) AS avg_success_rate,
    AVG(feu.avg_synthesis_time) AS avg_synthesis_time
FROM
    fact_example_usage feu
JOIN
    dim_program_pattern dpp ON feu.pattern_id = dpp.pattern_id
JOIN
    dim_language dl ON feu.language_id = dl.language_id
GROUP BY
    dpp.pattern_name,
    dpp.pattern_type,
    dl.language_name
ORDER BY
    total_usage DESC;
