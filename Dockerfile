FROM openjdk:17-slim as base

# Install required packages
RUN apt-get update && apt-get install -y \
    swi-prolog \
    python3 \
    python3-pip \
    guile-3.0 \
    leiningen \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install Hy
RUN pip3 install hy==1.0.0

# Set up working directory
WORKDIR /app

# Copy project files
COPY . .

# Set up Clojure environment
RUN cd /app && lein deps

# Define environment variables
ENV PATH="/app/bin:${PATH}"

# Create a script to run tests
RUN echo '#!/bin/bash\n\
echo "Running Prolog tests..."\n\
cd /app/tests/prolog && swipl -s run_tests.pl\n\
\n\
echo "Running Hy tests..."\n\
cd /app/tests/hy && hy run_tests.hy\n\
\n\
echo "Running Scheme tests..."\n\
cd /app/tests/scheme && guile run-tests.scm\n\
\n\
echo "Running Clojure tests..."\n\
cd /app && clojure -M:test\n\
' > /app/bin/run-tests && chmod +x /app/bin/run-tests

# Create a script to run examples
RUN echo '#!/bin/bash\n\
echo "Running Prolog examples..."\n\
cd /app/examples/prolog\n\
for f in *.pl; do swipl -q -l "$f" -t "main"; done\n\
\n\
echo "Running Hy examples..."\n\
cd /app/examples/hy\n\
for f in *.hy; do hy "$f"; done\n\
\n\
echo "Running Scheme examples..."\n\
cd /app/examples/scheme\n\
for f in *.scm; do guile "$f"; done\n\
\n\
echo "Running Clojure examples..."\n\
cd /app && clojure -M:examples\n\
' > /app/bin/run-examples && chmod +x /app/bin/run-examples

# Expose port for potential web interface
EXPOSE 8080

# Default command
CMD ["bash"]
