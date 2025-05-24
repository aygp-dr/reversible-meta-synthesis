#!/bin/bash
emacs --batch -l org --eval "(org-babel-tangle-file \"SETUP.org\")"
chmod +x install-linux.sh install-freebsd.sh install-macos.sh
chmod +x examples/prolog/*.pl examples/hy/*.hy examples/scheme/*.scm
chmod +x tests/prolog/run_tests.pl tests/hy/run_tests.hy tests/scheme/run-tests.scm
echo "All files tangled successfully!"
