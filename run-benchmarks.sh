#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

if [ "$1" = "--help" ]; then
  cat <<EOF
Run the benchmarks in this directory.

Usage: run-benchmarks.sh [OPTIONS] [BENCHMARK FILES]

Valid options are as follows:

  --report-style STYLE   Name of the style that should be used to format
                         test results. Supported STYLEs: describe,
                         sexp.

  --report-target TARGET Controls where the benchmark result report
                         should go. When supplied, TARGET is treated
                         as the name of a file into which the report
                         should be written. When the option is
                         omitted, the report is written to standard
                         output.

  --compare-to COMMIT    Include a comparison of the generated benchmark
                         results against results recorded for COMMIT.

  --no-color             Disable coloring of results.

If no BENCHMARK FILES are specified, run all benchmarks (i.e. files
matching the pattern *.benchmark.lisp).

The environment variable SBCL_WC has be set to the name of a directory
containing a SBCL working copy.

Comparision example (assuming SBCL working copy is initial pwd and
sb-benchmarks resides in the benchmarks sub-directory):

  $ export SBCL_WC=$(pwd)
  $ git checkout AAAAAAAA && sh make.sh
  $ ( cd benchmarks && ./run-benchmarks ) # record results for commit AAAAAAAA
  $ git checkout BBBBBBBB && sh make.sh
  $ ( cd benchmarks && ./run-benchmarks --report-style plot --compate-to AAAAAAAA )

EOF
  exit 0
fi

. ./subr.sh

echo /running benchmarks on \'$SBCL_RUNTIME --core $SBCL_CORE $SBCL_ARGS\'

tenfour () {
    if [ $1 = $EXIT_BENCHMARK_WIN ]; then
        echo ok
    else
        echo test failed, expected $EXIT_BENCHMARK_WIN return code, got $1
        exit 1
    fi
}
set +u
run_sbcl \
    --eval '(with-compilation-unit () (load "run-benchmarks.lisp"))' \
    --eval '(run-benchmarks::run-all)' $*

tenfour $?

echo '//apparent success (reached end of run-tests.sh normally)'
date
