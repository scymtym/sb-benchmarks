# To be sourced by shell scripts in the benchmark suite.

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

# Make the shell bomb out whenever an unset shell variable is used.
# Note that scripts may toggle this if necessary.
set -u

if [ -z "${SBCL_WC}" ] ; then
  echo "SBCL_WC not set; set it to the root directory of your SBCL working copy"
  exit 1
fi

# Initialize variables.
set -a # export all variables at assignment-time.
# Note: any script that uses the variables that name files should
# quote them (with double quotes), to contend with whitespace.
SBCL_HOME="${BENCHMARK_SBCL_HOME:-$SBCL_WC/contrib}"
SBCL_CORE="${BENCHMARK_SBCL_CORE:-$SBCL_WC/output/sbcl.core}"
SBCL_RUNTIME="${BENCHMARK_SBCL_RUNTIME:-$SBCL_WC/src/runtime/sbcl}"
SBCL_ARGS="${BENCHMARK_SBCL_ARGS:---noinform --no-sysinit --no-userinit --noprint --disable-debugger}"

# "Ten four" is the closest numerical slang I can find to "OK", so
# it's the Unix status value that we expect from a successful test.
# (Of course, zero is the usual success value, but we don't want to
# use that because SBCL returns that by default, so we might think
# we passed a test when in fact some error caused us to exit SBCL
# in a weird unexpected way. In contrast, 104 is unlikely to be
# returned unless we exit through the intended explicit "test
# successful" path.
EXIT_BENCHMARK_WIN=104

LANG=C
LC_ALL=C
set +a

run_sbcl () (
    set -u
    if [ $# -gt 0 ]; then
        "$SBCL_RUNTIME" --core "$SBCL_CORE" $SBCL_ARGS "$@"
    else
        "$SBCL_RUNTIME" --core "$SBCL_CORE" $SBCL_ARGS
    fi
)
