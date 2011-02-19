#!/bin/sh
# A simple shell-script to run the freshly build SBCL without
# installing it.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

set -e

BASE=`dirname "$0"`
if (readlink -f "${BASE}") >/dev/null 2>&1; then
    BASE=`readlink -f ${BASE}`
else
    opwd=`pwd`
    cd "${BASE}"
    BASE=`pwd`
    cd "${opwd}"
fi

. "$BASE/guess-environment.sh"

NATIVEBASE=$BASE
if [ "$OSTYPE" = "cygwin" ]
then
    BASE=`cygpath -w "$BASE"`
fi

if [ "$OSTYPE" = "wine" ]
then
    SBCL_RUNTIME_EXECUTABLE=sbcl.exe
else
    SBCL_RUNTIME_EXECUTABLE=sbcl
fi
CORE_DEFINED=no

for arg in $*; do
    case $arg in
        --core)
          CORE_DEFINED=yes
          ;;
        --help)
          echo "usage: run-sbcl.sh sbcl-options*"
          echo
          echo "Runs SBCL from the build directory or binary tarball without need for"
          echo "installation. Except for --help, accepts all the same command-line options"
          echo "as SBCL does."
          echo
          exit 1
          ;;
    esac
done

ARGUMENTS=""

if [ "$CORE_DEFINED" = "no" ]; then
    ARGUMENTS="--core "$BASE"/output/sbcl.core"
fi

if [ -x "$NATIVEBASE"/src/runtime/$SBCL_RUNTIME_EXECUTABLE -a -f "$NATIVEBASE"/output/sbcl.core ] ; then
    echo "(running SBCL from: $BASE)" 1>&2
    SBCL_HOME="$BASE"/contrib $SBCL_XRUN \
    "$NATIVEBASE"/src/runtime/$SBCL_RUNTIME_EXECUTABLE $ARGUMENTS "$@"
else
    echo "No built SBCL here ($BASE): run 'sh make.sh' first!"
    exit 1
fi
