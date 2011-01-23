#!/bin/sh
set -e

# This is a script to be run as part of make.sh. The only time you'd
# want to run it by itself is if you're trying to cross-compile the
# system or if you're doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-target-1.sh

LANG=C
LC_ALL=C
export LANG LC_ALL

# Link the runtime system and symbol table (.nm) file.
#
# This final stage of C build has to come after running the cross compiler,
# before the second genesis: undefined foreign symbols in cross-compiled Lisp sources
# are now available.
echo //building runtime system executable

# $GNUMAKE -C src/runtime depend
$GNUMAKE -C src/runtime all
