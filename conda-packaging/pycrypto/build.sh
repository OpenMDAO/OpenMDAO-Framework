#!/bin/bash
if [ "$(uname -s)" = "Linux" ]; then
    export LDFLAGS="$LDFLAGS -shared"
fi

if [ -n "$OSX_ARCH" ]; then
    export LDFLAGS="$LDFLAGS  -undefined dynamic_lookup"
fi

$PYTHON setup.py install

# Add more build steps here, if they are necessary.

# See
# http://docs.continuum.io/conda/build.html
# for a list of environment variables that are set during the build process.
