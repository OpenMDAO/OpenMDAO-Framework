export CFLAGS="$CFLAGS -fPIC"
export LDFLAGS="$LDFLAGS -shared"
export PKG_VERSION="1.1.1"


if [ -n "$OSX_ARCH" ]; then
    export LDFLAGS="$LDFLAGS  -undefined dynamic_lookup  -static-libgfortran"
fi

python setup.py install
