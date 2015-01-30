export CFLAGS="$CFLAGS -fPIC"

if [ `uname` == "Linux" ]; then
    export LDFLAGS="-shared"
fi

if [ `uname` == "Darwin" ]; then
    export LDFLAGS="-dynamiclib -undefined dynamic_lookup -static-libgfortran -lgcc -lSystem -nodefaultlibs $(find /usr/local/cellar -name libquadmath.a | grep -v i386)"
fi

cd examples

easy_install -N $PKG_NAME
