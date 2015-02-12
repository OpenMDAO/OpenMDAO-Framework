export CFLAGS="$CFLAGS -fPIC"

if [ "$(uname -s)" = "Linux" ]; then
    export LDFLAGS="$LDFLAGS -shared"
fi

if [ -n "$OSX_ARCH" ]; then
    export LDFLAGS="$LDFLAGS -dynamiclib -undefined dynamic_lookup -static-libgfortran -lgcc -lSystem -nodefaultlibs `find /usr/local/Cellar -name libquadmath.a | grep -v i386`"
fi

${PYTHON} setup.py build_ext
${PYTHON} setup.py install

python -c "from pkg_resources import get_distribution; print get_distribution('$PKG_NAME').version" > __conda_version__.txt
