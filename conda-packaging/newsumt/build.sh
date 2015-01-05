export CFLAGS="$CFLAGS -fPIC"
export PKG_VERSION="1.0.2"

if [ -n "$OSX_ARCH" ]; then
    export LDFLAGS="$LDFLAGS -dynamiclib -undefined dynamic_lookup -Xlinker -rpath -Xlinker @loader_path/../../../../../lib/openmdao"
fi

python setup.py build_ext
python setup.py install
