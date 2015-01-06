export CFLAGS="$CFLAGS -fPIC"

if [ "$(uname -s)" = "Linux" ]; then
    export LDFLAGS="$LDFLAGS -shared"
fi

if [ -n "$OSX_ARCH" ]; then
    export LDFLAGS="$LDFLAGS -dynamiclib -undefined dynamic_lookup -Xlinker -rpath -Xlinker @loader_path/../../../../../lib/openmdao"
fi

python setup.py build_ext
python setup.py install
