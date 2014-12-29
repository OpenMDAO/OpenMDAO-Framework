if [ -n "$OSX_ARCH" ]; then
    export LDFLAGS="$LDFLAGS  -undefined dynamic_lookup"
fi

python setup.py install
