if [ -n "$OSX_ARCH" ]; then
    mkdir -p $PREFIX/lib/openmdao
    cp /usr/local/Cellar/gcc/4.9.2/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libgfortran.3.dylib $PREFIX/lib/openmdao/.
    
    install_name_tool -id @rpath/libgfortran.3.dylib $PREFIX/lib/openmdao/libgfortran.3.dylib
    install_name_tool -change /usr/local/Cellar/gcc/4.9.2/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libquadmath.0.dylib @rpath/libquadmath.0.dylib $PREFIX/lib/openmdao/libgfortran.3.dylib
    install_name_tool -change /usr/local/Cellar/gcc/4.9.2/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib $PREFIX/lib/openmdao/libgfortran.3.dylib

    cp /usr/local/Cellar/gcc/4.9.2/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libquadmath.0.dylib $PREFIX/lib/openmdao/.
    install_name_tool -id @rpath/libquadmath.0.dylib $PREFIX/lib/openmdao/libquadmath.0.dylib
    install_name_tool -change /usr/local/Cellar/gcc/4.9.2/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib $PREFIX/lib/openmdao/libquadmath.0.dylib

    cp /usr/local/Cellar/gcc/4.9.2/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libgcc_s.1.dylib $PREFIX/lib/openmdao/.
    install_name_tool -id @rpath/libgcc_s.1.dylib $PREFIX/lib/openmdao/libgcc_s.1.dylib
fi
