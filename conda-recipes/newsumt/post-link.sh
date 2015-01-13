if [ `uname` == "Darwin" ]; then 
	install_name_tool -change /usr/local/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libgfortran.3.dylib @rpath/libgfortran.3.dylib $PREFIX/lib/python2.7/site-packages/$PKG_NAME-$PKG_VERSION-py2.7-macosx-10.5-x86_64.egg/$PKG_NAME/newsumtinterruptible.so
	install_name_tool -change /usr/local/Cellar/gcc/4.9.2/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libgcc_s.1.dylib @rpath/libgcc_s.1.dylib $PREFIX/lib/python2.7/site-packages/$PKG_NAME-$PKG_VERSION-py2.7-macosx-10.5-x86_64.egg/$PKG_NAME/newsumtinterruptible.so
	install_name_tool -change /usr/local/lib/gcc/x86_64-apple-darwin13.4.0/4.9.2/libquadmath.0.dylib @rpath/libquadmath.0.dylib $PREFIX/lib/python2.7/site-packages/$PKG_NAME-$PKG_VERSION-py2.7-macosx-10.5-x86_64.egg/$PKG_NAME/newsumtinterruptible.so 
fi
