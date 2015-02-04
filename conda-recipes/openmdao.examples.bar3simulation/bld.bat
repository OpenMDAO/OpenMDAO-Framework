cd %SP_DIR%\numpy\distutils\fcompiler
copy %RECIPE_DIR%\..\numpy.distutils-1.7.1.patch .
git apply numpy.distutils-1.7.1.patch
cd %SRC_DIR%

cd examples\%PKG_NAME%
python setup.py bdist_egg
cd ..
easy_install -N %PKG_NAME%
