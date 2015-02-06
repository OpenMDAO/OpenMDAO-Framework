cd %SP_DIR%\numpy\distutils\fcompiler
copy %RECIPE_DIR%\..\numpy.distutils-1.7.1.patch .
git apply numpy.distutils-1.7.1.patch
cd %SRC_DIR%

%PYTHON% setup.py install
%PYTHON% -c "from pkg_resources import get_distribution; print get_distribution('%PKG_NAME%').version" > __conda_version__.txt
