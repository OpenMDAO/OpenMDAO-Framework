SETLOCAL
cd %SP_DIR%\numpy\distutils\fcompiler
copy %RECIPE_DIR%\..\numpy.distutils-1.7.1.patch .
git apply numpy.distutils-1.7.1.patch
cd %SRC_DIR%

easy_install --no-deps . 

%PYTHON% -c "from pkg_resources import get_distribution; print get_distribution('%PKG_NAME%').parsed_version.base_version" > __conda_version__.txt

TYPE __buildnum__.txt > __conda_buildnum__.txt
ENDLOCAL
