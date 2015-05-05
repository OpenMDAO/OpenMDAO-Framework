SETLOCAL

cd examples

easy_install --no-deps %PKG_NAME% 

%PYTHON% -c "from pkg_resources import get_distribution; print get_distribution('%PKG_NAME%').parsed_version.base_version" > __conda_version__.txt

TYPE %PKG_NAME%\__buildnum__.txt > __conda_buildnum__.txt
ENDLOCAL
