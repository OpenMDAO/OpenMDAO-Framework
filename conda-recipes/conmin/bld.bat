%PYTHON% setup.py install
%PYTHON% -c "from pkg_resources import get_distribution; print get_distribution('%PKG_NAME%').version" > __conda_version__.txt
