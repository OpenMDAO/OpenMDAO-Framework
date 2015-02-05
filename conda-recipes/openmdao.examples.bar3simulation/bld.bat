cd examples\%PKG_NAME%
python setup.py bdist_egg
cd ..
easy_install -N %PKG_NAME%
