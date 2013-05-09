Adding a new file here can cause two problems:

1. test_PythonSourceTreeAnalyser can fail if openmdao.main/setup.py isn't
   updated to include entrypoints for new types.

2. openmdao build_docs may report a warning if docs/srcdocs/index.rst
   isn't updated to include a link for the new file.

