"""
Importing this file will fix problems we've found in distutils.

Current fixes are:

Update the ``library_dir_option`` function in MSVCCompiler to add quotes around /LIBPATH entries.
"""
import sys

if sys.platform == 'win32':
    import types
    def _lib_dir_option(self, dir):
        return '/LIBPATH:"%s"' % dir
    
    from distutils.msvc9compiler import MSVCCompiler
    setattr(MSVCCompiler, 'library_dir_option',
            types.MethodType(_lib_dir_option, None, MSVCCompiler))
