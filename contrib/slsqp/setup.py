import os.path
import setuptools
import sys

from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

include_dirs = []
library_dirs = []
if sys.platform == 'win32':
    # Update the ``library_dir_option`` function in MSVCCompiler 
    # to add quotes around /LIBPATH entries.
    import types
    def _lib_dir_option(self, dir):
        return '/LIBPATH:"%s"' % dir
    
    from distutils.msvc9compiler import MSVCCompiler
    setattr(MSVCCompiler, 'library_dir_option',
            types.MethodType(_lib_dir_option, None, MSVCCompiler))
    
    sdkdir = os.environ.get('WindowsSdkDir')
    if sdkdir:
        include_dirs.append(os.path.join(sdkdir,'Include'))
        library_dirs.append(os.path.join(sdkdir,'Lib'))
        # make sure we have mt.exe available in case we need it
        path = os.environ['PATH'].split(';')
        path.append(os.path.join(sdkdir,'bin'))
        os.environ['PATH'] = ';'.join(path)

config = Configuration(name='slsqp')
config.add_extension('slsqp',
                     sources=['*.f',
                              'f2py/slsqp.pyf'],
                     include_dirs=include_dirs,
                     library_dirs=library_dirs)
config.add_data_files('LICENSE','README')

kwds = {'install_requires':['numpy'],
        'version': '1.0.1',
        'zip_safe': False,
        'license': 'permissive open source',
   # NOTE: we use 'url' here, but it really translates to 'home-page'
   # in the metadata. Go figure.
        'url': 'http://www.pyopt.org',
        'package_data': {'openmdao.main': ['*.html']},
       }
kwds.update(config.todict())

setup(**kwds)

