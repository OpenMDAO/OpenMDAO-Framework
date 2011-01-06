import os.path
import setuptools
import sys

from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

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
    include_dirs = [os.path.join(sdkdir,'Include')]
    library_dirs = [os.path.join(sdkdir,'Lib')]
    # make sure we have mt.exe available in path
    path = os.environ['PATH'].split(';')
    path.append(os.path.join(sdkdir,'bin'))
    os.environ['PATH'] = ';'.join(path)
else:
    include_dirs = []
    library_dirs = []

config = Configuration(name='newsumt')
config.add_extension('newsumtinterruptible',
                     sources=['newsumt_interruptible.f',
                              'newsumt_interruptible.pyf'],
                     include_dirs=include_dirs,
                     library_dirs=library_dirs)

kwds = {'install_requires':['numpy'],
        'version': '1.0.0',
        'zip_safe': False,
        'license': 'public domain',
   # NOTE: we use 'url' here, but it really translates to 'home-page'
   # in the metadata. Go figure.
        'url': 'http://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19790018640_1979018640.pdf',
        'package_data': {'openmdao.main': ['*.html']},
       }
kwds.update(config.todict())

setup(**kwds)

