import os.path
import setuptools
import sys

from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration
from numpy.distutils.fcompiler import get_default_fcompiler

version = '0.1.0'

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

config = Configuration(name='axod')
config.add_extension('axod', sources=['src/*.f'],
                     include_dirs=include_dirs,
                     library_dirs=library_dirs)

config.add_data_dir('test')
config.add_data_dir('src')
config.add_data_files('README.txt', 'axod.pdf', 'AXOD_AAO.pdf')

# Dictionary of compiler flags indexed by compiler name.
# AXOD assumes (somewhere) that variables are statically allocated.
# (and likely depending on those variables to be initialized to zero)
# Note that these will replace the default flags.
f77_compiler_flags = {
    'gnu': '-Wall -ffixed-form -fno-second-underscore -fno-automatic',
    'gnu95': '-Wall -ffixed-form -fno-second-underscore -fno-automatic',
}

# Check if we need to configure a compile.
config_index = 0
for i, arg in enumerate(sys.argv):
    if arg == 'config_fc':
        break
    elif arg.startswith(('build', 'bdist', 'develop', 'install')):
        config_index = i
        break

if config_index > 0:
    f77 = get_default_fcompiler()
    f77flags = f77_compiler_flags.get(f77, None)
    if f77flags:
        # Configure compiler options using setup's 'config_fc' command.
        sys.argv[config_index:config_index] = \
            ['config_fc', '--f77flags=%s' % f77flags]

kwds = {'description':'AXOD - axial turbine off-design analysis.',
        'version':version,
        'license': 'public domain',
        'install_requires':['numpy'],
        'zip_safe':False,
       }

kwds.update(config.todict())

setup(**kwds)

