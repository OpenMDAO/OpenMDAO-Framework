import setuptools
import sys

from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration
from numpy.distutils.fcompiler import get_default_fcompiler

config = Configuration(name='axod')
config.add_extension('axod', sources=['src/*.f'], f2py_options=['-m', 'axod'])

config.add_data_dir('test')
config.add_data_dir('src')
config.add_data_files('README.txt', 'axod.pdf', 'AXOD_AAO.pdf')

# Dictionary of compiler flags indexed by compiler name.
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

kwds = {'install_requires':['numpy'],
        'version':'1.0',
        'zip_safe':False,
       }
kwds.update(config.todict())

setup(**kwds)

