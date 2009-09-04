# setup.py

import setuptools
import sys

from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration
from numpy.distutils.fcompiler import get_default_fcompiler

packages = ['axod']
config = Configuration(name='axod')
config.add_extension('axod', sources=['src/*.f'], f2py_options=['-m', 'axod'])

# data_files = (['test/axod.inp','test/axod.out','test/fort.7','test/pltfile','test/testaxod.py'])
# package_data = data_files

config.add_data_dir('test')
config.add_data_dir('src')
config.add_data_files('README.txt')
# config.add_extension('axod',data_files)
# Dictionary of compiler flags indexed by compiler name.
f77_compiler_flags = {
#    'gnu': '-g -fno-automatic',
    'gnu95': '-g -fno-automatic',
}

# Check if we need to configure a compile.
need_config = False
for arg in sys.argv:
    if arg == 'config_fc':
        need_config = False
        break
    elif arg.startswith('build') or \
         arg.startswith('bdist') or \
         arg.startswith('install'):
        need_config = True
        break

if need_config:
    f77 = get_default_fcompiler()
    f77flags = f77_compiler_flags.get(f77, None)
    if f77flags:
        # Configure compiler options using setup's 'config_fc' command.
        sys.argv[1:1] = ['config_fc', '--f77flags=%s' % f77flags]

# package_data = ['axod':data_files],

kwds = {'install_requires':['numpy'],
        'version':'1.0',
        'zip_safe':False,
#         package_data = ['axod':data_files],
       }
kwds.update(config.todict())

setup(**kwds)



