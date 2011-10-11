import os, sys

# pylint: disable-msg=F0401

#from setuptools import setup, find_packages
from setuptools import find_packages
from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'openmdao',
                                                 'examples',
                                                 'bar3simulation')))

import releaseinfo
version = releaseinfo.__version__

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
    
config = Configuration()
config.add_extension('openmdao.examples.bar3simulation.bar3', \
                     sources=['openmdao/examples/bar3simulation/bar3.pyf', \
                              'openmdao/examples/bar3simulation/bar3.f'],
                     include_dirs=include_dirs,
                     library_dirs=library_dirs)

kwds = { 'name':'openmdao.examples.bar3simulation',
         'version':version,
         'description':"OpenMDAO examples - Bar3 Truss Simulation Problem",
         'long_description':"""\
         """,
         'classifiers':[
            'Development Status :: 2 - Pre-Alpha',
            'Intended Audience :: Science/Research',
            'License :: OSI Approved',
            'Natural Language :: English',
            'Operating System :: OS Independent',
            'Programming Language :: Python :: 2.6',
            'Topic :: Scientific/Engineering',
             ],
         'keywords':'optimization multidisciplinary multi-disciplinary analysis',
         'author':'',
         'author_email':'',
         'url':'',
         'license':'Apache License, Version 2.0',
         'namespace_packages':["openmdao", "openmdao.examples"],
         #'package_dir':{'': 'openmdao/examples/bar3simulation'},
         'packages':find_packages(), #['openmdao','openmdao.examples'],
         'package_data': {'openmdao.examples.bar3simulation': ['*.csv']},
         'include_package_data': True,
         'test_suite':'nose.collector',
         'zip_safe': False,
         'install_requires':[
             'setuptools',
             'openmdao.lib',
             ],
         'entry_points':"""
         # -*- Entry points: -*-
         """,
      }

kwds.update(config.todict())

setup(**kwds)
