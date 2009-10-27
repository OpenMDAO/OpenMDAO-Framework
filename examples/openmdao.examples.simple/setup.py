import os, sys

# pylint: disable-msg=F0401

#from setuptools import setup, find_packages
from setuptools import find_packages
from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

version = '0.1.0'

if sys.platform == 'win32':
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
    
config = Configuration()

kwds = { 'name':'openmdao.examples.simple',
      	 'version':version,
         'description':"OpenMDAO examples - Simple Problem",
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
         'license':'NASA Open Source Agreement 1.3',
         'namespace_packages':["openmdao", "openmdao.examples"],
         'packages':find_packages(), #['openmdao','openmdao.examples'],
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


