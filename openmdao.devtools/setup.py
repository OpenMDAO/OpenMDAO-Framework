"""
This package is not intended to be used as anything other than a 'develop'
egg.  It's just a place to collect all of the scripts we use to build
openmdao docs, releases, etc., and a place to specify all of their 
dependencies without forcing non-dev openmdao users to satisfy dependencies
that they don't need.
"""

# pylint: disable-msg=F0401

import os,sys
from setuptools import setup, find_packages

#here = os.path.dirname(os.path.realpath(__file__))
#sys.path.insert(0, os.path.normpath(os.path.join(here,
#                                                 'src',
#                                                 'openmdao',
#                                                 'devtools')))

#import releaseinfo
#version = releaseinfo.__version__
version = '0.1'

setup(name='openmdao.devtools',
      version=version,
      description="various scripts to build docs, create releases, etc.",
      classifiers=[],
      keywords='',
      author='',
      author_email='',
      url='http://openmdao.org',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'Sphinx',
          'Fabric>=0.9',
          'openmdao.lib',
      ],
      entry_points = {
          "console_scripts": [
                "openmdao_build_docs=openmdao.devtools.build_docs:build_docs",
                "wingproj=openmdao.devtools.wingproj:run_wing",
                "testdocs=openmdao.devtools.build_docs:test_docs",
              ],
      }
    )
