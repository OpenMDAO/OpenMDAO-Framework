"""
This package is not included in an OpenMDAO release.  It's always present in 
a 'dev' install, but to be used with a release it must be explicitly installed
after the release virtualenv has been activated.

It's just a place to collect all of the tools we use to build
openmdao docs, releases, etc., and a place to specify all of their 
dependencies without forcing non-dev openmdao users to satisfy dependencies
that they don't need.
"""

# pylint: disable-msg=F0401

import os,sys
from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'devtools')))

import releaseinfo
version = releaseinfo.__version__

setup(name='openmdao.devtools',
      version=version,
      description="various scripts to build docs, create releases, etc.",
      classifiers=[],
      keywords='',
      author='',
      author_email='',
      url='http://openmdao.org',
      license='Apache License, Version 2.0',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'Sphinx',
          'Fabric>=0.9.3',
          'virtualenv',
          'openmdao.util',
          'boto',
          'paramiko>=1.7.7',
          'requests',
      ],
      entry_points = {
          "console_scripts": [
                "wingproj=openmdao.devtools.wingproj:run_wing",
                "make_installer=openmdao.devtools.mkinstaller:main",
                "release=openmdao.devtools.releasetools:release",
                "push_dists=openmdao.devtools.push_dists:main",
                "remote_build=openmdao.devtools.remote_build:main",
              ],
      }
    )


