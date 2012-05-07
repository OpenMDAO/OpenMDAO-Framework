from setuptools import setup, find_packages
import sys, os

here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'gui')))

import releaseinfo
version = releaseinfo.__version__

setup(name='openmdao.gui',
      version=version,
      description="OpenMDAO graphical user interface",
      long_description="""OpenMDAO graphical user interface""",
      classifiers=[], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='OpenMDAO GUI',
      author='',
      author_email='',
      url='http://openmdao.org',
      license='Apache License, Version 2.0',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'pyzmq-static>=2.1.11.1',
          'tornado>=2.2',
          'jsonpickle',
          'watchdog'
      ],
      extras_require = {
          'jsTest': [
              'path.py==2.2.2',
              'lazr.testing==0.1.2a',
              'mocker==1.1',
              'zope.testrunner==4.0.4',
              'zope.exceptions==3.6.1'
          ],
          'functionalTest': [
              'selenium==2.20.0',
              'PyVirtualDisplay==0.1',
          ]
      },   
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
