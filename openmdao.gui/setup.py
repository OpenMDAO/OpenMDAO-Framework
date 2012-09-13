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
      # NOTE: make sure that none of the glob patterns below match
      # directories.  Otherwise you'll get errors like the following:
      # Setup script exited with error: can't copy
      # 'src/openmdao/gui/test/js_unit_tests/src': doesn't exist or
      # not a regular file     
      package_data={'openmdao.gui': ['static/ace-min/*',
                                     'static/css/*.css',
                                     'static/css/jstree/classic/*',
                                     'static/css/jstree-themes/classic/*',
                                     'static/css/jstree-themes/openmdao/*.css',
                                     'static/css/jstree-themes/openmdao/*.gif',
                                     'static/css/jstree-themes/openmdao/*.png',
                                     'static/css/jstree-themes/openmdao/classic/*',
                                     'static/css/ui-openmdao/*.css',
                                     'static/css/ui-openmdao/images/*',
                                     'static/favicon.ico',
                                     'static/images/*.cur',
                                     'static/images/*.png',
                                     'static/images/alphabet/*',
                                     'static/js/*.js',
                                     'static/js/3Dresources/*.js',
                                     'static/js/3Dresources/o3djs/*',
                                     'static/js/3Dresources/o3d-webgl/*',
                                     'static/js/openmdao/*',
                                     'static/js/slickgrid/*',
                                     'static/js/themes/classic/*',
                                     'static/js/webgl/*',
                                     'templates/*.html',
                                     'templates/projdb/*',
                                     'templates/workspace/*',
                                     'test/*.proj',
                                     'test/functional/manaual/*',
                                     'test/js_unit_tests/*.js']},
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
