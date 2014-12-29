
# pylint: disable-msg=F0401

import os
import sys
from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'util')))

import releaseinfo
version = releaseinfo.__version__

setup(name='openmdao.util',
      version=version,
      description="various utility routines",
      long_description="""\
""",
      classifiers=[],
      keywords='',
      author='',
      author_email='',
      url='http://openmdao.org',
      license='Apache License, Version 2.0',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      include_package_data=True,
      package_data={
          'openmdao.util.test': ['src/doubler.py']
      },
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'pycrypto==2.3',
          'pyparsing',
          'Traits==4.3.0',
          #'PyYAML==3.09',
      ],
      entry_points="""
      [console_scripts]
      xyplot=openmdao.util.casedb:cmdlineXYplot
      plotgraph=openmdao.util.graphplot:main
      dotgraph=openmdao.util.dotgraph:main
      add_reqs=openmdao.util.addreqs:add_reqs
      mkpseudo=openmdao.util.mkpseudo:mkpseudo
      envdump=openmdao.util.envirodump:envdump
      pstadump=openmdao.util.dep:main
      update_libpath=openmdao.util.lib:update_libpath
      combine_paths=openmdao.util.lib:combine_paths
      """
      )
