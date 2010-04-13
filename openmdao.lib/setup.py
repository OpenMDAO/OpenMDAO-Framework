
# pylint: disable-msg=F0401

import os,sys
from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'lib')))
import releaseinfo
version = releaseinfo.__version__

setup(name='openmdao.lib',
      version=version,
      description="OpenMDAO Standard Library",
      long_description="""\
Component, Driver, and TraitType plugins for OpenMDAO
""",
      classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2.6',
        'Topic :: Scientific/Engineering',
      ],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
      author='',
      author_email='',
      url='http://openmdao.org',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      package_data={'openmdao.lib': ['components/test/*.inp']},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'openmdao.main',
          'conmin',
          'Pyevolve',
          'axod',
          ],
      entry_points="""
      [openmdao.driver]
      openmdao.lib.CONMINdriver = openmdao.lib.drivers.conmindriver:CONMINdriver
      openmdao.lib.pyevolvedriver = openmdao.lib.drivers.pyevolvedriver:pyevolvedriver
      """,
      )
