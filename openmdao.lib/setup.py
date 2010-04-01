
# pylint: disable-msg=F0401

from setuptools import setup, find_packages
import os,sys

here = os.path.dirname(os.path.realpath(__file__))
sdir = os.path.join(here, '..', 'scripts')
sdir = os.path.normpath(sdir)
if os.path.isdir(sdir):
    sys.path.insert(0, sdir)

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
      url='',
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
