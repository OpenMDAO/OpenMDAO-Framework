from setuptools import setup, find_packages
import os,sys

here = os.path.dirname(os.path.realpath(__file__))
sdir = os.path.join(here, '..', 'scripts')
sdir = os.path.normpath(sdir)
if os.path.isdir(sdir):
    sys.path.insert(0, sdir)

import releaseinfo

version = releaseinfo.__version__

setup(name='openmdao',
      version=version,
      description="",
      long_description="",
      # Get more strings from http://www.python.org/pypi?%3Aaction=list_classifiers
      classifiers=[
        "Programming Language :: Python",
        "Topic :: Software Development :: Libraries :: Python Modules",
        ],
      keywords='',
      author='',
      author_email='',
      url='',
      license='NOSA',
      #packages=find_packages(exclude=['ez_setup']),
      #namespace_packages=['openmdao'],
      namespace_packages=[],
      packages = [],
      zip_safe=False,
      install_requires=[
          'setuptools',
          'openmdao.lib=='+version,
          'openmdao.test=='+version,
          'openmdao.recipes=='+version,
      ],
      entry_points={
      },
      )
