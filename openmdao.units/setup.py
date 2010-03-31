
import os
import sys
from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sdir = os.path.join(here, '..', 'scripts')
sdir = os.path.normpath(sdir)
if os.path.isdir(sdir):
    sys.path.insert(0, sdir)

import releaseinfo

version = releaseinfo.__version__

setup(name='openmdao.units',
      version=version,
      license = "CeCILL-C",
      packages=find_packages(exclude=['ez_setup']),
      namespace_packages=['openmdao'],
      package_data = {'units': ['unitLibdefault.ini']},
      include_package_data=True,
      zip_safe=False,
      install_requires=[
          'setuptools',
      ],
      )



