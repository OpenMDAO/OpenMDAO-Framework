
import os,sys
from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'openmdao',
                                                 'units')))

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
      url='http://openmdao.org',
      install_requires=[
          'setuptools',
          'numpy',
      ],
      )



