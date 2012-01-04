
import os,sys
from setuptools import setup

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'openmdao',
                                                 'units')))
import releaseinfo

setup(name='openmdao.units',
      version=releaseinfo.__version__,
      license = "CeCILL-C",
      packages=['openmdao', 'openmdao.units', 'openmdao.units.test'],
      namespace_packages=['openmdao'],
      package_data = {'units': ['unitLibdefault.ini']},
      include_package_data=True,
      zip_safe=False,
      url='http://openmdao.org',
      )



