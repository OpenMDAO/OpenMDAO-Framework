
from setuptools import setup, find_packages

setup(name='openmdao.units',
      version='0.1',
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



