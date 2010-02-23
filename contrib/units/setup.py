from setuptools import setup,find_packages
setup(
  name="units",
  version = "0.1",
  license = "CeCILL-C",
  packages = find_packages(),
  package_data = {'units': ['unitLibdefault.ini']},
  include_package_data=True,
  zip_safe = False,
  )
  
