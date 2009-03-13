from setuptools import setup, find_packages
import sys, os

version = '.01'

setup(name='pyevolvedriver',
      version=version,
      description="driver wrapper for the pyevolve package",
      long_description="""\
""",
      classifiers=[], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='',
      author='Justin Gray',
      author_email='Justin.S.Gray@nasa.gov',
      url='',
      license='NOSA',
      packages=find_packages(exclude=['ez_setup', 'examples', 'tests']),
      include_package_data=True,
      zip_safe=True,
      install_requires=[
          # -*- Extra requirements: -*-
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
