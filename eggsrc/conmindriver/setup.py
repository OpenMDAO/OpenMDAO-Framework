from setuptools import setup, find_packages
import sys, os

version = '0.1'

setup(name='conmindriver',
      version=version,
      description="OpenMDAO Driver wrapper for CONMIN",
      long_description="""\
""",
      classifiers=[], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='conmin optimizer gradient',
      author='OpenMDAO Team',
      author_email='',
      url='',
      license='',
      packages=find_packages(exclude=['ez_setup', 'examples', 'tests']),
      include_package_data=True,
      zip_safe=False,
      install_requires=[
          # -*- Extra requirements: -*-
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
