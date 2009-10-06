from setuptools import setup, find_packages
import sys, os

version = '0.1'

setup(name='testplugins',
      version=version,
      description="a number of plugins for testing",
      long_description="""\
""",
      classifiers=[], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement',
      packages=find_packages(),
      include_package_data=True,
      zip_safe=True,
      install_requires=[
          # -*- Extra requirements: -*-
      ],
      test_suite='nose.collector',
      entry_points="""
      [openmdao.component]
      testplugins.components.dumb.DumbComponent = testplugins.components.dumb:DumbComponent
      """,
      )
