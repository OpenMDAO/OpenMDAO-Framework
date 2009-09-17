from setuptools import setup, find_packages
import sys, os


setup(name='setuptools_bzr',
      version='0.1',
      description="A setuptools plugin for bazaar.",
      long_description="""\
This plugin allows the include_package_data attribute in a setup call to
work as you would expect, where all files that are versioned in a bzr repo
will be included in your package automatically.
""",
      classifiers=[
        'Framework :: Setuptools Plugin',        
      ], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='bazaar bzr setuptools plugin',
      author='',
      author_email='',
      url='',
      license='',
      py_modules = ['setuptools_bzr'],
      packages=[],
      include_package_data=True,
      zip_safe=False,
      install_requires=[
          'setuptools'
      ],
      entry_points = {
         "setuptools.file_finders": [
             "bzrfinder = setuptools_bzr:findfiles"
         ]
      },
   )
