from setuptools import setup, find_packages
import sys, os


setup(name='nose_coverage2',
      version='0.1',
      description="A nose plugin to use new features of the coverage package.",
      long_description="""\
This nose plugin allows access to the new coverage features of html generation and cobertura compatible output.
""",
      classifiers=[
        'Framework :: Nose Plugin',        
      ], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='nose coverage plugin',
      author='',
      author_email='',
      url='',
      license='',
      #py_modules = ['cover2.py'],
      packages=find_packages(),
      include_package_data=True,
      zip_safe=False,
      install_requires=[
          'setuptools',
          'coverage>=3.2',
      ],
      entry_points = {
         "nose.plugins": [
             "coverage2 = nose_coverage2.cover2:Coverage2"
         ]
      },
   )


