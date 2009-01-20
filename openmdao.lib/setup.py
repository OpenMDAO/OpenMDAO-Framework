from setuptools import setup, find_packages
import os

version = '0.1'

setup(name='openmdao.lib',
      version=version,
      description="OpenMDAO Standard Library",
      long_description=open("README.txt").read() + "\n" +
                       open(os.path.join("docs", "HISTORY.txt")).read(),
      # Get more strings from http://www.python.org/pypi?%3Aaction=list_classifiers
      classifiers=[
        "Programming Language :: Python",
        "Topic :: Software Development :: Libraries :: Python Modules",
        ],
      keywords='',
      author='OpenMDAO Team',
      author_email='',
      url='',
      license='NASA Open Source Agreement',
      packages=find_packages(exclude=['ez_setup']),
      namespace_packages=['openmdao'],
      include_package_data=True,
      zip_safe=False,
      test_suite='nose.collector',
      install_requires=[
          'setuptools',
          'openmdao.main',
          'numpy',
          'pyparsing',
      ],
      entry_points="""
      [openmdao.drivers]
      conmindrv = openmdao.lib.conmindriver.CONMINdriver
      """,
      )
