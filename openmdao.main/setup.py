
# pylint: disable-msg=F0401
from setuptools import setup, find_packages
import os

version = '0.1'

setup(name='openmdao.main',
      version=version,
      description="MDAO framework kernel",
      long_description=open("README.txt").read() + "\n" +
                       open(os.path.join("docs", "HISTORY.txt")).read(),
      # Get more strings from 
      # http://www.python.org/pypi?%3Aaction=list_classifiers
      classifiers=[
        "Programming Language :: Python",
        "Topic :: Software Development :: Libraries :: Python Modules",
        ],
      keywords='optimization multidisciplinary multi-discipliniary analysis',
      author='OpenMDAO Team',
      author_email='',
      url='',
      license='NASA Open Source License',
      packages=find_packages(exclude=['ez_setup']),
      package_data={'openmdao.main': ['plugins/*.egg']},
      namespace_packages=['openmdao'],
      include_package_data=True,
      zip_safe=False,
      test_suite='nose.collector',
      install_requires=[
          'setuptools',
          'pyparsing',
          'numpy',
          # -*- Extra requirements: -*-
      ],
      entry_points="""
      [openmdao.drivers]
      onepass = openmdao.main.drivers.OnePassDriver
      """,
      )


