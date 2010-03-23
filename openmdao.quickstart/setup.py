from setuptools import setup, find_packages
import os

setup(name='openmdao.quickstart',
      version='0.1',
      description="",
      long_description="",
      # Get more strings from http://www.python.org/pypi?%3Aaction=list_classifiers
      classifiers=[
        "Programming Language :: Python",
        "Topic :: Software Development :: Libraries :: Python Modules",
        ],
      keywords='',
      author='',
      author_email='',
      url='',
      license='NOSA',
      packages=find_packages(exclude=['ez_setup']),
      namespace_packages=['openmdao'],
      package_data={'openmdao': ['*.cfg']},
      include_package_data=True,
      zip_safe=False,
      install_requires=[
          'setuptools',
          'zc.buildout',
      ],
      entry_points={
      "console_scripts": [
          'openmdao_quickstart = openmdao.quickstart.quickstart:run'
          ]
      },
      )
