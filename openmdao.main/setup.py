import os, sys

# pylint: disable-msg=F0401

from distutils.errors import DistutilsExecError, DistutilsPlatformError

try:
    from setuptools import setup
except ImportError, e:
    from distutils.core import setup


version = '0.0.1'

setup(name='openmdao.main',
      version=version,
      description="OpenMDAO framework infrastructure",
      long_description="""\
""",
      classifiers=[],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement',
      namespace_packages=["openmdao"],
      packages=['openmdao', 'openmdao.main'],
      package_dir={'': 'src'},
      package_data={'openmdao.main': ['plugins/*.egg','test/*.py']},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'pyparsing',
          'numpy',
          'PyYAML',
          # -*- Extra requirements: -*-
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
