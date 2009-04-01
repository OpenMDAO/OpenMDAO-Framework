import os, sys

# pylint: disable-msg=F0401


try:
    from setuptools import setup
except ImportError, e:
    from distutils.core import setup


version = '0.1.0'

setup(name='openmdao.util',
      version=version,
      description="various utility routines",
      long_description="""\
""",
      classifiers=[],
      keywords='',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=['openmdao', 'openmdao.util'],
      package_dir={'': 'src'},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
      ],
      entry_points = {
      },
    )
