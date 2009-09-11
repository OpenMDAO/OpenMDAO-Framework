import os, sys

# pylint: disable-msg=F0401

from setuptools import setup, find_packages

version = '0.1.0'

setup(name='openmdao.examples',
      version=version,
      description="OpenMDAO examples",
      long_description="""\
""",
      classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2.5',
        'Topic :: Scientific/Engineering',
      ],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=find_packages(), 
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      setup_requires=[
          'setuptools_bzr'
      ],
      install_requires=[
         'setuptools'
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
