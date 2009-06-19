import os, sys

# pylint: disable-msg=F0401

from setuptools import setup


version = '0.1.0'

setup(name='openmdao.main',
      version=version,
      description="OpenMDAO framework infrastructure",
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
      packages=['openmdao', 'openmdao.main'],
      package_dir={'': 'src'},
      #package_data={'openmdao.main': ['plugins/*.egg','test/*.py']},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'pyparsing',
          'numpy',
          'PyYAML',
          'ScientificPython',
          'networkx',
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
