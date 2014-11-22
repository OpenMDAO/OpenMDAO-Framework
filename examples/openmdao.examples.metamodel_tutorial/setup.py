import os, sys

# pylint: disable-msg=F0401

from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'openmdao',
                                                 'examples',
                                                 'metamodel_tutorial')))

import releaseinfo
version = releaseinfo.__version__

setup(name='openmdao.examples.metamodel_tutorial',
      version=version,
      description="OpenMDAO examples - Metamodel Tutorial",
      long_description="""\
         """,
      classifiers=[
            'Development Status :: 2 - Pre-Alpha',
            'Intended Audience :: Science/Research',
            'License :: OSI Approved',
            'Natural Language :: English',
            'Operating System :: OS Independent',
            'Programming Language :: Python :: 2.7',
            'Topic :: Scientific/Engineering',
             ],
       keywords='optimization multidisciplinary multi-disciplinary analysis',
       author='',
       author_email='',
       url='http://openmdao.org',
       license='Apache License, Version 2.0',
       namespace_packages=["openmdao", "openmdao.examples"],
       packages=find_packages(), #['openmdao','openmdao.examples'],
       include_package_data=True,
       test_suite='nose.collector',
       zip_safe=False,
       install_requires=[
             'setuptools',
             'openmdao.lib',
             ],
       entry_points="""
         # -*- Entry points: -*-
         """
      )
