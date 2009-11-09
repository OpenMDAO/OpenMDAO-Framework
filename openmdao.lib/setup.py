
# pylint: disable-msg=F0401

from setuptools import setup, find_packages
import sys
    
setup(name='openmdao.lib',
      version='0.1.0',
      description="OpenMDAO Standard Library",
      long_description="""\
Component, Driver, and TraitType plugins for OpenMDAO
""",
      classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2.6',
        'Topic :: Scientific/Engineering',
      ],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      package_data={'openmdao.lib': ['components/test/*.inp']},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'ScientificPython>=2.9.0',
          'openmdao.main',
          'openmdao.util',
          'conmin',
          'units',
          'Pyevolve',
          'axod',
          ],
      entry_points="""
      [openmdao.drivers]
      conmindrv = openmdao.lib.drivers.conmindriver.CONMINdriver
      pyevolvedrv = openmdao.lib.drivers.pyevolvedriver.pyevolvedriver
      """,
      )
