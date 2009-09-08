
# pylint: disable-msg=F0401

from setuptools import setup, find_packages

version = '0.1.0'

setup(name='openmdao.lib',
      version=version,
      description="OpenMDAO Standard Library",
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
      packages=find_packages('src'),
      #packages=['openmdao',
      #          'openmdao.lib',
      #          'openmdao.lib.drivers',
      #          'openmdao.lib.components',
      #          'openmdao.lib.variables',
      #          'openmdao.lib.factories'],
      package_dir={'': 'src'},
      #package_data={'openmdao.lib.drivers': ['test/*.py'],
      #              'openmdao.lib.components': ['test/*.py'],
      #              'openmdao.lib.variables': ['test/*.py'],
      #              'openmdao.lib.factories': ['test/*.py'],
      #},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'openmdao.main',
          'conmin',
          'Pyevolve',
#          'axod',
      ],
      entry_points="""
      [openmdao.drivers]
      conmindrv = openmdao.lib.drivers.conmindriver.CONMINdriver
      pyevolvedrv = openmdao.lib.drivers.pyevolvedriver.pyevolvedriver
      """,
      )
