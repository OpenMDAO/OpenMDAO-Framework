import os, sys

# pylint: disable-msg=F0401

from setuptools import setup, find_packages

version = '0.0.1'

setup(name='openmdao.lib',
      version=version,
      description="OpenMDAO Standard Library",
      long_description="""\
""",
      classifiers=[],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement',
      namespace_packages=["openmdao"],
      packages=['openmdao','openmdao.lib','openmdao.lib.drivers',
                'openmdao.lib.components','openmdao.lib.variables',
                'openmdao.lib.factories'],
      package_dir={'': 'src'},
      package_data={'openmdao.lib.drivers': ['test/*.py'],
                    'openmdao.lib.variables': ['test/*.py'],
                    'openmdao.lib.factories': ['test/*.py'],
                    'openmdao.lib.components': ['test/*.py']
      },
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'openmdao.main',
          'conmin',
      ],
      entry_points="""
      [openmdao.drivers]
      conmindrv = openmdao.lib.conmindriver.CONMINdriver
      """,
      )
