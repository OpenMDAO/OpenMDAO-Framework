
# pylint: disable-msg=F0401
import os,sys

from distutils.errors import DistutilsExecError, DistutilsPlatformError

from setuptools import setup

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'test')))

import releaseinfo
version = releaseinfo.__version__

setup(name='openmdao.test',
      version=version,
      description="OpenMDAO framework testing package",
      long_description="""\
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
      keywords='testing',
      author='',
      author_email='',
      url='http://openmdao.org',
      license='Apache License, Version 2.0',
      namespace_packages=["openmdao"],
      packages=['openmdao', 'openmdao.test'],
      package_dir={'': 'src'},
      package_data={ 'openmdao.test': ['plugins/*.egg'] },
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'nose',
          'openmdao.main',
      ],
      entry_points={
      "openmdao.component": [
          'openmdao.test.execcomp.ExecComp = openmdao.test.execcomp:ExecComp'
          ],
      "console_scripts": [
          'openmdao_test = openmdao.test.testing:run_openmdao_suite_deprecated'
          ],
      "nose.plugins.0.10": [
          'nosetimerplugin = openmdao.test.nosetimer:TestTimer',
          'nosesummaryplugin = openmdao.test.testing:TestFailureSummary',
          'noseearlyplugin = openmdao.test.early_report:EarlyTestInfo'
          ],
      },
      )
