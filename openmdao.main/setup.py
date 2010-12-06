# pylint: disable-msg=F0401

import os,sys
from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'main')))

import releaseinfo
version = releaseinfo.__version__

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
        'Programming Language :: Python :: 2.6',
        'Topic :: Scientific/Engineering',
      ],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
      author='',
      author_email='',
      url='http://openmdao.org/docs/srcdocs/packages/openmdao.main.html',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'pyparsing==1.5.2',
          'numpy',
          'PyYAML==3.09',
          'networkx==1.0.1',
          'Traits==3.3.0',
          'decorator',
          'openmdao.units',
          'openmdao.util',
          'pycrypto',
      ],
      entry_points = {
          "console_scripts": [
                "openmdao_docs=openmdao.util.view_docs:view_docs",
              ],
          },
    )
