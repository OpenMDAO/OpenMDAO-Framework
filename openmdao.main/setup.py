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
      package_data={
          'openmdao.main.test': ['src/doubler.py']
      },
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'decorator',
          'networkx==1.3',
          'numpy',
          'openmdao.units',
          'openmdao.util',
          'pycrypto',
          'pyparsing==1.5.2',
          'PyYAML==3.09',
          'setuptools',
          'Sphinx',
          'Traits==3.3.0',
      ],
      entry_points = """
      [console_scripts]
      openmdao_docs=openmdao.util.view_docs:view_docs
      plugin_docs=openmdao.main.plugin:plugin_docs
      plugin_build_docs=openmdao.main.plugin:plugin_build_docs
      plugin_install=openmdao.main.plugin:plugin_install
      plugin_makedist=openmdao.main.plugin:plugin_makedist
      plugin_quickstart=openmdao.main.plugin:plugin_quickstart
      
      [openmdao.component]
      openmdao.main.assembly.Assembly = openmdao.main.assembly:Assembly
      openmdao.main.assembly.ComponentWithDerivatives = openmdao.main.assembly:ComponentWithDerivatives
      """,
    )
