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
      license='Apache License, Version 2.0',
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
          'argparse',
          'decorator',
          'networkx==1.3',
          'openmdao.units',
          'openmdao.util',
          'pycrypto',
          'pyparsing==1.5.2',
          'PyYAML==3.09',
          'setuptools',
          'Sphinx',
          'Traits==3.3.0',
          'zope.interface',
      ],
      extras_require = {
          'numpy_comps': ['numpy'],
      },
      entry_points = """
      [console_scripts]
      openmdao_docs=openmdao.util.view_docs:view_docs
      plugin=openmdao.main.plugin:plugin
      update_libpath=openmdao.main.plugin:update_libpath
      openmdao=openmdao.main.cli:openmdao
      
      [openmdao.component]
      openmdao.main.assembly.Assembly = openmdao.main.assembly:Assembly
      openmdao.main.component_with_derivatives.ComponentWithDerivatives = openmdao.main.component_with_derivatives:ComponentWithDerivatives
      openmdao.main.driver_uses_derivatives.DriverUsesDerivatives = openmdao.main.driver_uses_derivatives:DriverUsesDerivatives
      openmdao.main.problem_formulation.ArchitectureAssembly = openmdao.main.problem_formulation:ArchitectureAssembly
      
      [openmdao.variable]
      openmdao.main.datatypes.slot.Slot = openmdao.main.datatypes.slot:Slot
      openmdao.main.datatypes.enum.Enum = openmdao.main.datatypes.enum:Enum
      openmdao.main.datatypes.file.File = openmdao.main.datatypes.file:File
      openmdao.main.datatypes.float.Float = openmdao.main.datatypes.float:Float
      openmdao.main.datatypes.int.Int = openmdao.main.datatypes.int:Int
      
      """,
    )
