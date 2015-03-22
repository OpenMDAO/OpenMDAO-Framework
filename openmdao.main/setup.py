
# pylint: disable-msg=F0401

import os
import sys
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
      url='http://openmdao.org',
      license='Apache License, Version 2.0',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      include_package_data=True,
      package_data={
          'openmdao.main.test': ['src/doubler.py',
                                 'load_test/_macros/default',
                                 'load_test/_settings.cfg',
                                 'load_test/simple.py'],
          'openmdao.main': ['src/openmdao/main/docs/*']
      },
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'decorator',
          'mock',
          'networkx',
          'openmdao.units',
          'openmdao.util',
          'pycrypto==2.3',
          'pyparsing',
          'requests',
          'setuptools',
          'Sphinx==1.2.2',
          'Traits==4.3.0',
          'zope.interface',
      ],
      extras_require={
          'numpy_comps': ['numpy'],
      },
      entry_points="""
      [console_scripts]
      idle=idlelib.PyShell:main
      plugin=openmdao.main.plugin:plugin
      openmdao=openmdao.main.cli:openmdao

      [openmdao.component]
      openmdao.main.assembly.Assembly = openmdao.main.assembly:Assembly
      openmdao.main.component_with_derivatives.ComponentWithDerivatives = openmdao.main.component_with_derivatives:ComponentWithDerivatives
      openmdao.main.driver_uses_derivatives.DriverUsesDerivatives = openmdao.main.driver_uses_derivatives:DriverUsesDerivatives
      openmdao.main.problem_formulation.ArchitectureAssembly = openmdao.main.problem_formulation:ArchitectureAssembly
      openmdao.main.implicitcomp.ImplicitComponent = openmdao.main.implicitcomp:ImplicitComponent

      [openmdao.variable]
      openmdao.main.datatypes.any.Any = openmdao.main.datatypes.any:Any
      openmdao.main.datatypes.bool.Bool = openmdao.main.datatypes.bool:Bool
      openmdao.main.datatypes.complex.Complex = openmdao.main.datatypes.complex:Complex
      openmdao.main.datatypes.dict.Dict = openmdao.main.datatypes.dict:Dict
      openmdao.main.datatypes.enum.Enum = openmdao.main.datatypes.enum:Enum
      openmdao.main.datatypes.event.Event = openmdao.main.datatypes.event:Event
      openmdao.main.datatypes.file.File = openmdao.main.datatypes.file:File
      openmdao.main.datatypes.float.Float = openmdao.main.datatypes.float:Float
      openmdao.main.datatypes.geom.Geom = openmdao.main.datatypes.geom:Geom
      openmdao.main.datatypes.instance.Base = openmdao.main.datatypes.instance:Base
      openmdao.main.datatypes.instance.Instance = openmdao.main.datatypes.instance:Instance
      openmdao.main.datatypes.int.Int = openmdao.main.datatypes.int:Int
      openmdao.main.datatypes.list.List = openmdao.main.datatypes.list:List
      openmdao.main.datatypes.slot.Slot = openmdao.main.datatypes.slot:Slot
      openmdao.main.datatypes.str.Str = openmdao.main.datatypes.str:Str
      openmdao.main.datatypes.uncertaindist.UncertainDistVar = openmdao.main.datatypes.uncertaindist:UncertainDistVar
      openmdao.main.datatypes.vtree.VarTree = openmdao.main.datatypes.vtree:VarTree
      openmdao.main.datatypes.array.Array = openmdao.main.datatypes.array:Array
      """,
      )
