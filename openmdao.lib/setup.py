
# pylint: disable-msg=F0401

import os,sys
from setuptools import setup, find_packages

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'lib')))
import releaseinfo
version = releaseinfo.__version__

setup(name='openmdao.lib',
      version=version,
      description="OpenMDAO Standard Library",
      long_description="""\
Component, Driver, and Variable plugins for OpenMDAO
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
      url='http://openmdao.org/docs/srcdocs/packages/openmdao.lib.html',
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
          'openmdao.main',
          'conmin==1.0.1',
          'newsumt==1.0.0',
          'Pyevolve==0.6',
          'ordereddict',
          'scipy',
          ],
      entry_points="""
      [openmdao.driver]
      openmdao.lib.drivers.broydensolver.BroydenSolver = openmdao.lib.drivers.broydensolver:BroydenSolver
      openmdao.lib.drivers.caseiterdriver.CaseIteratorDriver = openmdao.lib.drivers.caseiterdriver:CaseIteratorDriver
      openmdao.lib.drivers.conmindriver.CONMINdriver = openmdao.lib.drivers.conmindriver:CONMINdriver
      openmdao.lib.drivers.doedriver.DOEdriver = openmdao.lib.drivers.doedriver:DOEdriver
      openmdao.lib.drivers.genetic.Genetic = openmdao.lib.drivers.genetic:Genetic
      openmdao.lib.drivers.iterate.FixedPointIterator = openmdao.lib.drivers.iterate:FixedPointIterator
      openmdao.lib.drivers.iterate.IterateUntil = openmdao.lib.drivers.iterate:IterateUntil
      openmdao.lib.drivers.newsumtdriver.NEWSUMTdriver = openmdao.lib.drivers.newsumtdriver:NEWSUMTdriver
      openmdao.lib.drivers.simplecid.SimpleCaseIterDriver = openmdao.lib.drivers.simplecid:SimpleCaseIterDriver

      [openmdao.component]
      openmdao.lib.components.expected_improvement.ExpectedImprovement = openmdao.lib.components.expected_improvement:ExpectedImprovement
      openmdao.lib.components.expected_improvement_multiobj.MultiObjExpectedImprovement = openmdao.lib.components.expected_improvement_multiobj:MultiObjExpectedImprovement
      openmdao.lib.components.external_code.ExternalCode = openmdao.lib.components.external_code:ExternalCode
      openmdao.lib.components.metamodel.MetaModel = openmdao.lib.components.metamodel:MetaModel
      openmdao.lib.components.mux.Mux = openmdao.lib.components.mux:Mux
      openmdao.lib.components.mux.DeMux = openmdao.lib.components.mux:DeMux
      openmdao.lib.components.pareto_filter.ParetoFilter = openmdao.lib.components.pareto_filter:ParetoFilter
      openmdao.lib.components.prob_intersect.ProbIntersect = openmdao.lib.components.prob_intersect:ProbIntersect
      openmdao.lib.components.nastran.nastran.NastranComponent = openmdao.lib.components.nastran.nastran:NastranComponent

      [openmdao.variable]
      opendao.lib.datatypes.array.Array = openmdao.lib.datatypes.array:Array
      opendao.lib.datatypes.enum.Enum = openmdao.lib.datatypes.enum:Enum
      opendao.lib.datatypes.file.File = openmdao.lib.datatypes.file:File
      opendao.lib.datatypes.float.Float = openmdao.lib.datatypes.float:Float
      opendao.lib.datatypes.int.Int = openmdao.lib.datatypes.int:Int
      """,
      )
