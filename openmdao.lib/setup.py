# pylint: disable=F0401

import os
import sys
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
Component, Driver, Variable and other plugins for OpenMDAO
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
      namespace_packages=['openmdao'],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      package_data={'openmdao.lib': ['casehandlers/test/*.bson',
                                     'casehandlers/test/*.json',
                                     'casehandlers/test/*.csv',
                                     'components/test/*.inp',
                                     'datatypes/domain/test/grid.in',
                                     'datatypes/domain/test/q.save',
                                     'datatypes/domain/test/lpc-test.*']},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'openmdao.main',
          'Pyevolve==0.6',
          'pytz>=2014.4',
          'bson==0.3.3',
          'conmin==1.0.2',
          'newsumt==1.1.1',
          'cobyla==1.0.2',
          'slsqp==1.0.2',
          'numpy',
          'scipy>=0.11.0',
          ],
      entry_points="""
      [openmdao.driver]
      openmdao.lib.drivers.adaptivesampledriver.AdaptiveSampleDriver = openmdao.lib.drivers.adaptivesampledriver:AdaptiveSampleDriver
      openmdao.lib.drivers.broydensolver.BroydenSolver = openmdao.lib.drivers.broydensolver:BroydenSolver
      openmdao.lib.drivers.caseiterdriver.CaseIteratorDriver = openmdao.lib.drivers.caseiterdriver:CaseIteratorDriver
      openmdao.lib.drivers.conmindriver.CONMINdriver = openmdao.lib.drivers.conmindriver:CONMINdriver
      openmdao.lib.drivers.cobyladriver.COBYLAdriver = openmdao.lib.drivers.cobyladriver:COBYLAdriver
      openmdao.lib.drivers.doedriver.DOEdriver = openmdao.lib.drivers.doedriver:DOEdriver
      openmdao.lib.drivers.doedriver.NeighborhoodDOEdriver = openmdao.lib.drivers.doedriver:NeighborhoodDOEdriver
      openmdao.lib.drivers.genetic.Genetic = openmdao.lib.drivers.genetic:Genetic
      openmdao.lib.drivers.iterate.FixedPointIterator = openmdao.lib.drivers.iterate:FixedPointIterator
      openmdao.lib.drivers.iterate.IterateUntil = openmdao.lib.drivers.iterate:IterateUntil
      openmdao.lib.drivers.newton_solver.NewtonSolver = openmdao.lib.drivers.newton_solver:NewtonSolver
      openmdao.lib.drivers.newsumtdriver.NEWSUMTdriver = openmdao.lib.drivers.newsumtdriver:NEWSUMTdriver
      openmdao.lib.drivers.simplecid.SimpleCaseIterDriver = openmdao.lib.drivers.simplecid:SimpleCaseIterDriver
      openmdao.lib.drivers.mpicasedriver.MPICaseDriver = openmdao.lib.drivers.mpicasedriver.MPICaseDriver
      openmdao.lib.drivers.slsqpdriver.SLSQPdriver = openmdao.lib.drivers.slsqpdriver:SLSQPdriver
      openmdao.lib.drivers.sensitivity.SensitivityDriver = openmdao.lib.drivers.sensitivity:SensitivityDriver
      openmdao.lib.drivers.brent.Brent = openmdao.lib.drivers.brent:Brent

      [openmdao.component]
      openmdao.lib.components.expected_improvement.ExpectedImprovement = openmdao.lib.components.expected_improvement:ExpectedImprovement
      openmdao.lib.components.expected_improvement_multiobj.MultiObjExpectedImprovement = openmdao.lib.components.expected_improvement_multiobj:MultiObjExpectedImprovement
      openmdao.lib.components.external_code.ExternalCode = openmdao.lib.components.external_code:ExternalCode
      openmdao.lib.components.metamodel.MetaModel = openmdao.lib.components.metamodel:MetaModel
      openmdao.lib.components.mux.Mux = openmdao.lib.components.mux:Mux
      openmdao.lib.components.mux.DeMux = openmdao.lib.components.mux:DeMux
      openmdao.lib.components.broadcaster.Broadcaster = openmdao.lib.components.broadcaster:Broadcaster
      openmdao.lib.components.pareto_filter.ParetoFilter = openmdao.lib.components.pareto_filter:ParetoFilter
      openmdao.lib.components.linear_distribution.LinearDistribution = openmdao.lib.components.linear_distribution:LinearDistribution
      openmdao.lib.components.sleep_comp.SleepComponent = openmdao.lib.components.sleep_comp:SleepComponent
      openmdao.lib.components.linear_system.LinearSystem = openmdao.lib.componnets.linear_system:LinearSystem
      openmdao.lib.geometry.stl_group.STLGroup = openmdao.lib.components.stl_group:STLGroup
      openmdao.lib.geometry.box.BoxParametricGeometry = openmdao.lib.components.box:BoxParametricGeometry
      openmdao.lib.components.multi_metamodel.MultiFiMetaModel = openmdao.lib.components.multi_metamodel:MultiFiMetaModel

      [openmdao.surrogatemodel]
      openmdao.lib.surrogatemodels.kriging_surrogate.KrigingSurrogate = openmdao.lib.surrogatemodels.kriging_surrogate:KrigingSurrogate
      openmdao.lib.surrogatemodels.kriging_surrogate.FloatKrigingSurrogate = openmdao.lib.surrogatemodels.kriging_surrogate:FloatKrigingSurrogate
      openmdao.lib.surrogatemodels.multifi_cokriging_surrogate.MultiFiCoKrigingSurrogate = openmdao.lib.surrogatemodels.multifi_cokriging_surrogate:MultiFiCoKrigingSurrogate
      openmdao.lib.surrogatemodels.multifi_cokriging_surrogate.FloatMultiFiCoKrigingSurrogate = openmdao.lib.surrogatemodels.multifi_cokriging_surrogate:FloatMultiFiCoKrigingSurrogate
      openmdao.lib.surrogatemodels.logistic_regression.LogisticRegression = openmdao.lib.surrogatemodels.logistic_regression:LogisticRegression
      openmdao.lib.surrogatemodels.response_surface.ResponseSurface = openmdao.lib.surrogatemodels.response_surface:ResponseSurface

      [openmdao.optproblem]
      openmdao.lib.optproblems.sellar.SellarProblem = openmdao.lib.optproblems.sellar:SellarProblem
      openmdao.lib.optproblems.branin.BraninProblem = openmdao.lib.optproblems.branin:BraninProblem
      openmdao.lib.optproblems.polyscale.PolyScalableProblem = openmdao.lib.optproblems.polyscale:PolyScalableProblem

      [openmdao.caserecorder]
      openmdao.lib.casehandlers.dumpcase.DumpCaseRecorder = openmdao.lib.casehandlers.dumpcase:DumpCaseRecorder
      openmdao.lib.casehandlers.listcase.ListCaseRecorder = openmdao.lib.casehandlers.listcase:ListCaseRecorder
      openmdao.lib.casehandlers.dbcase.DBCaseRecorder = openmdao.lib.casehandlers.dbcase:DBCaseRecorder
      openmdao.lib.casehandlers.csvcase.CSVCaseRecorder = openmdao.lib.casehandlers.csvcase:CSVCaseRecorder
      openmdao.lib.casehandlers.caseset.CaseArray = openmdao.lib.casehandlers.caseset:CaseArray
      openmdao.lib.casehandlers.caseset.CaseSet = openmdao.lib.casehandlers.caseset:CaseSet
      openmdao.lib.casehandlers.jsoncase.JSONCaseRecorder = openmdao.lib.casehandlers.jsoncase:JSONCaseRecorder
      openmdao.lib.casehandlers.jsoncase.BSONCaseRecorder = openmdao.lib.casehandlers.jsoncase:BSONCaseRecorder

      [openmdao.caseiterator]
      openmdao.lib.casehandlers.listcase.ListCaseIterator = openmdao.lib.casehandlers.listcase:ListCaseIterator
      openmdao.lib.casehandlers.dbcase.DBCaseIterator = openmdao.lib.casehandlers.dbcase:DBCaseIterator
      openmdao.lib.casehandlers.csvcase.CSVCaseIterator = openmdao.lib.casehandlers.csvcase:CSVCaseIterator
      openmdao.lib.casehandlers.caseset.CaseArray = openmdao.lib.casehandlers.caseset:CaseArray
      openmdao.lib.casehandlers.caseset.CaseSet = openmdao.lib.casehandlers.caseset:CaseSet

      [openmdao.casefilter]
      openmdao.lib.casehandlers.filters.ExprCaseFilter = openmdao.lib.casehandlers.filters:ExprCaseFilter
      openmdao.lib.casehandlers.filters.IteratorCaseFilter = openmdao.lib.casehandlers.filters:IteratorCaseFilter
      openmdao.lib.casehandlers.filters.SequenceCaseFilter = openmdao.lib.casehandlers.filters:SequenceCaseFilter
      openmdao.lib.casehandlers.filters.SliceCaseFilter = openmdao.lib.casehandlers.filters:SliceCaseFilter

      [openmdao.doegenerator]
      openmdao.lib.doegenerators.full_factorial.FullFactorial = openmdao.lib.doegenerators.full_factorial:FullFactorial
      openmdao.lib.doegenerators.central_composite.CentralComposite = openmdao.lib.doegenerators.central_composite:CentralComposite
      openmdao.lib.doegenerators.optlh.OptLatinHypercube = openmdao.lib.doegenerators.optlh:OptLatinHypercube
      openmdao.lib.doegenerators.uniform.Uniform = openmdao.lib.doegenerators.uniform:Uniform
      openmdao.lib.doegenerators.csvfile.CSVFile = openmdao.lib.doegenerators.csvfile:CSVFile

      [openmdao.architecture]
      openmdao.lib.architectures.bliss.BLISS = openmdao.lib.architectures.bliss:BLISS
      openmdao.lib.architectures.co.CO = openmdao.lib.architectures.co:CO
      openmdao.lib.architectures.ego.EGO = openmdao.lib.architectures.ego:EGO
      openmdao.lib.architectures.mdf.MDF = openmdao.lib.architectures.mdf:MDF

      [openmdao.parametric_geometry]
      openmdao.lib.geometry.box.BoxParametricGeometry = openmdao.lib.geometry.box:BoxParametricGeometry

      [openmdao.binpub]
      openmdao.lib.geometry.stl.STLSender = openmdao.lib.geometry.stl:STLSender
      openmdao.lib.geometry.box.BoxSender = openmdao.lib.geometry.box:BoxSender
      openmdao.lib.geometry.stl_group.STLGroupSender = openmdao.lib.geometry.stl_group:STLGroupSender

      """,
      )
