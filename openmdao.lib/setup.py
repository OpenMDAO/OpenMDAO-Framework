
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
      license='Apache License, Version 2.0',
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
          'Pyevolve==0.6',
          'ordereddict',
          ],
      extras_require = {
          'numpy_comps': ['numpy', 'conmin==1.0.1', 'newsumt==1.1.0'],
          'scipy_comps': ['scipy'],
      },
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
      openmdao.lib.drivers.sensitivity.SensitivityDriver = openmdao.lib.drivers.sensitivity:SensitivityDriver

      [openmdao.component]
      openmdao.lib.components.expected_improvement.ExpectedImprovement = openmdao.lib.components.expected_improvement:ExpectedImprovement
      openmdao.lib.components.expected_improvement_multiobj.MultiObjExpectedImprovement = openmdao.lib.components.expected_improvement_multiobj:MultiObjExpectedImprovement
      openmdao.lib.components.external_code.ExternalCode = openmdao.lib.components.external_code:ExternalCode
      openmdao.lib.components.metamodel.MetaModel = openmdao.lib.components.metamodel:MetaModel
      openmdao.lib.components.mux.Mux = openmdao.lib.components.mux:Mux
      openmdao.lib.components.mux.DeMux = openmdao.lib.components.mux:DeMux
      openmdao.lib.components.broadcaster.Broadcaster = openmdao.lib.components.broadcaster:Broadcaster
      openmdao.lib.components.pareto_filter.ParetoFilter = openmdao.lib.components.pareto_filter:ParetoFilter

      [openmdao.differentiator]
      openmdao.lib.differentiators.finite_difference.FiniteDifference = openmdao.lib.differentiators.finite_difference:FiniteDifference
      
      [openmdao.variable]
      openmdao.lib.datatypes.array.Array = openmdao.lib.datatypes.array:Array
      
      [openmdao.surrogatemodel]
      openmdao.lib.surrogatemodels.kriging_surrogate.KrigingSurrogate = openmdao.lib.surrogatemodels.kriging_surrogate:KrigingSurrogate
      openmdao.lib.surrogatemodels.logistic_regression.LogisticRegression = openmdao.lib.surrogatemodels.logistic_regression:LogisticRegression
      openmdao.lib.surrogatemodels.nn_surrogate.NeuralNet = openmdao.lib.surrogatemodels.nn_surrogate:NeuralNet
      
      [openmdao.optproblems]
      openmdao.lib.optproblems.sellar.SellarProblem = openmdao.lib.optprobelems.sellar:SellarProblem
      openmdao.lib.optproblems.branin.BraninProblem = openmdao.lib.optprobelems.branin:BraninProblem
      
      [openmdao.caserecorder]
      openmdao.lib.casehandlers.dumpcaserecorder.DumpCaseRecorder = openmdao.lib.casehandlers.dumpcaserecorder:DumpCaseRecorder
      openmdao.lib.casehandlers.listcaserecorder.ListCaseRecorder = openmdao.lib.casehandlers.listcaserecorder:ListCaseRecorder
      openmdao.lib.casehandlers.db.DBCaseRecorder = openmdao.lib.casehandlers.db:DBCaseRecorder
      openmdao.lib.casehandlers.caseset.CaseArray = openmdao.lib.casehandlers.caseset:CaseArray
      openmdao.lib.casehandlers.caseset.CaseSet = openmdao.lib.casehandlers.caseset:CaseSet

      [openmdao.caseiterator]
      openmdao.lib.casehandlers.listcaseiter.ListCaseIterator = openmdao.lib.casehandlers.listcaseiter:ListCaseIterator
      openmdao.lib.casehandlers.db.DBCaseIterator = openmdao.lib.casehandlers.db:DBCaseIterator
      openmdao.lib.casehandlers.caseset.CaseArray = openmdao.lib.casehandlers.caseset:CaseArray
      openmdao.lib.casehandlers.caseset.CaseSet = openmdao.lib.casehandlers.caseset:CaseSet
      
      [openmdao.doegenerator]
      openmdao.lib.doegenerators.full_factorial.FullFactorial = openmdao.lib.doegenerators.full_factorial:FullFactorial
      openmdao.lib.doegenerators.central_composite.CentralComposite = openmdao.lib.doegenerators.central_composite:CentralComposite
      openmdao.lib.doegenerators.optlh.OptLatinHypercube = openmdao.lib.doegenerators.optlh:OptLatinHypercube
      openmdao.lib.doegenerators.uniform.Uniform = openmdao.lib.doegenerators.uniform:Uniform

      [openmdao.architecture]
      openmdao.lib.architectures.bliss.BLISS = openmdao.lib.architectures.bliss:BLISS
      openmdao.lib.architectures.co.CO = openmdao.lib.architectures.co:CO
      openmdao.lib.architectures.ego.EGO = openmdao.lib.architectures.ego:EGO
      openmdao.lib.architectures.mdf.MDF = openmdao.lib.architectures.mdf:MDF
      """,
      )
