import os
from tempfile import mkdtemp
import os.path
import shutil

from openmdao.main.api import Assembly, Component, Driver, \
     SequentialWorkflow, Case
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.uncertain_distributions import NormalDistribution

from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.doegenerators.uniform import Uniform

from openmdao.lib.datatypes.api import Float, Int, Instance, Str, Array

from openmdao.lib.api import MetaModel,MultiObjExpectedImprovement,\
     ProbIntersect,ParetoFilter,DOEdriver,Genetic,CaseIteratorDriver,\
     DBCaseRecorder,DumpCaseRecorder,DBCaseIterator, Mux

from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions

@add_delegate(HasStopConditions)

class ConceptA(Component):
    x = Float(iotype="in",low=-3.5,high=10)
    y = Float(iotype="in",low=0.8,high=1.2)
    z = Float(iotype="in",low=0.8,high=1.2)

    f1 = Float(0.,iotype="out")
    f2 = Float(0.,iotype="out")
    
    def execute(self):
        self.f1 = self.x*self.y
        self.f2 =  (12./(self.x+4)-20.)*self.z

class Analysis(Assembly):

    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        self._tdir = mkdtemp()
        
        #Component
        self.add("A",ConceptA())

        #Driver
        self.add("DOE_A",DOEdriver())
        #self.DOE_A.sequential = True
        #self.DOE_A.DOEgenerator = FullFactorial(num_levels = 3)
        self.DOE_A.DOEgenerator = Uniform(num_levels = 3)
        self.DOE_A.add_parameter("A.x")
        self.DOE_A.add_parameter("A.y")
        self.DOE_A.case_outputs = ['A.f1','A.f2']
        self.DOE_A.recorder = DBCaseRecorder(os.path.join(self._tdir,'A.db'))

        #Iteration Hierarchy
        self.driver.workflow.add(['DOE_A'])
        self.DOE_A.workflow.add('A')

    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    import sys
    from openmdao.main.api import set_as_top
    from openmdao.lib.casehandlers.db import case_db_to_dict

    analysis = Analysis()
    
    set_as_top(analysis)

    analysis.run()
    
    DOE1 = case_db_to_dict(os.path.join(analysis._tdir,'A.db'),['A.x','A.y','A.z','A.f1','A.f2'])
    DOE2 = case_db_to_dict(os.path.join(analysis._tdir,'A.db'),['A.x','A.y','A.f1','A.f2'])
    
    print DOE1
    print DOE2
    
    analysis.cleanup()
    

    