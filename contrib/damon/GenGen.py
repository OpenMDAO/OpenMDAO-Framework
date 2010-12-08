import os
from tempfile import mkdtemp
import os.path
import shutil

from openmdao.main.api import Assembly, Component, Driver, \
     SequentialWorkflow, Case
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.uncertain_distributions import NormalDistribution

from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.doegenerators.uniform import Uniform

from openmdao.lib.datatypes.api import Float, Int, Instance, Str, Array, List, implements

from openmdao.lib.api import MetaModel,MultiObjExpectedImprovement,\
     ProbIntersect,ParetoFilter,DOEdriver,Genetic,CaseIteratorDriver,\
     DBCaseRecorder,DumpCaseRecorder,DBCaseIterator, Mux

from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions

from numpy import sqrt,sort

from openmdao.main.interfaces import IDOEgenerator

class ConceptA(Component):
    x = Float(iotype="in",low=-3.5,high=10)
    y = Float(iotype="in",low=0.8,high=1.2)
    z = Float(iotype="in",low=0.8,high=1.2)

    f1 = Float(0.,iotype="out")
    f2 = Float(0.,iotype="out")
    
    def execute(self):
        self.f1 = (self.x+20.)*self.y
        self.f2 =  (10./(self.x+4)+5.)*self.z*self.y

"""        
class DOEGenGen(Driver):

    n = Int(0,iotype='in',desc='number of samples in the DOE')
    DOEgen = Instance(IDOEgenerator,iotype='out')

    def __init__(self,doc=None):
        super(DOEGenGen,self).__init__(doc)
        
    def execute(self):
        self.DOEgen = Uniform(num_samples=self.n)
"""
class Iterator(Driver):

    cases = List([],iotype='in',desc='list of integers of maximum sample size')
    n = Int(0,iotype='out',desc='max sample size of current iteration')
    
    def start_iteration(self):
        self.iterations = len(self.cases)
        self._iterations = 0
        
    def continue_iteration(self):
        self._iterations +=1
        if self._iterations > 1:
            return False
        if self._iterations <= self.iterations:
            self.n = self.cases[self._iterations-1]
            print self.n
            return True
            
        return False

class Analysis(Assembly):

    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        self._tdir = mkdtemp()
        
        #Components
        self.add("A",MetaModel())
        self.A .surrogate = KrigingSurrogate()
        self.A.model = ConceptA()
        self.A.recorder = DBCaseRecorder(':memory:')
        self.A.force_execute = True

        #Drivers
        self.add("trainA",DOEdriver())
        self.trainA.sequential = True
        self.trainA.add_parameter("A.x")
        self.trainA.add_parameter("A.y")
        self.trainA.add_parameter("A.z")
        self.trainA.DOEgenerator = Uniform()        
        self.trainA.add_event("A.train_next")
        self.trainA.case_outputs = ['A.f1','A.f2']
        self.trainA.recorder = DBCaseRecorder(os.path.join(self._tdir,'A.db'))
        self.trainA.force_execute = True
        
        #self.add('DOEgengen',DOEGenGen())
        #self.DOEgengen.force_execute = True
        
        self.add('iter',Iterator())
        self.iter.iters_per_case = 1
        self.iter.cases = [10,15,20]
        self.iter.force_execute = True

        #Iteration Hierarchy

        self.driver.workflow.add(['iter'])
        
        self.iter.workflow.add(['trainA'])
        
        self.trainA.workflow.add('A')
        
        #Data Connections
        self.connect('iter.n','trainA.DOEgenerator.num_samples')
        
        #self.connect('iter.n','DOEgengen.n')
        #self.connect('DOEgengen.DOEgen','trainA.DOEgenerator')
        
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    from openmdao.main.api import set_as_top
    analysis = Analysis()
    set_as_top(analysis)
    analysis.run()
    analysis.cleanup()
    

    