import os
from tempfile import mkdtemp
import os.path
import shutil

from openmdao.main.api import Assembly, Component, Driver, SequentialWorkflow, Case
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.uncertain_distributions import NormalDistribution

from openmdao.lib.surrogatemodels.api import KrigingSurrogate
from openmdao.lib.doegenerators.api import OptLatinHypercube,FullFactorial
from openmdao.lib.doegenerators.uniform import Uniform

from openmdao.lib.datatypes.api import Float, Int, Instance, Str, Array, List

from openmdao.lib.components.api import MetaModel,MultiObjExpectedImprovement,\
     ProbIntersect,ParetoFilter, Mux
from openmdao.lib.drivers.api import DOEdriver,Genetic,CaseIteratorDriver,FixedPointIterator
from openmdao.lib.caserecorders.api import DBCaseRecorder,DumpCaseRecorder
from openmdao.lib.caseiterators.api import DBCaseIterator

from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions
from openmdao.main.hasparameters import HasParameters

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

class DOE_Maker(Component):

    cases = List([],iotype='in',desc='list of integers of maximum sample size')
    DOEgen = Instance(IDOEgenerator,iotype='out')
    
    def __init__(self,doc=None):
        super(DOE_Maker,self).__init__(doc)
        #self.force_execute = True
    
    def execute(self):
        n = self.cases.pop()
        print 'Number of training cases = ',n
        self.DOEgen = Uniform(num_samples=n)

@add_delegate(HasStopConditions)       
class Iterator(Driver):
    
    def start_iteration(self):
        self._iterations = 0
        
    def continue_iteration(self):
        self._iterations +=1
        if self.should_stop():
            return False
        else:
            return True

        return False

class Res(Component):
    def execute(self):
        analysis.A.x = 0
        analysis.A.y = 1
        analysis.A.z = 1
        analysis.A.execute()
        print analysis.A.f1.mu
        
class Analysis(Assembly):

    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        self._tdir = mkdtemp()
        
        #Components
        self.add("A",MetaModel())
        self.A .surrogate = {'default':KrigingSurrogate()}
        self.A.model = ConceptA()
        self.A.recorder = DBCaseRecorder(':memory:')

        self.add('DOE_maker',DOE_Maker())
        self.DOE_maker.cases = [10]*3
        #self.DOE_maker.force_execute = True

        #Drivers
        self.add("trainA",DOEdriver())
        self.trainA.sequential = True
        self.trainA.add_parameter("A.x")
        self.trainA.add_parameter("A.y")
        self.trainA.add_parameter("A.z")
        self.trainA.add_event("A.train_next")
        self.trainA.case_outputs = ['A.f1','A.f2']
        self.trainA.recorder = DBCaseRecorder(os.path.join(self._tdir,'A.db'))
        
        self.add('driver',Iterator())
        self.driver.add_stop_condition('len(DOE_maker.cases)==0')
        self.driver.add_event('A.reset_training_data')
        
        self.add('res',Res())
        self.res.force_execute = True
        
        #Iteration Hierarchy
        self.driver.workflow.add(['DOE_maker','trainA','res'])
        #self.driver.workflow.add(['DOE_maker','trainA'])
        self.trainA.workflow.add('A')
        
        #Data Connections
        self.connect('DOE_maker.DOEgen','trainA.DOEgenerator')
        
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    from openmdao.main.api import set_as_top
    analysis = Analysis()
    set_as_top(analysis)
    analysis.run()
    analysis.cleanup()
    
    