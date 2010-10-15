import os
from tempfile import mkdtemp
import os.path
import shutil

from enthought.traits.api import Instance, Str, Array

from openmdao.main.api import Assembly, Component, Driver, \
     SequentialWorkflow, Case
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.uncertain_distributions import NormalDistribution

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.expected_improvement_multiobj import MultiObjExpectedImprovement
from openmdao.lib.components.prob_intersect import ProbIntersect
from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate
from openmdao.lib.components.pareto_filter import ParetoFilter
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.drivers.genetic import Genetic

from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.lib.caserecorders.dbcaserecorder import DBCaseRecorder
from openmdao.lib.caserecorders.dumpcaserecorder import DumpCaseRecorder
from openmdao.lib.caseiterators.dbcaseiter import DBCaseIterator
from openmdao.lib.api import Float, Int

from openmdao.examples.expected_improvement.alg_component1 import Alg_Component1
from openmdao.examples.expected_improvement.alg_component3 import Alg_Component3

from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions

from matplotlib import pyplot as plt, cm 
from matplotlib.pylab import get_cmap
from numpy import meshgrid,array, pi,arange,cos,sin,linspace,remainder

@add_delegate(HasStopConditions)
class Iterator(Driver):
    iterations = Int(10,iotype="in")
    
    def start_iteration(self):
        self._iterations = 0
    
    def continue_iteration(self):
        self._iterations += 1
        if (self._iterations > 1) and self.should_stop():
            return False
        if self._iterations <= self.iterations: 
            return True
    
        return False
    
class MyDriver(Driver):
    def __init__(self,doc=None):
        super(MyDriver,self).__init__(doc)
        
        self.ins = ['c1.x']
        self.outs = ['c1.f1','c1.f2']
        
    def execute(self):
        self.set_events()
        self.run_iteration()
        
        print "PI : ", analysis.MOEI.PI
        print "EI : ", analysis.MOEI.EI
        print
        inputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.ins]
        outputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.outs]
        
        case = Case(inputs = inputs,
                    outputs = outputs)
        self.recorder.record(case)

        base = analysis.iter.iterations/2*100+10
        
        print "Iteration : ", analysis.iter._iterations
        sp = analysis.iter._iterations/2+base
        Z1_pred = []
        Z2_pred = []
        ZZ1_pred = []
        ZZ2_pred = []
        for y in Y: 
            analysis.c2.y = y
            analysis.c2.execute()
            ZZ1_pred.append(analysis.c2.f1.mu)
            ZZ2_pred.append(analysis.c2.f2.mu)
        for x in X:
            analysis.c1.x = x
            analysis.c1.execute()
            Z1_pred.append(analysis.c1.f1.mu)
            Z2_pred.append(analysis.c1.f2.mu)
            
            
        #plot the initial training data
        data_train2 = case_db_to_dict(os.path.join(analysis._tdir,'trainer2.db'),['c2.y','c2.f1','c2.f2'])
        data_train1 = case_db_to_dict(os.path.join(analysis._tdir,'trainer1.db'),['c1.x','c1.f1','c1.f2'])

        data_EI = case_db_to_dict(os.path.join(analysis._tdir,'retrain.db'),['c1.x','c1.f1','c1.f2'])
        
        yy_train  = [case for case in data_train2['c2.y']]
        ff1_train = [case.mu for case in data_train2['c2.f1']]
        ff2_train = [case.mu for case in data_train2['c2.f2']]

        y_train = [case for case in data_train1['c1.x']]+[case for case in data_EI['c1.x']][:-1]
        f1_train = [case.mu for case in data_train1['c1.f1']]+[case.mu for case in data_EI['c1.f1']][:-1]
        f2_train = [case.mu for case in data_train1['c1.f2']]+[case.mu for case in data_EI['c1.f2']][:-1]
        
        y_iter  = [case for case in data_EI['c1.x']][-1:]
        f1_iter = [case.mu for case in data_EI['c1.f1']][-1:]
        f2_iter = [case.mu for case in data_EI['c1.f2']][-1:]
        
        t = 'Iteration ' + str(analysis.iter._iterations)
        desname = 'des'+str(analysis.iter._iterations+10)+'.png'
        parname = 'par'+str(analysis.iter._iterations+10)+'.png'
        
        plt.figure()
        plt.plot(X,Z1,'r')
        plt.plot(X,Z2,'b')
        plt.plot(X,Z1_pred,'r--')
        plt.plot(X,Z2_pred,'b--')
        plt.scatter(y_train,f1_train,s=8,c='k',zorder=10)
        plt.scatter(y_iter,f1_iter,s=20,c='r',zorder=11)
        plt.scatter(y_train,f2_train,s=8,c='k',zorder=10)
        plt.scatter(y_iter,f2_iter,s=20,c='r',zorder=11)
        plt.xlim(0,1)
        plt.ylim(-15,20)
        plt.legend(('$f_1(x)$','$f_2(x)$','$f_1(x)$ Prediction','$f_2(x)$ Prediction'),ncol=2,loc=9)
        plt.text(0.05,10,t)
        plt.xlabel('$x$',fontsize=20)
        plt.ylabel('$f(x)$',fontsize=20)
        plt.savefig(desname,format='png')
        
        plt.figure()
        plt.plot(Z1,Z2,'k')
        plt.plot(Z1_pred,Z2_pred,'k--')
        plt.scatter(f1_train,f2_train,s=8,c='k',zorder=10)
        plt.scatter(f1_iter,f2_iter,s=20,c='r',zorder=11)
        plt.plot(ZZ1,ZZ2,'g')
        plt.text(-7,6,t)
        plt.xlim(-10,20)
        plt.ylim(-20,10)
        plt.legend(('Concept A','Concept A Prediction','Concept B'),loc=7)
        plt.xlabel('$f_1$',fontsize=20)
        plt.ylabel('$f_2$',fontsize=20)
        plt.savefig(parname,format='png')

class TwoMux(Component):
    one = Instance(NormalDistribution,iotype="in")
    two = Instance(NormalDistribution,iotype="in")
    out = Array(iotype="out")
    def execute(self):
        self.out = [self.one,self.two]
        
class Analysis(Assembly):
    """
    Example problem that implements an intermediate step to GAEI.
    The algorithm optimizes multiobjective EI of a single model
    but the Pareto frontier is made up of globally dominant
    points from both models
    """
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        self._tdir = mkdtemp()
        
        #Components
        #CONCEPTS
        #CONCEPT C1
        self.add("c1",MetaModel())
        self.c1.surrogate = KrigingSurrogate()
        self.c1.model = Alg_Component1()
        self.c1.recorder = DBCaseRecorder(':memory:')
        self.c1.force_execute = True

        #CONCEPT C2
        self.add("c2",MetaModel())
        self.c2.surrogate = KrigingSurrogate()
        self.c2.model = Alg_Component3()
        self.c2.recorder = DBCaseRecorder(':memory:')
        self.c2.force_execute = True
        
        #SAMPLING CRITERIA CALCULATORS
        self.add("MOEI",MultiObjExpectedImprovement())
        self.MOEI.criteria = ['f1','f2']

        #FILTERS
        self.add("gfilter",ParetoFilter()) #GLOBAL FILTER
        self.gfilter.criteria = ['f1','f2']
        #self.gfilter.case_sets = [self.c1.recorder.get_iterator(), self.c2.recorder.get_iterator()]
        self.gfilter.case_sets = [self.c1.recorder.get_iterator()]
        self.gfilter.force_execute = True
        
        #Driver Configuration
        self.add("DOE_trainer1",DOEdriver())
        self.DOE_trainer1.sequential = True
        self.DOE_trainer1.DOEgenerator = FullFactorial(4, 1)
        self.DOE_trainer1.add_parameter("c1.x")
        self.DOE_trainer1.add_event("c1.train_next")
        self.DOE_trainer1.case_outputs = ['c1.f1','c1.f2']
        self.DOE_trainer1.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer1.db'))
        
        self.add("DOE_trainer2",DOEdriver())
        self.DOE_trainer2.sequential = True
        self.DOE_trainer2.DOEgenerator = FullFactorial(10, 1)
        self.DOE_trainer2.add_parameter("c2.y")
        self.DOE_trainer2.add_event("c2.train_next")
        self.DOE_trainer2.case_outputs = ['c2.f1','c2.f2']
        self.DOE_trainer2.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer2.db'))
        
        self.add("MOEI_opt",Genetic())
        self.MOEI_opt.opt_type = "maximize"
        self.MOEI_opt.population_size = 100
        self.MOEI_opt.generations = 20
        self.MOEI_opt.selection_method = "tournament"
        self.MOEI_opt.elitism = True
        #self.MOEI_opt.seed = 1
        self.MOEI_opt.add_parameter("c1.x")
        #self.MOEI_opt.add_objective("MOEI.PI")
        self.MOEI_opt.add_objective("MOEI.EI")

        self.MOEI_opt.force_execute = True
        
        self.add("retrain",MyDriver())
        self.retrain.add_event("c1.train_next")
        self.retrain.recorder = DBCaseRecorder(os.path.join(self._tdir,'retrain.db'))
        self.retrain.force_execute = True
        
        self.add("iter",Iterator())
        self.iter.iterations = 12
        #self.iter.add_stop_condition('MOEI.EI <= .00001')
        
        self.add("muxer",TwoMux())
        
        #Iteration Heirarchy
        self.driver.workflow.add([self.DOE_trainer1, self.DOE_trainer2,self.iter])
        
        self.DOE_trainer1.workflow.add(self.c1)
        self.DOE_trainer2.workflow.add(self.c2)
        
        self.iter.workflow = SequentialWorkflow()
        self.iter.workflow.add([self.gfilter,self.MOEI_opt, self.retrain])
        
        self.MOEI_opt.workflow.add([self.c1,self.muxer,self.MOEI])
        self.retrain.workflow.add(self.c1)
        
        #Data Connections
        self.connect("gfilter.pareto_set","MOEI.best_cases")
        self.connect("c1.f1","muxer.one")
        self.connect("c1.f2","muxer.two")
        self.connect("muxer.out","MOEI.predicted_values")

        
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    import sys
    from openmdao.main.api import set_as_top
    from openmdao.lib.caserecorders.dbcaserecorder import case_db_to_dict
    
    analysis = Analysis()
    
    set_as_top(analysis)

    def C1(y):
        return y
    def C2(y):
        return 12./(y+4.)-20.
    
    def A1(x):
        return (6.*x-2)**2*sin(12.*x-4.)

    def A2(x):
        return 0.5*A1(x)+10.*(x-0.5)-5.
        
    m = 400
    Y = linspace(-3.5,10.0,m)
    X = linspace(0,1.0,m)
    
    ZZ1,ZZ2 = C1(Y),C2(Y)
    Z1,Z2 = A1(X),A2(X)
    
    analysis.run()    

    #plt.show()
    analysis.cleanup()
    