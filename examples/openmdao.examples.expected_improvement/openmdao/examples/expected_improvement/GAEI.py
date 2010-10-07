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
        print "x     : ", analysis.GAEI_opt.best_individual[0]
        print "P_int : ", analysis.probInt.PInt
        print "PI    : ", analysis.MOEI.PI
        print "EI    : ", analysis.MOEI.EI
        print
        inputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.ins]
        outputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.outs]
        
        case = Case(inputs = inputs,
                    outputs = outputs)
        self.recorder.record(case)

        base = analysis.iter.iterations/2*100+10
        if remainder(analysis.iter._iterations,2.) == 0:
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

            y_train = [case for case in data_train1['c1.x']]+[case for case in data_EI['c1.x']][:-2]
            f1_train = [case.mu for case in data_train1['c1.f1']]+[case.mu for case in data_EI['c1.f1']][:-2]
            f2_train = [case.mu for case in data_train1['c1.f2']]+[case.mu for case in data_EI['c1.f2']][:-2]
            
            y_iter  = [case for case in data_EI['c1.x']][-2:]
            f1_iter = [case.mu for case in data_EI['c1.f1']][-2:]
            f2_iter = [case.mu for case in data_EI['c1.f2']][-2:]
            
            t = 'Iteration ' + str(analysis.iter._iterations)
            
            des1 = des.add_subplot(sp)
            des1.plot(X,Z1,'r')
            des1.plot(X,Z2,'b')
            des1.plot(X,Z1_pred,'r--')
            des1.plot(X,Z2_pred,'b--')
            des1.scatter(y_train,f1_train,s=8,c='k',zorder=10)
            des1.scatter(y_iter,f1_iter,s=20,c='r',zorder=11)
            des1.scatter(y_train,f2_train,s=8,c='k',zorder=10)
            des1.scatter(y_iter,f2_iter,s=20,c='r',zorder=11)
            des1.set_xlim(0,1)
            des1.set_ylim(-15,20)
            
            par1 = par.add_subplot(sp)
            par1.plot(Z1,Z2,'k')
            par1.plot(Z1_pred,Z2_pred,'k--')
            par1.scatter(f1_train,f2_train,s=8,c='k',zorder=10)
            par1.scatter(f1_iter,f2_iter,s=20,c='r',zorder=11)
            
            par1.plot(ZZ1,ZZ2,'g')
            #par1.plot(ZZ1_pred,ZZ2_pred,'r--')
            #par1.scatter(ff1_train,ff2_train,s=5,c='k',zorder=12)
            
            par1.text(-7,6,t)
            par1.set_xlim(-10,20)
            par1.set_ylim(-20,10)
            
            if analysis.iter._iterations == 2:
                des1.legend(('$f_1(x)$','$f_2(x)$','$f_1(x)$ Prediction','$f_2(x)$ Prediction'),loc=1,ncol=2,prop={'size':9})
                par1.legend(('Concept A','Concept A Prediction'),loc=7,prop={'size':8})
                des1.text(0.6,-10,t)
            else:
                des1.text(0.05,13,t)
            
            if analysis.iter._iterations == analysis.iter.iterations:
                des1.set_xlabel('$x$',fontsize=15)
                par1.set_xlabel('$f_1$',fontsize=10)

class TwoMux(Component):
    one = Instance(NormalDistribution,iotype="in")
    two = Instance(NormalDistribution,iotype="in")
    out = Array(iotype="out")
    def execute(self):
        self.out = [self.one,self.two]
        
class Analysis(Assembly):
    """
    Implements an adaptive sampling scheme based on GAEI
    GAEI combines multiobjective EI with a measure of 
    probability that a new design point is close to a 
    Pareto intersection.
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
        
        self.add("probInt",ProbIntersect())
        self.probInt.criteria = ['f1','f2']           

        #FILTERS
        self.add("gfilter",ParetoFilter()) #GLOBAL FILTER
        self.gfilter.criteria = ['f1','f2']
        self.gfilter.case_sets = [self.c1.recorder.get_iterator(), self.c2.recorder.get_iterator()]
        #self.gfilter.case_sets = [self.c1.recorder.get_iterator()]
        self.gfilter.force_execute = True

        self.add("filter_c1",ParetoFilter())
        self.filter_c1.criteria = ['f1','f2']
        self.filter_c1.case_sets = [self.c1.recorder.get_iterator()]
        self.filter_c1.force_execute = True

        self.add("filter_other",ParetoFilter())
        self.filter_other.criteria = ['f1','f2']
        self.filter_other.case_sets = [self.c2.recorder.get_iterator()]
        self.filter_other.force_execute = True
        
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
        
        self.add("GAEI_opt",Genetic())
        self.GAEI_opt.opt_type = "maximize"
        self.GAEI_opt.population_size = 100
        self.GAEI_opt.generations = 20
        self.GAEI_opt.selection_method = "tournament"
        self.GAEI_opt.elitism = True
        #self.GAEI_opt.seed = 1
        self.GAEI_opt.add_parameter("c1.x")
        #self.GAEI_opt.add_objective("MOEI.EI*probInt.PInt")
        self.GAEI_opt.add_objective("probInt.PInt")
        self.GAEI_opt.force_execute = True
        
        self.add("retrain",MyDriver())
        self.retrain.add_event("c1.train_next")
        self.retrain.recorder = DBCaseRecorder(os.path.join(self._tdir,'retrain.db'))
        self.retrain.force_execute = True
        
        self.add("iter",Iterator())
        self.iter.iterations = 12
        #self.iter.add_stop_condition('MOEI.EI <= .000001')
        
        self.add("muxer",TwoMux())
        
        #Iteration Heirarchy
        self.driver.workflow.add([self.DOE_trainer1, self.DOE_trainer2,self.iter])
        
        self.DOE_trainer1.workflow.add(self.c1)
        self.DOE_trainer2.workflow.add(self.c2)
        
        self.iter.workflow = SequentialWorkflow()
        self.iter.workflow.add([self.gfilter, self.filter_c1,self.filter_other,self.GAEI_opt, self.retrain])
        
        self.GAEI_opt.workflow.add([self.c1,self.muxer,self.MOEI,self.probInt])
        self.retrain.workflow.add(self.c1)
        
        #Data Connections
        self.connect("filter_c1.pareto_set","probInt.my_Pareto")
        self.connect("filter_other.pareto_set","probInt.other_Pareto")
        self.connect("gfilter.pareto_set","MOEI.best_cases")
        self.connect("c1.f1","muxer.one")
        self.connect("c1.f2","muxer.two")
        self.connect("muxer.out","MOEI.predicted_values")
        self.connect("muxer.out","probInt.predicted_values")
        
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    import sys
    from openmdao.main.api import set_as_top
    from openmdao.lib.caserecorders.dbcaserecorder import case_db_to_dict
    
    analysis = Analysis()
    
    set_as_top(analysis)

    def A1(x):
        return (6.*x-2)**2*sin(12.*x-4.)

    def A2(x):
        return 0.5*A1(x)+10.*(x-0.5)-5.

    def B1(y):
        return y
    def B2(y):
        return 12./(y+4.)-20.
        
    m = 100
    X = linspace(0,1.0,m)
    Y = linspace(-3.5,10.0,m)
    
    Z1,Z2 = A1(X),A2(X)
    ZZ1,ZZ2 = B1(Y),B2(Y)
    
    par = plt.figure(figsize=(4,14))
    des = plt.figure(figsize=(4,14))
    #par = plt.figure()
    #des = plt.figure()    
    analysis.run()    
    """
    Z1_pred = []
    Z2_pred = []
    
    for y in Y: 
        analysis.c2.y = y
        analysis.c2.execute()
        Z1_pred.append(analysis.c2.f1.mu)
        Z2_pred.append(analysis.c2.f2.mu)
    
    plt.figure()
    plt.plot(Z1,Z2)
    plt.plot(Z1_pred,Z2_pred,'r--')
    
    plt.figure()
    plt.plot(Y,Z1,'g')
    plt.plot(Y,Z2,'c')
    plt.plot(Y,Z1_pred,'g--')
    plt.plot(Y,Z2_pred,'c--')
    """
    plt.show()
    analysis.cleanup()
    