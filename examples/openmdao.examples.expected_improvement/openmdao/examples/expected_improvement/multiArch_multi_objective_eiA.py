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
from openmdao.examples.expected_improvement.alg_component2 import Alg_Component2

from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions

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
        
        self.ins = ['alg1_meta_model.x']
        self.outs = ['alg1_meta_model.f1','alg1_meta_model.f2']
        
    def execute(self):
        self.set_events()
        self.run_iteration()
        print analysis.MOEI.EI

        inputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.ins]
        outputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.outs]
        
        case = Case(inputs = inputs,
                    outputs = outputs)
        self.recorder.record(case)

class TwoMux(Component):
    one = Instance(NormalDistribution,iotype="in")
    two = Instance(NormalDistribution,iotype="in")
    out = Array(iotype="out")
    def execute(self):
        self.out = [self.one,self.two]
        
class Analysis(Assembly):
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        self._tdir = mkdtemp()
        
        #Components
        self.add("alg1_meta_model",MetaModel())
        self.alg1_meta_model.surrogate = KrigingSurrogate()
        self.alg1_meta_model.model = Alg_Component1()
        self.alg1_meta_model.recorder = DBCaseRecorder(':memory:')
        self.alg1_meta_model.force_execute = True
        
        self.add("MOEI",MultiObjExpectedImprovement())
        self.MOEI.criteria = ['alg1_meta_model.f1','alg1_meta_model.f2']
        
        self.add("filter",ParetoFilter())
        self.filter.criteria = ['alg1_meta_model.f1','alg1_meta_model.f2']
        self.filter.case_sets = [self.alg1_meta_model.recorder.get_iterator()]
        self.filter.force_execute = True
        
        #Driver Configuration
        self.add("DOE_trainer",DOEdriver())
        self.DOE_trainer.sequential = True
        #self.DOE_trainer.DOEgenerator = OptLatinHypercube(10, 1)
        self.DOE_trainer.DOEgenerator = FullFactorial(4, 1)
        
        self.DOE_trainer.add_parameter("alg1_meta_model.x")
        self.DOE_trainer.add_event("alg1_meta_model.train_next")
        self.DOE_trainer.case_outputs = ['alg1_meta_model.f1',
                                         'alg1_meta_model.f2']
        self.DOE_trainer.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer.db'))
        
        self.add("MOEI_opt",Genetic())
        self.MOEI_opt.opt_type = "maximize"
        self.MOEI_opt.population_size = 100
        self.MOEI_opt.generations = 20
        self.MOEI_opt.selection_method = "tournament"
        self.MOEI_opt.elitism = True
        self.MOEI_opt.add_parameter("alg1_meta_model.x")
        self.MOEI_opt.add_objective("MOEI.EI")
        #self.MOEI_opt.add_objective("MOEI.PI")
        self.MOEI_opt.force_execute = True
        
        self.add("retrain",MyDriver())
        self.retrain.add_event("alg1_meta_model.train_next")
        self.retrain.recorder = DBCaseRecorder(os.path.join(self._tdir,'retrain.db'))
        self.retrain.force_execute = True
        
        self.add("iter",Iterator())
        self.iter.iterations = 3
        self.iter.add_stop_condition('MOEI.EI <= .0001')
        
        self.add("EI_mux",TwoMux())
        
        #Iteration Heirarchy
        self.driver.workflow.add([self.DOE_trainer,self.iter])
        
        self.DOE_trainer.workflow.add(self.alg1_meta_model)
        
        self.iter.workflow = SequentialWorkflow()
        self.iter.workflow.add([self.filter, self.MOEI_opt, self.retrain])
        
        self.MOEI_opt.workflow.add([self.alg1_meta_model,self.EI_mux,self.MOEI])
        self.retrain.workflow.add(self.alg1_meta_model)
        
        #Data Connections
        self.connect("filter.pareto_set","MOEI.best_cases")
        self.connect("alg1_meta_model.f1","EI_mux.one")
        self.connect("alg1_meta_model.f2","EI_mux.two")
        self.connect("EI_mux.out","MOEI.predicted_values")
        
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    import sys
    from openmdao.main.api import set_as_top
    from openmdao.lib.caserecorders.dbcaserecorder import case_db_to_dict
    
    #seed = None
    #backend = None
    #figname = None
    #for arg in sys.argv[1:]:
    #    if arg.startswith('--seed='):
    #        import random
    #        seed = int(arg.split('=')[1])
    #        random.seed(seed)
    #    if arg.startswith('--backend='):
    #        backend = arg.split('=')[1]
    #    if arg.startswith('--figname='):
    #        figname = arg.split('=')[1]
    #
    
    import matplotlib
    
    #if backend is not None:
    #    matplotlib.use(backend)
    #elif sys.platform == 'win32':
    #    matplotlib.use('WxAgg')
    import matplotlib
    #matplotlib.use("WxAgg")
    from matplotlib import pyplot as plt, cm 
    from matplotlib.pylab import get_cmap
    from numpy import meshgrid,array, pi,arange,cos,sin
        
    
    analysis = Analysis()
    
    set_as_top(analysis)
    
    analysis.run()
    
    print "pareto set:",[case.inputs for case in analysis.filter.pareto_set]
    print "best cases" , [case.inputs for case in analysis.MOEI.best_cases]
    
    def f1(x):
        return (6.*x-2)**2*sin(12.*x-4.)
    
    def f2(x):
        return 0.5*f1(x)+10.*(x-0.5)-5.
    
    inc = 0.01
    X = arange(0,1.+inc,inc)
    Z1,Z2 = f1(X),f2(X)
    
    
    Z1_pred = []
    Z2_pred = []
        
    for x in X: 
        analysis.alg1_meta_model.x = x
        analysis.alg1_meta_model.execute()
        Z1_pred.append(analysis.alg1_meta_model.f1.mu)
        Z2_pred.append(analysis.alg1_meta_model.f2.mu)
    
    #plot the initial training data
    data_train = case_db_to_dict(os.path.join(analysis._tdir,'trainer.db'),
                                     ['alg1_meta_model.x',
                                      'alg1_meta_model.f1',
                                      'alg1_meta_model.f2'])
   
    data_EI = case_db_to_dict(os.path.join(analysis._tdir,'retrain.db'),
                                     ['alg1_meta_model.x',
                                      'alg1_meta_model.f1',
                                      'alg1_meta_model.f2'])
    
    count = len(data_EI['alg1_meta_model.x'])
    colors = arange(0,count)/float(count)
    color_map = get_cmap('spring')
    
    f1_train = [case.mu for case in data_train['alg1_meta_model.f1']]
    f2_train = [case.mu for case in data_train['alg1_meta_model.f2']]
    f1_iter  = [case.mu for case in data_EI['alg1_meta_model.f1']]
    f2_iter  = [case.mu for case in data_EI['alg1_meta_model.f2']]
    
    plt.figure()
    
    plt.subplot(121)
    plt.plot(X,Z1,'b')
    plt.plot(X,Z1_pred,'b--')
    plt.scatter(data_train['alg1_meta_model.x'],f1_train,s=30,c='#572E07',zorder=10)
    plt.scatter(data_EI['alg1_meta_model.x'],f1_iter,s=30,c=colors,zorder=11)
    
    plt.subplot(122)
    plt.plot(X,Z2,'r')
    plt.plot(X,Z2_pred,'r--')
    plt.scatter(data_train['alg1_meta_model.x'],f2_train,s=30,c='#572E07',zorder=10)
    plt.scatter(data_EI['alg1_meta_model.x'],f2_iter,s=30,c=colors,zorder=11)
    
    plt.figure()
    plt.plot(Z1,Z2)
    plt.plot(Z1_pred,Z2_pred,'b--')
    plt.scatter(f1_train,f2_train,s=30,c='#572E07',zorder=10)
    plt.scatter(f1_iter,f2_iter,s=30,c=colors,zorder=11)
    
    plt.show()
    analysis.cleanup()
    