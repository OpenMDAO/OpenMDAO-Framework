import os
from tempfile import mkdtemp
import os.path
import shutil

from openmdao.lib.datatypes.api import Instance, Str, Array, Float, Int

from openmdao.main.api import Assembly, Component, Driver, \
     SequentialWorkflow, Case
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.uncertain_distributions import NormalDistribution
from openmdao.main.hasstopcond import HasStopConditions

from openmdao.lib.api import MetaModel, MultiObjExpectedImprovement,\
     ParetoFilter, DOEdriver, Genetic, CaseIteratorDriver, DBCaseIterator,\
     DBCaseRecorder, DumpCaseRecorder, Mux

from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate

from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.doegenerators.full_factorial import FullFactorial

from openmdao.examples.expected_improvement.spiral_component import SpiralComponent

from openmdao.util.decorators import add_delegate

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
    """Custom driver to retrain the MetaModel each iteration. Also records each 
    retrain case"""
    
    def __init__(self,doc=None):
        super(MyDriver,self).__init__(doc)
        
        self.ins = ['spiral_meta_model.x','spiral_meta_model.y']
        self.outs = ['spiral_meta_model.f1_xy','spiral_meta_model.f2_xy']  
        
    def execute(self):
        self.set_events()
        self.run_iteration()
        
        inputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.ins]
        outputs = [(name,None,ExprEvaluator(name,self.parent).evaluate()) for name in self.outs]
        
        case = Case(inputs = inputs,
                    outputs = outputs)
        self.recorder.record(case)
        
class Analysis(Assembly):
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        self._tdir = mkdtemp()
        
        #Components
        self.add("spiral_meta_model",MetaModel())
        self.spiral_meta_model.surrogate = KrigingSurrogate()
        self.spiral_meta_model.model = SpiralComponent()
        self.spiral_meta_model.recorder = DBCaseRecorder(':memory:')
        self.spiral_meta_model.force_execute = True
        
        self.add("MOEI",MultiObjExpectedImprovement())
        self.MOEI.criteria = ['spiral_meta_model.f1_xy','spiral_meta_model.f2_xy']
        
        self.add("filter",ParetoFilter())
        self.filter.criteria = ['spiral_meta_model.f1_xy','spiral_meta_model.f2_xy']
        self.filter.case_sets = [self.spiral_meta_model.recorder.get_iterator()]
        self.filter.force_execute = True
        
        #Driver Configuration
        self.add("DOE_trainer",DOEdriver())
        self.DOE_trainer.sequential = True
        self.DOE_trainer.DOEgenerator = OptLatinHypercube(25, 2)
        self.DOE_trainer.add_parameter("spiral_meta_model.x")
        self.DOE_trainer.add_parameter("spiral_meta_model.y")
        self.DOE_trainer.add_event("spiral_meta_model.train_next")
        self.DOE_trainer.case_outputs = ['spiral_meta_model.f1_xy',
                                         'spiral_meta_model.f2_xy']
        self.DOE_trainer.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer.db'))
        
        self.add("MOEI_opt",Genetic())
        self.MOEI_opt.opt_type = "maximize"
        self.MOEI_opt.population_size = 100
        self.MOEI_opt.generations = 10
        self.MOEI_opt.selection_method = "tournament"
        self.MOEI_opt.add_parameter("spiral_meta_model.x")
        self.MOEI_opt.add_parameter("spiral_meta_model.y")
        self.MOEI_opt.add_objective("MOEI.EI")
        self.MOEI_opt.force_execute = True
        
        self.add("retrain",MyDriver())
        self.retrain.add_event("spiral_meta_model.train_next")
        self.retrain.recorder = DBCaseRecorder(os.path.join(self._tdir,'retrain.db'))
        self.retrain.force_execute = True
        
        self.add("iter",Iterator())
        self.iter.iterations = 15
        self.iter.add_stop_condition('MOEI.EI <= .0001')
        
        self.add("EI_mux",Mux(2))
        
        #Iteration Heirarchy
        self.driver.workflow.add([self.DOE_trainer,self.iter])
        
        self.DOE_trainer.workflow.add(self.spiral_meta_model)
        
        self.iter.workflow = SequentialWorkflow()
        self.iter.workflow.add([self.filter, self.MOEI_opt, self.retrain])
        
        self.MOEI_opt.workflow.add([self.spiral_meta_model,self.EI_mux,self.MOEI])
        self.retrain.workflow.add(self.spiral_meta_model)
        
        #Data Connections
        self.connect("filter.pareto_set","MOEI.best_cases")
        self.connect("spiral_meta_model.f1_xy","EI_mux.input_1")
        self.connect("spiral_meta_model.f2_xy","EI_mux.input_2")
        self.connect("EI_mux.output","MOEI.predicted_values")
        
    def cleanup(self):
        """cleans up any files left in the temp directory from execution"""
        shutil.rmtree(self._tdir, ignore_errors=True)

if __name__ == "__main__": #pragma: no cover
    import sys
    from openmdao.main.api import set_as_top
    from openmdao.lib.caserecorders.dbcaserecorder import case_db_to_dict
    
    import matplotlib
    if sys.platform == 'win32':
        matplotlib.use('WxAgg')
    
    from matplotlib import pyplot as plt, cm 
    from matplotlib.pylab import get_cmap
    from numpy import meshgrid,array, pi,arange,cos,sin
    
    
    #create the analysis
    analysis = Analysis()
    set_as_top(analysis)
    #run the analysis
    analysis.run()
    
    
    #plot the samples points, along with the data from the function
    def f1(x,y):
        return cos(x)/x+sin(y)/y
        
    def f2(x,y):
        return sin(x)/x+cos(y)/y

    X_range = arange(0.75,5.*pi,0.5)
    Y_range = arange(0.75,5.*pi,0.5)
    
    X , Y = meshgrid(X_range,Y_range)
    Z1,Z2 = f1(X,Y),f2(X,Y)
    
    plt.figure()
    plt.subplot(121)
    plt.contour(X,Y,Z1,50)
    plt.axis([0.75,5*pi,0.75,5*pi])
    
    plt.subplot(122)
    plt.contour(X,Y,Z2,50)
    cb = plt.colorbar(shrink=.6)
    plt.axis([0.75,5*pi,0.75,5*pi])

    plt.figure()
    
    Z1_pred = []
    Z2_pred = []
        
    for x_row,y_row in zip(X,Y): 
        row1 = []
        row2 = []
        for x,y in zip(x_row,y_row): 
            analysis.spiral_meta_model.x = x
            analysis.spiral_meta_model.y = y
            analysis.spiral_meta_model.execute()
            row1.append(analysis.spiral_meta_model.f1_xy.mu)
            row2.append(analysis.spiral_meta_model.f2_xy.mu)
        Z1_pred.append(row1)        
        Z2_pred.append(row2)
    Z1_pred = array(Z1_pred)
    Z2_pred = array(Z2_pred)
    
    #plot the initial training data
    data_train = case_db_to_dict(os.path.join(analysis._tdir,'trainer.db'),
                                     ['spiral_meta_model.x',
                                      'spiral_meta_model.y',
                                      'spiral_meta_model.f1_xy',
                                      'spiral_meta_model.f2_xy'])

    plt.scatter(data_train['spiral_meta_model.x'],
                data_train['spiral_meta_model.y'],s=30,c='#572E07',zorder=10)
    
    data_EI = case_db_to_dict(os.path.join(analysis._tdir,'retrain.db'),
                                     ['spiral_meta_model.y',
                                      'spiral_meta_model.x',
                                      'spiral_meta_model.f1_xy',
                                      'spiral_meta_model.f2_xy'])
    
    count = len(data_EI['spiral_meta_model.x'])
    colors = arange(0,count)/float(count)
    color_map = get_cmap('spring')

    f1_train = [case.mu for case in data_train['spiral_meta_model.f1_xy']]
    f2_train = [case.mu for case in data_train['spiral_meta_model.f2_xy']]
    f1_iter = [case.mu for case in data_EI['spiral_meta_model.f1_xy']]
    f2_iter = [case.mu for case in data_EI['spiral_meta_model.f2_xy']]
    
    plt.subplot(121)
    plt.contour(X,Y,Z1_pred,50)
    plt.scatter(data_train['spiral_meta_model.x'],
                data_train['spiral_meta_model.y'],s=30,c='#572E07',zorder=10)
    plt.scatter(data_EI['spiral_meta_model.x'],data_EI['spiral_meta_model.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)
    plt.axis([0.75,5*pi,0.75,5*pi])
    
    plt.subplot(122)
    plt.contour(X,Y,Z2_pred,50)
    cb = plt.colorbar(shrink=.6)
    plt.scatter(data_train['spiral_meta_model.x'],
                data_train['spiral_meta_model.y'],s=30,c='#572E07',zorder=10)
    plt.scatter(data_EI['spiral_meta_model.x'],data_EI['spiral_meta_model.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)

    plt.axis([0.75,5*pi,0.75,5*pi])
    
    plt.figure()
    plt.scatter(Z1,Z2)
    plt.scatter(f1_train,f2_train,s=30,c='#572E07',zorder=10)
    plt.scatter(f1_iter,f2_iter,s=30,c=colors,zorder=11,cmap=color_map)
    
    plt.show()
    analysis.cleanup()
    