import os
from tempfile import mkdtemp
import os.path
import shutil

from openmdao.main.api import Assembly, Component, Driver, \
     SequentialWorkflow, Case
from openmdao.main.interfaces import ICaseIterator
from openmdao.main.expreval import ExprEvaluator

from openmdao.lib.components.api import MetaModel, ExpectedImprovement, ParetoFilter
from openmdao.lib.surrogatemodels.api import KrigingSurrogate
from openmdao.lib.drivers.api import DOEdriver, Genetic, CaseIteratorDriver, IterateUntil

from openmdao.lib.doegenerators.api import OptLatinHypercube, FullFactorial
from openmdao.lib.caserecorders.api import DBCaseRecorder, DumpCaseRecorder

from openmdao.lib.caseiterators.api import DBCaseIterator
from openmdao.lib.datatypes.api import Instance, Str, Array, Float, Int
from openmdao.examples.expected_improvement.branin_component import BraninComponent
from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions
        

#TODO: Can the DesVar objects allow some sort of mapping, used for normalization?
#    Normalization is common in problem formulation, and it would be nice to support it 
#    without having to make changes to the components themselves

#TODO: Currently, I just create a broadcaster to make use of these vars. But
#      They could be special objects that the driver sets the value of directly, and 
#      then they push the value to wherever... including any mapping functions needed for
#      the above thoughts on normalization
class GlobalDesVar(object): 
    def __init__(self): 
        self.name = None
        self.targets = []
        self.low = None
        self.high = None
        
class LocalDesVar(object): 
    def __init__(self): 
        self.target = ""
        self.low = None
        self.high = None      
        
class CouplingVar(object): 
    def __init__(self): 
        self.vary = None
        self.constraint= None
    
class MyDriver(Driver): 
    def __init__(self,doc=None):
        super(MyDriver,self).__init__(doc)
        
        self.ins = ['branin.x','branin.y']
        self.outs = ['branin.f_xy']  
        
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
        
        self.add('branin',BraninComponent())
        
        loc1 = LocalDesVar()
        loc1.target = "branin.x"
        
        loc2 = LocalDesVar()
        loc2.target = "branin.y"
        
        self.global_des_vars = []
        self.local_des_vars = [loc1,loc2]
        self.coupling_vars = []
        
        #IDEA: Architectures can support the same sub-interfaces as regular drivers, then 
        #    assembly can check against the objective/constraints listed to see if it's compatible. 
        #TODO: Should drivers list the type of variables they support, so this can be checked as well? 
        #TODO: Multiple objectives? What happens if you put an achitecture in that only supports one objective
        self.objective = 'branin.f_xy'
        #TODO: What happens if you put in a type of constraint that is not supported by an achitecture
        self.constraints = []
        
        
    #TODO: Passing arguments here for component name, and any other options. I don't like giving it a component name. 
    #    What if I was optimizting more than a single component? could I specify a whole workflow then? Right now you 
    #    would always have to wrap up your analysis in an assmebly so it looked like a single box. 
    def setup_single_EI(self,comp,min_ei=.0001):
        
        #change name of component to add '_model' to it. lets me name the metamodel as the old name
        name = comp.name
        comp.name = "%s_model"%name
        
        #add in the metamodel
        meta_model = self.add(name,MetaModel()) #metamodel now replaces old component with same name
        meta_model.surrogate = {'default':KrigingSurrogate()}
        meta_model.model = comp
        
        meta_model_recorder = DBCaseRecorder(':memory:')
        meta_model.recorder = meta_model_recorder
        meta_model.force_execute = True        
        
        
        self.add("EI",ExpectedImprovement())
        self.EI.criteria = self.objective
        
        self.add("filter",ParetoFilter())
        self.filter.criteria = [self.objective]
        self.filter.case_sets = [meta_model_recorder.get_iterator(),]
        self.filter.force_execute = True
        
        #Driver Configuration
        self.add("DOE_trainer",DOEdriver())
        self.DOE_trainer.sequential = True
        self.DOE_trainer.DOEgenerator = OptLatinHypercube(num_samples=15)
        #self.DOE_trainer.DOEgenerator = FullFactorial(num_levels=5)
        
        for dvar in self.local_des_vars: 
            self.DOE_trainer.add_parameter(dvar.target)

        self.DOE_trainer.add_event("%s.train_next"%name)
        
        self.DOE_trainer.case_outputs = [self.objective]
        self.DOE_trainer.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer.db'))

        
        self.add("EI_opt",Genetic())
        self.EI_opt.opt_type = "maximize"
        self.EI_opt.population_size = 100
        self.EI_opt.generations = 10
        self.EI_opt.selection_method = "tournament"
        
        
        for dvar in self.local_des_vars: 
            self.EI_opt.add_parameter(dvar.target)
        self.EI_opt.add_objective("EI.EI")
        self.EI_opt.force_execute = True
        
        
        self.add("retrain",MyDriver())
        self.retrain.add_event("%s.train_next"%name)
        self.retrain.recorder = DBCaseRecorder(os.path.join(self._tdir,'retrain.db'))
        self.retrain.force_execute = True
        
        self.add("iter",IterateUntil())
        self.iter.max_iterations = 30
        self.iter.add_stop_condition('EI.EI <= %s'%min_ei)
        
        #Iteration Heirarchy
        self.driver.workflow.add(['DOE_trainer', 'iter'])
        
        self.DOE_trainer.workflow.add(name)
        
        self.iter.workflow = SequentialWorkflow()
        self.iter.workflow.add(['filter', 'EI_opt', 'retrain'])
        
        self.EI_opt.workflow.add([name,'EI'])
        self.retrain.workflow.add(name)
        
        #Data Connections
        self.connect("filter.pareto_set","EI.best_case")
        self.connect(self.objective,"EI.predicted_value")
        
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)
        
        
        
if __name__ == "__main__": #pragma: no cover
    import sys
    from openmdao.main.api import set_as_top
    from openmdao.lib.caserecorders.dbcaserecorder import case_db_to_dict
    
    seed = None
    backend = None
    figname = None
    for arg in sys.argv[1:]:
        if arg.startswith('--seed='):
            import random
            seed = int(arg.split('=')[1])
            random.seed(seed)
        if arg.startswith('--backend='):
            backend = arg.split('=')[1]
        if arg.startswith('--figname='):
            figname = arg.split('=')[1]
    import matplotlib
    if backend is not None:
        matplotlib.use(backend)
    elif sys.platform == 'win32':
        matplotlib.use('WxAgg')
    from matplotlib import pyplot as plt, cm 
    from matplotlib.pylab import get_cmap
    from mpl_toolkits.mplot3d import Axes3D
    from numpy import meshgrid,array, pi,arange,cos
    
    analysis = Analysis()
       
    set_as_top(analysis)
    analysis.setup_single_EI(analysis.branin,.0001)
    analysis.run()
        
    points = [(-pi,12.275,.39789),(pi,2.275,.39789),(9.42478,2.745,.39789)]
    for x,y,z in points: 
        print "x: ", x, "; y: ", y
        analysis.branin.x = x
        analysis.branin.y = y
        analysis.branin.execute()
        print "f_xy: ",analysis.branin.f_xy, " % error: ", \
              (analysis.branin.f_xy.mu - z)/z*100
    
    #Generate the Contour plot to show the function
    def branin(x,y): 
        return (y-(5.1/(4.*pi**2.))*x**2.+5.*x/pi-6.)**2.+\
               10.*(1.-1./(8.*pi))*cos(x)+10.
    
    X_range = arange(-5,10.2,.25)
    Y_range = arange(0,15.2,.25)
    
    X,Y = meshgrid(X_range,Y_range)
    Z = branin(X,Y)
    
    iterator = analysis.branin.recorder.get_iterator()
    
    plt.contour(X,Y,Z,arange(1,200,2),zorder=1)
    
    cb = plt.colorbar(shrink=.45)
    
    #plot the initial training data
    data_train = case_db_to_dict(os.path.join(analysis._tdir,'trainer.db'),
                                     ['branin.y',
                                      'branin.x',
                                      'branin.f_xy'])
    
    plt.scatter(data_train['branin.x'],
                data_train['branin.y'],s=30,c='#572E07',zorder=10)
    
    data_EI = case_db_to_dict(os.path.join(analysis._tdir,'retrain.db'),
                                     ['branin.y',
                                      'branin.x',
                                      'branin.f_xy'])
    
    count = len(data_EI['branin.x'])
    colors = arange(0,count)/float(count)

    color_map = get_cmap('spring')
    
    
    plt.scatter(data_EI['branin.x'],data_EI['branin.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=color_map)
    
    plt.axis([-5,10,0,15])
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Branin Function Contours and EI Sample Points")
    plt.text(10.9,11,"Branin\nFunction\nValue")
    
    if figname is not None:
        matplotlib.pylab.savefig(figname)
    
    plt.figure()
    Z2 = []

    for x_row,y_row in zip(X,Y): 
        row = []
        for x,y in zip(x_row,y_row): 
            analysis.branin.x = x
            analysis.branin.y = y
            analysis.branin.execute()
            row.append(analysis.branin.f_xy.mu)
        Z2.append(row)
    Z2 = array(Z2)
    plt.contour(X,Y,Z2,arange(1,200,2),zorder=1)
    cb = plt.colorbar(shrink=.45)    
    plt.axis([-5,10,0,15])
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Branin Meta Model Contours")
    plt.text(10.9,11,"Meta Model\nFunction\nValue")
    
    plt.show()

    analysis.cleanup()
            