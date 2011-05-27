import os
from tempfile import mkdtemp
import os.path
import shutil

from enthought.traits.api import HasTraits

from openmdao.main.api import Assembly, Component, Driver, \
     SequentialWorkflow, Case, ExprEvaluator, implements

from openmdao.util.decorators import add_delegate
from openmdao.main.problem_formulation import HasCouplingVars, IArchitecture, \
     ArchitectureAssembly
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasobjective import HasObjective

from openmdao.lib.components.api import MetaModel, ExpectedImprovement, ParetoFilter
from openmdao.lib.surrogatemodels.api import KrigingSurrogate
from openmdao.lib.drivers.api import DOEdriver, Genetic, CaseIteratorDriver, IterateUntil

from openmdao.lib.doegenerators.api import OptLatinHypercube
from openmdao.lib.casehandlers.api import DBCaseRecorder, DBCaseIterator
from openmdao.lib.datatypes.api import Str, Array, Float, Int

from openmdao.examples.expected_improvement.branin_component import BraninComponent

from openmdao.util.decorators import add_delegate
        
    
class MyDriver(Driver): 
    def __init__(self,doc=None):
        super(MyDriver,self).__init__(doc)
        
        self.ins = ['branin.x','branin.y']
        self.outs = ['branin.f_xy']  
        
    def execute(self):
        self.set_events()
        self.run_iteration()
        
        inputs = [(name,self.parent.get(name)) for name in self.ins]
        outputs = [(name,self.parent.get(name)) for name in self.outs]
        
        case = Case(inputs = inputs,
                    outputs = outputs)
        self.recorder.record(case)
        
 
#Only supports HasObjective,HasParameters, real/contiunous variables        
class EGO(HasTraits): 
    implements(IArchitecture)
    
    min_ei = Float(default=0.001, iotype="in", desc="EI to use for stopping condition of optimization")
    
    def configure(self):    
        self._tdir = mkdtemp()        
        
        self.comp_name = None
        #check to make sure no more than one component is being referenced
        #TODO: possibly wrap multiple components up into an enclosing assembly automatigally? 
        for target,param in self.parent.get_parameters().iteritems():
            comp = param.get_referenced_compnames()
            if len(comp) > 1:
                self.parent.raise_exception('The EGO architecture can only be used on one'
                                            'component at a time, but parameters from %s '
                                            'were added to the problem formulation.'%comp,ValueError)
            elif self.comp_name is None: 
                self.comp_name = comp.pop()
            else:
                comp = comp.pop()                
                if comp != self.comp_name: 
                    self.parent.raise_exception('The EGO architecture can only be used on one'
                                            'component at a time, but parameters from %s '
                                            'were added to the problem formulation.'%([comp[0],self.comp_name],),ValueError)
                
                
        #change name of component to add '_model' to it. 
        #     lets me name the metamodel as the old name
        self.comp= getattr(self.parent,self.comp_name)
        self.comp.name = "%s_model"%self.comp_name
        
        #add in the metamodel
        meta_model = self.parent.add(self.comp_name,MetaModel()) #metamodel now replaces old component with same name
        meta_model.surrogate = {'default':KrigingSurrogate()}
        meta_model.model = self.comp
        
        meta_model_recorder = DBCaseRecorder(':memory:')
        meta_model.recorder = meta_model_recorder
        meta_model.force_execute = True        
        
        EI = self.parent.add("EI",ExpectedImprovement())
        self.objective = self.parent.list_objective()        
        EI.criteria = self.objective
        
        pfilter = self.parent.add("filter",ParetoFilter())
        pfilter.criteria = [self.objective]
        pfilter.case_sets = [meta_model_recorder.get_iterator(),]
        pfilter.force_execute = True
        
        #Driver Configuration
        DOE_trainer = self.parent.add("DOE_trainer",DOEdriver())
        DOE_trainer.sequential = True
        DOE_trainer.DOEgenerator = OptLatinHypercube(num_samples=30)
        
        for target in self.parent.get_parameters(): 
            DOE_trainer.add_parameter(target)

        DOE_trainer.add_event("%s.train_next"%self.comp_name)
        
        DOE_trainer.case_outputs = [self.objective]
        DOE_trainer.recorder = DBCaseRecorder(os.path.join(self._tdir,'trainer.db'))
        
        EI_opt = self.parent.add("EI_opt",Genetic())
        EI_opt.opt_type = "maximize"
        EI_opt.population_size = 100
        EI_opt.generations = 10
        EI_opt.selection_method = "tournament"
        
        
        for target in self.parent.get_parameters(): 
            EI_opt.add_parameter(target)
        EI_opt.add_objective("EI.EI")
        EI_opt.force_execute = True
        
        
        retrain = self.parent.add("retrain",MyDriver())
        retrain.add_event("%s.train_next"%self.comp_name)
        retrain.recorder = DBCaseRecorder(os.path.join(self._tdir,'retrain.db'))
        retrain.force_execute = True
        
        iter = self.parent.add("iter",IterateUntil())
        iter.max_iterations = 30
        iter.add_stop_condition('EI.EI <= %s'%self.min_ei)
        
        #Iteration Heirarchy
        self.parent.driver.workflow.add(['DOE_trainer', 'iter'])
        
        DOE_trainer.workflow.add(self.comp_name)
        
        iter.workflow = SequentialWorkflow()
        iter.workflow.add(['filter', 'EI_opt', 'retrain'])
        
        EI_opt.workflow.add([self.comp_name,'EI'])
        retrain.workflow.add(self.comp_name)
        
        #Data Connections
        self.parent.connect("filter.pareto_set","EI.best_case")
        self.parent.connect(self.objective,"EI.predicted_value")
        
    def cleanup(self):
        shutil.rmtree(self._tdir, ignore_errors=True)
        
class Analysis(ArchitectureAssembly): 
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        
        
        self.add('branin',BraninComponent())
        
        #Problem Formulation 
        
        #Local Des Vars
        self.add_parameter('branin.x')        
        self.add_parameter('branin.y')
        
        #No Global Des Vars or Coupling Vars
        
        #Objective (single only)    
        self.add_objective('branin.f_xy')
        #No constraints for this problem
        
        
        
        
if __name__ == "__main__":
    import sys
    from openmdao.main.api import set_as_top
    from openmdao.lib.casehandlers.api import case_db_to_dict
    
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
    
    analysis = set_as_top(Analysis())
    analysis.architecture = EGO()
    analysis.configure()
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
    data_train = case_db_to_dict(os.path.join(analysis.architecture._tdir,'trainer.db'),
                                     ['branin.y',
                                      'branin.x',
                                      'branin.f_xy'])
    
    plt.scatter(data_train['branin.x'],
                data_train['branin.y'],s=30,c='#572E07',zorder=10)
    
    data_EI = case_db_to_dict(os.path.join(analysis.architecture._tdir,'retrain.db'),
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

    analysis.EGO.cleanup()
            