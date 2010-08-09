from enthought.traits.api import Instance, Str

from openmdao.main.api import Assembly, Component, Driver, SequentialWorkflow, Case
from openmdao.main.interfaces import ICaseIterator

from openmdao.lib.components.metamodel import MetaModel
from openmdao.lib.components.kriging_surrogate import KrigingSurrogate
from openmdao.lib.components.pareto_filter import ParetoFilter
from openmdao.lib.drivers.doedriver import DOEdriver
from openmdao.lib.drivers.single_crit_ei import SingleCritEI
from openmdao.lib.doegenerators.optlh import OptLatinHypercube
from openmdao.lib.doegenerators.full_factorial import FullFactorial
from openmdao.lib.drivers.caseiterdriver import CaseIteratorDriver
from openmdao.lib.caserecorders.dbcaserecorder import DBCaseRecorder
from openmdao.lib.caserecorders.dumpcaserecorder import DumpCaseRecorder
from openmdao.lib.caseiterators.dbcaseiter import DBCaseIterator
from openmdao.lib.api import Float, Int

from openmdao.main.uncertain_distributions import convert_norm_dist

from openmdao.examples.singleEI.branin_component import BraninComponent

class Broadcaster(Component): 
    x_in = Float(iotype="in",low=-5,high=10)
    x_out = Float(iotype="out")
    
    y_in = Float(iotype="in",low=0,high=15)
    y_out = Float(iotype="out")
    
    def execute(self): 
        self.x_out = self.x_in
        self.y_out = self.y_in
        
class Iterator(Driver):
    iterations = Int(10,iotype="in")
    
    def start_iteration(self):
        self._iterations = 0
    
    def continue_iteration(self):
        #print "iter"
        self._iterations += 1
        if (self._iterations > 1) and (analysis.EI_driver.EI <= .03): return False
        if self._iterations <= self.iterations: return True
        
        return False

    def post_iteration(self): 
        outputs = [("%s.iteration"%self.name,None,self._iterations),
                   ("branin_meta_model.x",None,self.parent.branin_meta_model.x),
                   ("branin_meta_model.y",None,self.parent.branin_meta_model.y),
                   ("branin_meta_model.f_xy",None,self.parent.branin_meta_model.f_xy)
                   ]
        c = Case(outputs = outputs)
        self.recorder.record(c)
        
        
class Analysis(Assembly): 
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        #Components
        self.add("branin_meta_model",MetaModel())
        self.branin_meta_model.surrogate = KrigingSurrogate()
        self.branin_meta_model.model = BraninComponent()
        self.branin_meta_model.recorder = DBCaseRecorder('branin_meta_model.db')
        
        
        self.add("filter",ParetoFilter())
        self.filter.criteria = ['branin_meta_model.f_xy']
        self.filter.case_set = DBCaseIterator('branin_meta_model.db')
        self.filter.force_execute = True

        #Driver Configuration
        self.add("DOE_trainer",DOEdriver())
        #self.DOE_trainer.DOEgenerator = OptLatinHypercube(5,2)
        self.DOE_trainer.DOEgenerator = FullFactorial(3,2)
        self.DOE_trainer.add_parameter("branin_meta_model.x")
        self.DOE_trainer.add_parameter("branin_meta_model.y")
        self.DOE_trainer.add_event("branin_meta_model.train_next")
        self.DOE_trainer.case_outputs = ["branin_meta_model.f_xy"]
        self.DOE_trainer.recorder = DBCaseRecorder('trainer.db')
        
        self.add("EI_driver",SingleCritEI())
        self.EI_driver.criteria = "branin_meta_model.f_xy"
        self.EI_driver.add_parameter("branin_meta_model.x")
        self.EI_driver.add_parameter("branin_meta_model.y")
        self.EI_driver.criterion = "branin_meta_model.f_xy"
        self.EI_driver.next_case_events = ['branin_meta_model.train_next']
        self.rand_seed = 10
        #self.EI_driver.force_execute = True
        
        self.add("retrain",CaseIteratorDriver())
        self.retrain.recorder = DBCaseRecorder('retrain.db')
        #self.retrain.recorder = DBCaseRecorder()
        #self.retrain.force_execute = True
        
        self.add("iter",Iterator())
        self.iter.iterations = 2
        self.iter.recorder = DumpCaseRecorder(open('iter.out','w'))
        
        #Iteration Heirarchy
        self.driver.workflow.add([self.DOE_trainer,self.iter])
        
        self.DOE_trainer.workflow.add(self.branin_meta_model)
        self.iter.workflow.add([self.filter,self.EI_driver,self.retrain])
        
        self.EI_driver.workflow.add(self.branin_meta_model)
        self.retrain.workflow.add(self.branin_meta_model)
        
        #Data Connections
        self.connect("filter.pareto_set","EI_driver.best_case")
        self.connect("EI_driver.next_case","retrain.iterator")
        
        
if __name__ == "__main__":
    from openmdao.main.api import set_as_top #, dump_iteration_tree
    from openmdao.util.plot import case_db_to_dict
    from matplotlib import pyplot as py, cm 
    from mpl_toolkits.mplot3d import Axes3D
    from numpy import meshgrid,array, pi
    
    analysis = Analysis()
    set_as_top(analysis)
    analysis.run()
    
    
    points = [(-pi,12.275,.39789),(pi,2.275,.39789),(9.42478,2.745,.39789)]
    for x,y,z in points: 
        print "x: ", x, "; y: ", y
        analysis.branin_meta_model.x = x
        analysis.branin_meta_model.y = y
        analysis.branin_meta_model.execute()
        print "f_xy: ",analysis.branin_meta_model.f_xy, " % error: ", (analysis.branin_meta_model.f_xy.mu - z)/z*100
    #dump_iteration_tree(analysis)

    data_train = case_db_to_dict('trainer.db',
                                 ['broadcaster.y_in','broadcaster.x_in','branin_meta_model.f_xy'])
    
    #convert the database data to python objects
    data_train['branin_meta_model.f_xy'] = [convert_norm_dist(x).mu for x in data_train['branin_meta_model.f_xy']]
    
   