from __future__ import division
import os

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

from openmdao.util.decorators import add_delegate
from openmdao.main.hasstopcond import HasStopConditions
        
@add_delegate(HasStopConditions)
class Iterator(Driver):
    iterations = Int(10,iotype="in")
    
    def start_iteration(self):
        self._iterations = 0
    
    def continue_iteration(self):
        self._iterations += 1
        #print 'Iter'
        if (self._iterations > 1) and self.should_stop():
            return False
        if self._iterations <= self.iterations: 
            return True
        
        return False
        
        
class Analysis(Assembly): 
    def __init__(self,*args,**kwargs):
        super(Analysis,self).__init__(self,*args,**kwargs)
        
        #Components
        self.add("branin_meta_model",MetaModel())
        self.branin_meta_model.surrogate = KrigingSurrogate()
        self.branin_meta_model.model = BraninComponent()
        self.branin_meta_model.recorder = DBCaseRecorder(':memory:')
        
        self.add("filter",ParetoFilter())
        self.filter.criteria = ['branin_meta_model.f_xy']
        self.filter.case_set = self.branin_meta_model.recorder.get_iterator()

        #Driver Configuration
        self.add("DOE_trainer",DOEdriver())
        self.DOE_trainer.DOEgenerator = OptLatinHypercube(21, 2)
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
        
        self.add("retrain",CaseIteratorDriver())
        self.retrain.add_event("branin_meta_model.train_next")
        self.retrain.recorder = DBCaseRecorder('retrain.db')
        
        self.add("iter",Iterator())
        self.iter.iterations = 30
        self.iter.add_stop_condition('EI_driver.EI <= .0001')
        
        #Iteration Heirarchy
        self.driver.workflow.add([self.DOE_trainer,self.iter])
        
        self.DOE_trainer.workflow.add(self.branin_meta_model)
        self.iter.workflow.add([self.filter, self.EI_driver, self.retrain])
        
        self.EI_driver.workflow.add(self.branin_meta_model)
        self.retrain.workflow.add(self.branin_meta_model)
        
        #Data Connections
        self.connect("filter.pareto_set","EI_driver.best_case")
        self.connect("EI_driver.next_case","retrain.iterator")
        
    def cleanup(self):
        try:
            os.remove('retrain.db')
            os.remove('trainer.db')
        except:
            pass
        
if __name__ == "__main__":
    from openmdao.main.api import set_as_top
    from openmdao.util.plot import case_db_to_dict
    from matplotlib import pyplot as plt, cm 
    from matplotlib.pylab import get_cmap
    from mpl_toolkits.mplot3d import Axes3D
    from numpy import meshgrid,array, pi,arange,cos
    
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

    #Generate the Contour plot to show the function
    def branin(x,y): 
        return (y-(5.1/(4.*pi**2.))*x**2.+5.*x/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x)+10.
    
    X_range = arange(-5,10.2,.25)
    Y_range = arange(0,15.2,.25)
    
    X,Y = meshgrid(X_range,Y_range)
    Z = branin(X,Y)
    
    
    plt.contour(X,Y,Z,arange(1,200,2),zorder=1)
    
    cb = plt.colorbar(shrink=.45)
    
    #plot the initial training data
    data_train = case_db_to_dict('trainer.db',
                                     ['branin_meta_model.y','branin_meta_model.x','branin_meta_model.f_xy'])
    
    plt.scatter(data_train['branin_meta_model.x'],data_train['branin_meta_model.y'],s=30,c='r',zorder=10)
    
    data_EI = case_db_to_dict('retrain.db',
                                     ['branin_meta_model.y','branin_meta_model.x','branin_meta_model.f_xy'])
    
    count = len(data_EI['branin_meta_model.x'])
    colors = arange(0,count)/count
    winter = get_cmap('winter')
    plt.scatter(data_EI['branin_meta_model.x'],data_EI['branin_meta_model.y'],
                s=30,
                c=colors,
                zorder=11,
                cmap=winter)
    
    plt.axis([-5,10,0,15])
    plt.xlabel("x")
    plt.ylabel("y")
    plt.title("Branin Function Contours and EI Sample Points")
    plt.text(10.9,11,"Branin\nFunction\nValue")
    plt.show()

    analysis.cleanup()
    