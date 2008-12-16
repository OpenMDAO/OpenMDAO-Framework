
import numpy

from openmdao.main.component import Component
from openmdao.main.assembly import Assembly
from openmdao.lib.conmindriver import CONMINdriver
from openmdao.main.arrayvar import ArrayVariable
from openmdao.main.variable import INPUT,OUTPUT
from openmdao.main.float import Float

# we need to add the ImportFactory to the factorymanager to be able to find plugin modules
import openmdao.main.factorymanager as factorymanager
from openmdao.main.importfactory import ImportFactory
factorymanager.register_factory(ImportFactory())

# we need to find a better way to bootstrap the var to type map
import openmdao.main.containervar

class OptRosenSuzukiComponent(Component):
    """ From the CONMIN User's Manual:
    EXAMPLE 1 - CONSTRAINED ROSEN-SUZUKI FUNCTION. NO GRADIENT INFORMATION.
    
    Consider the minimization problem discussed in SECTION I:
    
         MINIMIZE OBJ = X(1)**2 - 5*X(1) + X(2)**2 - 5*X(2) +
                        2*X(3)**2 - 21*X(3) + X(4)**2 + 7*X(4) + 50
    
         Subject to:
    
              G(1) = X(1)**2 + X(1) + X(2)**2 - X(2) +
                     X(3)**2 + X(3) + X(4)**2 - X(4) - 8   .LE.0
    
              G(2) = X(1)**2 - X(1) + 2*X(2)**2 + X(3)**2 +
                     2*X(4)**2 - X(4) - 10                  .LE.0
    
              G(3) = 2*X(1)**2 + 2*X(1) + X(2)**2 - X(2) +
                     X(3)**2 - X(4) - 5                     .LE.0
    This problem is solved beginning with an initial X-vector of
         X = (1.0, 1.0, 1.0, 1.0)
    The optimum design is known to be
         OBJ = 6.000
    and the corresponding X-vector is
         X = (0.0, 1.0, 2.0, -1.0)
    """
    
    def __init__(self, name, parent=None, desc=None):
        Component.__init__(self, name, parent, desc)
        self.x = numpy.array([1.,1.,1.,1.],dtype=float)
        self.result = 0.
        ArrayVariable('x',self,iostatus=INPUT,entry_type=float)
        Float('result',self,iostatus=OUTPUT)

    def execute(self):
        self.result = (self.x[0]**2 - 5.*self.x[0] + self.x[1]**2 - 5.*self.x[1] +
                       2.*self.x[2]**2 - 21.*self.x[2] + self.x[3]**2 + 7.*self.x[3] + 50)
        
    def opt_objective(self):
        return 6.
    
    def opt_design_vars(self):
        return [0., 1., 2., -1.]
    
        

top = Assembly('top',None)
comp = OptRosenSuzukiComponent('comp', top)
top.add_child(comp)
top.workflow.add_node(comp)
top.add_child(CONMINdriver('driver'))
top.driver.iprint = 0
top.driver.objective = 'comp.result'
top.driver.maxiters = 300
top.driver.design_vars = ['comp.x[0]','comp.x[1]','comp.x[2]','comp.x[3]']
top.driver.constraints = ['comp.x[0]**2 + comp.x[0] + comp.x[1]**2 - comp.x[1] + comp.x[2]**2 + comp.x[2] + comp.x[3]**2 - comp.x[3] - 8',
                          'comp.x[0]**2 - comp.x[0] + 2*comp.x[1]**2 + comp.x[2]**2 + 2*comp.x[3]**2 - comp.x[3] - 10',
                          '2*comp.x[0]**2 + 2*comp.x[0] + comp.x[1]**2 - comp.x[1] + comp.x[2]**2 - comp.x[3] - 5']
#top.driver.upper_bounds = [1.,1.,1.,1.,1.]
#top.driver.lower_bounds = [0.,0.,0.,0.,0.]

top.run()

print 'final objective value: ',top.driver.objective_val
dvdiff = top.comp.x - top.comp.opt_design_vars()
print 'dvdiff = ',dvdiff
print 'design values:'
for val in top.driver.design_vals[0:-2]:
    print val
    



    
    