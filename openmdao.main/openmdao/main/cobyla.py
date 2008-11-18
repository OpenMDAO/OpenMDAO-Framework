"""
A driver is used to run a workflow in an assembly.
"""

#public symbols
__all__ = ["COBYLA"]

__version__ = "0.1"


from zope.interface import implements
from scipy.optimize.cobyla import fmin_cobyla

from openmdao.main.driver import Driver
from openmdao.main.component import Component, STATE_WAITING, STATE_RUNNING, RUN_UNKNOWN
from openmdao.main.float import Float
from openmdao.main.string import String
from openmdao.main.stringlist import StringList
from openmdao.main.int import Int
from openmdao.main.variable import INPUT,OUTPUT

# if our globals dict doesn't contain __builtins__, python will copy current global dict into it
__emptyglobals = { '__builtins__':None }

class COBYLA(Driver):
    """ Constrained Optimization By Linear Approximation. """
    
    def __init__(self, name, parent=None, desc=None):
        Driver.__init__(self, name, parent, desc)
        self.design_vars = [] #names of design variables
        self.__funct = None
        self.min_expr = None
        #self.func_args = (self,)
        self._consfuncts = [] # constraint functs. All must be >=0
        self.constraint_exprs = [] # constraint expression strings
        #self.cons_args = None
        self.rhobeg = 1.0
        self.rhoend = 0.0001
        self.maxiters = 1000
        self.status = RUN_UNKNOWN
        
        Float('rhobeg',INPUT,parent=self,desc='reasonable initial changes to the variables')
        Float('rhoend',INPUT,parent=self,desc='final accuracy in the optimization (not precisely guaranteed)')
        Int('maxiters',INPUT,parent=self,desc='maximum number of function iterations')
        String('min_expr',INPUT,parent=self,desc='expression to minimize')
        StringList('constraint_exprs',INPUT,parent=self,desc='constraint expressions (must be >=0)')
        
    def _get_funct(self):
        return self.__funct
    
    def _set_funct(self, fstring):
        code = compile(fstring,'<string>','eval')
        self.__funct = lambda: eval(code, __emptyglobals, self.parent.__dict__)
        
    _funct = property(_get_funct,_set_funct)

    def _driven_func(self, x):
        # first, set the design variables in the model
        for name,val in zip(self.design_vars,x):
            self._parent.set(name, val)
        self.state = STATE_WAITING
        self.status = self._parent.workflow.run()
        
        if self._stop is True:
            raise StopIteration()
        
        return self._funct()
        
    def execute(self):
        """ Run the assembly by invoking run() on the workflow. """
        self.iter_count = 0
        # get initial values of design vars
        x0 = [self._parent.get(a) for a in self.design_vars]
        
        # set up constraint function list
        self._consfuncts = []
        for expr in self.constraint_exprs:
            self.add_constraint(expr)
        
        self.xmin = fmin_cobyla(self._driven_func, x0, self._consfuncts,
                                rhobeg=self.rhobeg, rhoend=self.rhoend,
                                iprint=0, maxfun=self.maxiters)
        
        return self.status


    def add_constraint(self, constraint):
        """Add a string expression that must evaluate to >= 0."""
        code = compile(constraint,'<string>','eval')
        self.consfuncts.append(lambda: eval(code, __emptyglobals, self.parent.__dict__) )
        
