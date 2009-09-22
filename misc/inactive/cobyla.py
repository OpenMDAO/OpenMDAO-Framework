
#public symbols
__all__ = ["COBYLA"]




from scipy.optimize.cobyla import fmin_cobyla

from openmdao.main.api import Driver, StringRef, StringRefArray

# if our globals dict doesn't contain __builtins__, python will 
# copy current global dict into it
#_emptyglobals = { '__builtins__':None }


class COBYLA(Driver):
    """ Constrained Optimization BY Linear Approximation. """
    
    rhobeg = Float(iostatus='in',
                   desc='reasonable initial changes to the variables')
    rhoend = Float(iostatus='in',
                   desc='final accuracy (not precisely guaranteed)')
    maxiters = Int(iostatus='in', desc='maximum number of function iterations')
    iprint = Int(iostatus='in',
                 desc='print frequency: 0 (no output),1,2,3')
    min_expr = StringRef(iostatus='in', desc='expression to minimize')
    constraint_exprs = StringRefArray(iostatus='in',
                        desc='constraint expressions (must be >=0)')
    design_vars = StringRefArray(iostatus='in',
                                 desc='list of design variable names')
        
    def __init__(self, desc=None):
        super(COBYLA, self).__init__(desc)
        self.design_vars = [] #names of design variables
        self.__funct = None
        self._code = None
        self.min_expr = None
        #self.func_args = (self,)
        self._consfuncts = [] # constraint functs. All must be >=0
        self.constraint_exprs = [] # constraint expression strings
        #self.cons_args = None
        self.rhobeg = 0.1
        self.rhoend = 0.0001
        self.maxiters = 1000
        self.iter_count = 0
        self.xmin = []
        self.iprint = 0
                
    def _get_funct(self):
        return self.__funct
    
    def _set_funct(self, fstring):
        text = translate_expr(fstring, self)
        self._code = compile(text, '<string>','eval')
#        self.__funct = lambda: eval(code, _emptyglobals, self.parent.__dict__)
        
    _funct = property(_get_funct, _set_funct)
    
    def _driven_func(self, dvars):
        # first, set the design variables in the model
        for name, val in zip(self.design_vars, dvars):
            eval(translate_expr(name+'='+str(val), self))
        self.parent.workflow.run()
        
        if self._stop is True:
            raise StopIteration()
        
        return eval(self._code)

        
    def execute(self):
        """ Run the model by invoking run() on the workflow. """
        self.iter_count = 0
        # get initial values of design vars
        dvinit = [eval(translate_expr(a, self)) for a in self.design_vars]
        
        self.xmin = fmin_cobyla(self._driven_func, dvinit, self._consfuncts,
                                consargs=(),
                                rhobeg=self.rhobeg, rhoend=self.rhoend,
                                iprint=self.iprint, maxfun=self.maxiters)


    def add_constraint(self, constraint):
        """Add a function that evaluates to >= 0 if the 
        constraint is not violated.
        """
        self._consfuncts.append(constraint)
        
