""" ExecComp is a simple component that lets you easily define mathematical
expressions."""

import re, time

from openmdao.main.api import Component
from openmdao.main.datatypes.api import Float
from openmdao.main.expreval import ExprEvaluator, _expr_dict

from numpy import zeros

class ExecComp(Component):
    """Given a list of assignment statements, this component creates
    input and output I/O traits at construction time.  All variables
    appearing on the left-hand side of the assignments are outputs,
    and the rest are inputs.  All variables are assumed to be of
    type Float.
    
    exprs: list of strings
    
    sleep: float
        Time (in seconds) to sleep during execute.
    """
    
    def __init__(self, exprs=(), sleep=0, trace=False):
        super(ExecComp, self).__init__()
        outs = set()
        allvars = set()
        self.exprs = exprs
        self.codes = [compile(expr,'<string>','exec') for expr in exprs]
        self.sleep = sleep
        self.trace = trace
        for expr in exprs:
            lhs,rhs = expr.split('=')
            lhs = lhs.strip()
            lhs = lhs.split(',')
            outs.update(lhs)
            expreval = ExprEvaluator(expr, scope=self)
            allvars.update(expreval.get_referenced_varpaths(copy=False))

        for var in allvars:
            if '.' not in var:  # if a varname has dots, it's outside of our scope,
                                # so don't add a trait for it
                if var in outs:
                    iotype = 'out'
                else:
                    iotype = 'in'
                self.add(var, Float(0.0, iotype=iotype))
        
    def __getstate__(self):
        """Return dict representing this container's state."""
        
        state = super(ExecComp, self).__getstate__()
        
        # Compiled stuff doesn't pickle
        state['codes'] = None
        
        return state
        
    def __setstate__(self, state):
        """Restore this component's state."""
        
        super(ExecComp, self).__setstate__(state)
        self.codes = [compile(expr,'<string>','exec') for expr in self.exprs]
        
    def execute(self):
        ''' ExecComp execute function '''
        global _expr_dict
        if self.trace:
            for var in self.list_inputs(connected=True):
                print "%s.%s = %s" % (self.name,var,getattr(self,var))
            for var in self.list_outputs(connected=True):
                print "%s.%s = %s" % (self.name,var,getattr(self,var))
        for expr in self.codes:
            exec(expr, _expr_dict, self.__dict__ )
            
        if self.sleep:
            time.sleep(self.sleep)

        if self.trace:
            for var in self.list_inputs(connected=True):
                print "%s.%s = %s" % (self.name,var,getattr(self,var))
            for var in self.list_outputs(connected=True):
                print "%s.%s = %s" % (self.name,var,getattr(self,var))


class ExecCompWithDerivatives(Component):
    """ Works as ExecComp, except derivatives can also be defined.
    
    Given a list of assignment statements, this component creates
    input and output I/O traits at construction time.  All variables
    appearing on the left-hand side of the assignments are outputs,
    and the rest are inputs.  All variables are assumed to be of
    type Float.
    """
    
    def __init__(self, exprs=(), derivatives=(), sleep=0, dsleep=0):
        super(ExecCompWithDerivatives, self).__init__()
        
        outs = set()
        allvars = set()
        self.exprs = exprs
        self.codes = [compile(expr,'<string>','exec') for expr in exprs]
        self.sleep = sleep
        self.dsleep = dsleep
        
        for expr in exprs:
            lhs,rhs = expr.split('=')
            lhs = lhs.strip()
            lhs = lhs.split(',')
            outs.update(lhs)
            expreval = ExprEvaluator(expr, scope=self)
            allvars.update(expreval.get_referenced_varpaths(copy=False))
        
        for var in allvars:
            if '.' not in var:  # if a varname has dots, it's outside of our scope,
                                # so don't add a trait for it
                if var in outs:
                    iotype = 'out'
                else:
                    iotype = 'in'
                self.add(var, Float(0.0, iotype=iotype))
    
        self.deriv_exprs = derivatives
        self.derivative_codes = \
            [compile(expr,'<string>','exec') for expr in derivatives]
        
        self.derivative_names = []
        regex = re.compile('d(.*)_d(.*)')
        for expr in derivatives:
            expreval = ExprEvaluator(expr, scope=self)
            exvars = expreval.get_referenced_varpaths(copy=False)
            
            lhs, _ = expr.split('=')
            lhs = lhs.strip()
            allvars.add(lhs)
            
            # Check for undefined vars the cool way with sets
            if len(exvars-allvars) > 0:
                self.raise_exception('derivative references a variable '
                                     'that is not defined in exprs',
                                     ValueError)
                    
            names = regex.findall(lhs)
            num = names[0][0]
            wrt = names[0][1]
            
            self.derivative_names.append( (lhs, num, wrt) )
    
    def __getstate__(self):
        """Return dict representing this container's state."""
        
        state = super(ExecCompWithDerivatives, self).__getstate__()
        
        # Compiled stuff doesn't pickle
        state['codes'] = None
        state['derivative_codes'] = None
        
        return state
        
    def __setstate__(self, state):
        """Restore this component's state."""
        
        super(ExecCompWithDerivatives, self).__setstate__(state)
        self.codes = [compile(expr,'<string>','exec') for expr in self.exprs]
        self.derivative_codes = \
            [compile(expr,'<string>','exec') for expr in self.deriv_exprs]
        
    def execute(self):
        ''' ExecCompWithDerivatives execute function '''
        
        global _expr_dict
        
        for expr in self.codes:
            exec(expr, _expr_dict, self.__dict__ )
                
        if self.sleep:
            time.sleep(self.sleep)
    
    def provideJ(self):
        '''Calculate the Jacobian using our derivative expressions.'''
        
        global _expr_dict
        
        for expr in self.derivative_codes:
            exec(expr, _expr_dict, self.__dict__ )

        inputs, outputs = self.list_deriv_vars()
        self.J = zeros((len(outputs), len(inputs)))
        
        for item in sorted(self.derivative_names):
            
            inp = inputs.index(item[2])
            outp = outputs.index(item[1])
            
            self.J[outp, inp] = getattr(self, item[0])
            
        if self.dsleep:
            time.sleep(self.dsleep)

        return self.J
            
    def list_deriv_vars(self):
        """Return the full Jacobian"""
        
        input_keys = [x[0] for x in self.items(framework_var=None, 
                                               iotype='in')]
        output_keys = [x[0] for x in self.items(framework_var=None, 
                                                iotype='out')]
        return input_keys, output_keys
            
