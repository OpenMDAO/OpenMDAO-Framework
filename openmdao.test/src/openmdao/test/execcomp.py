""" ExecComp is a simple component that lets you easily define mathematical
expressions."""

import re

from openmdao.main.api import Component, ComponentWithDerivatives
from openmdao.main.datatypes.api import Float
from openmdao.main.expreval import ExprEvaluator


class ExecComp(Component):
    """Given a list of assignment statements, this component creates
    input and output I/O traits at construction time.  All variables
    appearing on the left hand side of the assignments are outputs
    and the rest are inputs.  All variables are assumed to be of
    type Float.
    """
    
    def __init__(self, exprs=()):
        super(ExecComp, self).__init__()
        ins = set()
        outs = set()
        allvars = set()
        self.codes = [compile(expr,'<string>','exec') for expr in exprs]
        for expr in exprs:
            expreval = ExprEvaluator(expr, scope=self)
            exvars = expreval.get_referenced_varpaths()
            lhs,rhs = expr.split('=')
            lhs = lhs.strip()
            lhs = lhs.split(',')
            outs.update(lhs)
            allvars.update(exvars)
        ins = allvars - outs
        for var in allvars:
            if '.' not in var:  # if a varname has dots, it's outside of our scope,
                                # so don't add a trait for it
                if var in outs:
                    iotype = 'out'
                else:
                    iotype = 'in'
                self.add(var, Float(iotype=iotype))
        
    def execute(self):
        ''' ExecComp execute function '''
        for expr in self.codes:
            exec expr in self.__dict__


class ExecCompWithDerivatives(ComponentWithDerivatives):
    """ Works as ExecComp, except derivatives can also be defined.
    
    Given a list of assignment statements, this component creates
    input and output I/O traits at construction time.  All variables
    appearing on the left hand side of the assignments are outputs
    and the rest are inputs.  All variables are assumed to be of
    type Float.
    """
    
    def __init__(self, exprs=(), derivatives=()):
        super(ExecCompWithDerivatives, self).__init__()
        
        ins = set()
        outs = set()
        allvars = set()
        self.codes = [compile(expr,'<string>','exec') for expr in exprs]
        
        for expr in exprs:
            expreval = ExprEvaluator(expr, scope=self)
            exvars = expreval.get_referenced_varpaths()
            lhs,rhs = expr.split('=')
            lhs = lhs.strip()
            lhs = lhs.split(',')
            outs.update(lhs)
            allvars.update(exvars)
        ins = allvars - outs
        
        for var in allvars:
            if '.' not in var:  # if a varname has dots, it's outside of our scope,
                                # so don't add a trait for it
                if var in outs:
                    iotype = 'out'
                else:
                    iotype = 'in'
                self.add(var, Float(iotype=iotype))
    
        self.derivative_codes = \
            [compile(expr,'<string>','exec') for expr in derivatives]
        
        self.derivative_names = []
        regex = re.compile('d(.*)_d(.*)')
        for expr in derivatives:
            expreval = ExprEvaluator(expr, scope=self)
            exvars = expreval.get_referenced_varpaths()
            
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
            self.derivatives.declare_first_derivative(num, wrt)
            
            self.derivative_names.append( (lhs, num, wrt) )
    
    def execute(self):
        ''' ExecCompWithDerivatives execute function '''
        
        for expr in self.codes:
            exec expr in self.__dict__    
            
            
    def calculate_first_derivatives(self):
        ''' Calculate the first derivatives '''
        
        for expr in self.derivative_codes:
            exec expr in self.__dict__
            
        for item in self.derivative_names:
            self.derivatives.set_first_derivative(item[1], item[2], 
                                                  getattr(self, item[0]))