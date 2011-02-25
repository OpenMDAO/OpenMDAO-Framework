from openmdao.main.api import Component, plugin
from openmdao.lib.datatypes.api import Float
from openmdao.main.expreval import ExprEvaluator

@plugin('openmdao.component')
class ExecComp(Component):
    """Given a list of assignment statements, this component creates
    input and output I/O traits at construction time.  All variables
    appearing on the left hand side of the assignments are outputs
    and the rest are inputs.  All variables are assumed to be of
    type Float.
    """
    
    def __init__(self, exprs=[]):
        super(ExecComp, self).__init__()
        ins = set()
        outs = set()
        allvars = set()
        self.codes = [compile(expr,'<string>','exec') for expr in exprs]
        for expr in exprs:
            expreval = ExprEvaluator(expr, scope=self, lazy_check=True)
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
                self.add_trait(var, Float(iotype=iotype))
        self.runcount = 0
        
    def execute(self):
        for expr in self.codes:
            exec expr in self.__dict__
        self.runcount += 1


