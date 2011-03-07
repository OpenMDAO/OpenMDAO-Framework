""" Differentiates a driver's workflow using the finite difference method. a
variety of difference types are available for both first and second order."""


# pylint: disable-msg=E0611,F0401
from numpy import zeros, copy

from enthought.traits.api import HasTraits
from openmdao.lib.datatypes.api import implements, Enum, Float
from openmdao.main.interfaces import IDifferentiator


def diff_1st_central(fp, fm, eps):
    """Evaluates a first order central difference."""
    
    return (fp-fm)/(2.0*eps)

def diff_1st_fwrdbwrd(fp, fm, eps):
    """Evaluates a first order forward or backward difference."""
    
    return (fp-fm)/eps


class FiniteDifference(HasTraits):
    """ Differentiates a driver's workflow using the finite difference method.
    a variety of difference types are available for both first and second
    order."""

    implements(IDifferentiator)
    
    # pylint: disable-msg=E1101
    form = Enum("Central", ["Central", "Forward", "Backward"], iotype='in', \
                desc="Finite difference form (central, forward, backward)")
    
    default_stepsize = Float(1.0e-6, iotype='in', desc='Default finite ' + \
                             'difference step size.')
    
    def __init__(self, parent):
        
        self._parent = parent
        
        self.gradient_obj = zeros(0, 'd')
        self.gradient_ineq_const = zeros(0, 'd')
        self.gradient_eq_const = zeros(0, 'd')
        
        self.n_eqconst = 0
        self.n_ineqconst = 0
        self.n_param = 0
    
    def calc_gradient(self):
        """Returns the gradient vectors for this Driver's workflow"""

        self.n_param = len(self._parent._hasparameters._parameters)
        #self.n_objective = len(self._parent._hasobjective._objective)
        if hasattr(self._parent, '_hasineqconstraints'):
            self.n_ineqconst = len(self._parent._hasineqconstraints._constraints)
        if hasattr(self._parent, '_haseqconstraints'):
            self.n_eqconst = len(self._parent._haseqconstraints._constraints)
        
        # Dimension the matrices that will store the answers
        self.gradient_obj = zeros(self.n_param, 'd')
        self.gradient_ineq_const = zeros([self.n_param, self.n_ineqconst], 'd')
        self.gradient_eq_const = zeros([self.n_param, self.n_eqconst], 'd')
        
        # Temp storage of all responses
        self.data_obj = zeros([2, 1], 'd')
        self.data_ineqconst = zeros([2, self.n_ineqconst], 'd')
        self.data_eqconst = zeros([2, self.n_eqconst], 'd')

        base_param = zeros(self.n_param, 'd')
        for i_param, val in enumerate(self._parent.get_parameters().values()):
            base_param[i_param] = val.expreval.evaluate()
        
        # Central Difference
        if self.form == 'Central':
            
            for i_param in range(0, self.n_param):
                
                eps = self.default_stepsize
                
                # Positive increment
                base_param[i_param] += eps
                self._run_point(base_param, 0)
                
                # Negative increment
                base_param[i_param] -= 2.0*eps
                self._run_point(base_param, 1)
                
                # Calculate gradients
                self.gradient_obj[i_param] = \
                     diff_1st_central(self.data_obj[0], self.data_obj[1], eps)
                     
                for j in range(0, self.n_ineqconst):
                    self.gradient_ineq_const[i_param, j] = \
                        diff_1st_central(self.data_ineqconst[0, j], \
                                         self.data_ineqconst[1, j], eps)
                    
                for j in range(0, self.n_eqconst):
                    self.gradient_eq_const[i_param, j] = \
                        diff_1st_central(self.data_eqconst[0, j], \
                                         self.data_eqconst[1, j], eps)
                    
                base_param[i_param] += eps
                
        elif self.form == 'Forward':
            
            for i_param in range(0, self.n_param):
                
                eps = self.default_stepsize
                
                # No increment
                self._run_point(base_param, 0, run=False)
                
                # Positive increment
                base_param[i_param] += eps
                self._run_point(base_param, 1)
                
                # Calculate gradients
                self.gradient_obj[i_param] = \
                     diff_1st_fwrdbwrd(self.data_obj[1], 
                                         self.data_obj[0], eps)
                     
                for j in range(0, self.n_ineqconst):
                    self.gradient_ineq_const[i_param, j] = \
                        diff_1st_fwrdbwrd(self.data_ineqconst[1, j], \
                                         self.data_ineqconst[0, j], eps)
                    
                for j in range(0, self.n_eqconst):
                    self.gradient_eq_const[i_param, j] = \
                        diff_1st_fwrdbwrd(self.data_eqconst[1, j], \
                                         self.data_eqconst[0, j], eps)
                    
                base_param[i_param] += -eps
                
        elif self.form == 'Backward':
            
            for i_param in range(0, self.n_param):
                
                eps = self.default_stepsize
                
                # No increment
                self._run_point(base_param, 0, run=False)
                
                # Positive increment
                base_param[i_param] += -eps
                self._run_point(base_param, 1)
                
                # Calculate gradients
                self.gradient_obj[i_param] = \
                     diff_1st_fwrdbwrd(self.data_obj[0], 
                                         self.data_obj[1], eps)
                     
                for j in range(0, self.n_ineqconst):
                    self.gradient_ineq_const[i_param, j] = \
                        diff_1st_fwrdbwrd(self.data_ineqconst[0, j], \
                                         self.data_ineqconst[1, j], eps)
                    
                for j in range(0, self.n_eqconst):
                    self.gradient_eq_const[i_param, j] = \
                        diff_1st_fwrdbwrd(self.data_eqconst[0, j], \
                                         self.data_eqconst[1, j], eps)
                    
                base_param[i_param] += eps
                
        else:
            raise NotImplementedError("Only Central Difference implented so far.")
            

    def calc_hessian(self):
        """Returns the Hessian matrix for this Driver's workflow"""
        
        raise NotImplementedError("Hessian FD not implemented just yet.")
    
    def _run_point(self, data_param, ii, run=True):
        """Runs the model at a single point and captures the results. Note,
        some differences require the baseline point. For these, set the run
        flag to False."""

        dvals = [float(val) for val in data_param]
        self._parent.set_parameters(dvals)
        
        # Runs the model
        if run:
            super(type(self._parent), self._parent).run_iteration()
        
        self.data_obj[ii] = self._parent.eval_objective()

        if self.n_ineqconst:
            for j, v in enumerate(self._parent.get_ineq_constraints().values()):
                val = v.evaluate()
                if '>' in val[2]:
                    self.data_ineqconst[ii, j] = val[1]-val[0]
                else:
                    self.data_ineqconst[ii, j] = val[0]-val[1]
        
        if self.n_eqconst:
            for j, v in enumerate(self._parent.get_eq_constraints().values()):
                val = v.evaluate()
                if '>' in val[2]:
                    self.data_eqconst[ii, j] = val[1]-val[0]
                else:
                    self.data_eqconst[ii, j] = val[0]-val[1]
                    
                    
