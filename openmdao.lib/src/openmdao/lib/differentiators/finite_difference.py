""" Differentiates a driver's workflow using the finite difference method. A
variety of difference types are available for both first and second order."""

from ordereddict import OrderedDict

# pylint: disable-msg=E0611,F0401
from numpy import array

from enthought.traits.api import HasTraits
from openmdao.lib.datatypes.api import Enum, Float
from openmdao.main.interfaces import implements, IDifferentiator
from openmdao.main.container import find_name


def diff_1st_central(fp, fm, eps):
    """Evaluates a first order central difference."""
    
    return (fp - fm)/(2.0*eps)

def diff_1st_fwrdbwrd(fp, fm, eps):
    """Evaluates a first order forward or backward difference."""
    
    return (fp - fm)/eps

def diff_2nd_xx(fp, f0, fm, eps):
    """Evaluates an on-diagonal 2nd derivative term"""
    
    return (fp - 2.0*f0 + fm)/eps**2

def diff_2nd_xy(fpp, fpm, fmp, fmm, eps1, eps2):
    """Evaluates an off-diagonal 2nd derivative term"""
    
    return (fpp - fpm - fmp + fmm)/(4.0*eps1*eps2)


class FiniteDifference(HasTraits):
    """ Differentiates a driver's workflow using the finite difference method.
    A variety of difference types are available for both first and second
    order."""

    implements(IDifferentiator)
    
    # pylint: disable-msg=E1101
    form = Enum("central", ["central", "forward", "backward"], iotype='in', \
                desc="Finite difference form (central, forward, backward)")
    
    default_stepsize = Float(1.0e-6, iotype='in', desc='Default finite ' + \
                             'difference step size.')
    
    def __init__(self, parent):
        
        self._parent = parent
        
        self.param_names = []
        self.objective_names = []
        self.eqconst_names = []
        self.ineqconst_names = []
        
        self.gradient_case = OrderedDict()
        self.gradient = {}
        
        self.hessian_ondiag_case = OrderedDict()
        self.hessian_offdiag_case = OrderedDict()
        self.hessian = {}
        
    def setup(self):
        """Sets some dimensions."""

        self.param_names = self._parent.get_parameters().keys()
        self.objective_names = self._parent.get_objectives().keys()
        
        try:
            self.ineqconst_names = self._parent.get_ineq_constraints().keys()
        except AttributeError:
            self.ineqconst_names = []
        try:
            self.eqconst_names = self._parent.get_eq_constraints().keys()
        except AttributeError:
            self.eqconst_names = []
        
        
    def get_derivative(self, output_name, wrt):
        """Returns the derivative of output_name with respect to wrt.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
            
        wrt: string
            Name of the input in the local OpenMDAO hierarchy. The
            derivative is with respect to this variable.
        """
        
        return self.gradient[wrt][output_name]

    
    def get_2nd_derivative(self, output_name, wrt):
        """Returns the 2nd derivative of output_name with respect to both vars
        in the tuple wrt.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
            
        wrt: tuple containing two strings
            Names of the inputs in the local OpenMDAO hierarchy. The
            derivative is with respect to these 2 variables.
        """
        
        return self.hessian[wrt[0]][wrt[1]][output_name]

    
    def get_gradient(self, output_name=None):
        """Returns the gradient of the given output with respect to all 
        parameters.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
        """
        
        return array([self.gradient[wrt][output_name] for wrt in self.param_names])
        
        
    def get_Hessian(self, output_name=None):
        """Returns the Hessian matrix of the given output with respect to
        all parameters.
        
        output_name: string
            Name of the output in the local OpenMDAO hierarchy.
        """       
                

        return array([self.hessian[wrt[0]][wrt[1]][out] for out in name_list])


    def calc_gradient(self):
        """Returns the gradient vectors for this Driver's workflow."""
        
        self.setup()

        # Create our 2D dictionary the first time we execute.
        if not self.gradient:
            for name in self.param_names:
                self.gradient[name] = {}
                
        # Pull initial state and stepsizes from driver's parameters
        base_param = OrderedDict()
        stepsize = {}
        for key, item in self._parent.get_parameters().iteritems():
            base_param[key] = item.evaluate()
            
            if item.fd_step:
                stepsize[key] = item.fd_step
            else:
                stepsize[key] = self.default_stepsize

        # For Forward or Backward diff, we want to save the baseline
        # objective and constraints. These are also needed for the
        # on-diagonal Hessian terms, so we will save them in the class
        # later.
        base_data = self._run_point(base_param)
        
        # Set up problem based on Finite Difference type
        if self.form == 'central':
            deltas = [1, -1]
            func = diff_1st_central
        elif self.form == 'forward':
            deltas = [1, 0]
            func = diff_1st_fwrdbwrd
        else:
            deltas = [0, -1]
            func = diff_1st_fwrdbwrd

        self.gradient_case = OrderedDict()

        # Assemble input data
        for param in self.param_names:
            
            pcase = []
            for j_step, delta in enumerate(deltas):
                
                case = base_param.copy()
                case[param] += delta*stepsize[param]
                pcase.append({ 'param': case })
                
            self.gradient_case[param] = pcase
            
        # Run all "cases".
        # TODO - Integrate OpenMDAO's concurrent processing capability once it
        # is formalized. This operation is inherently paralellizable.
        for key, case in self.gradient_case.iteritems():
            for ipcase, pcase in enumerate(case):
                if deltas[ipcase]:
                    pcase['data'] = self._run_point(pcase['param'])
                else:
                    pcase['data'] = base_data
                
        
        # Calculate gradients
        for key, case in self.gradient_case.iteritems():
            
            eps = stepsize[key]
            
            for name in list(self.objective_names + \
                             self.eqconst_names + \
                             self.ineqconst_names):
                self.gradient[key][name] = \
                    func(case[0]['data'][name],
                         case[1]['data'][name], eps)

        # Save these for Hessian calculation
        self.base_param = base_param
        self.base_data = base_data

        
    def calc_hessian(self, reuse_first=False):
        """Returns the Hessian matrix for this Driver's workflow.
        
        reuse_first: bool
            Switch to reuse some data from the gradient calculation so that
            we don't have to re-run some points we already ran (namely the
            baseline, +eps, and -eps cases.) Obviously you do this when the
            driver needs gradient and hessian information at the same point.
        """
        
        self.setup()
        
        # Create our 3D dictionary the first time we execute.
        if not self.hessian:
            for name1 in self.param_names:
                self.hessian[name1] = {}
                for name2 in self.param_names:
                    self.hessian[name1][name2] = {}
                
        self.hessian_ondiag_case = OrderedDict()
        self.hessian_offdiag_case = OrderedDict()

        # Pull stepsizes from driver's parameters
        base_param = OrderedDict()
        stepsize = {}
        for key, item in self._parent.get_parameters().iteritems():
            
            if item.fd_step:
                stepsize[key] = item.fd_step
            else:
                stepsize[key] = self.default_stepsize

        # Diagonal terms in Hessian always need base point
        # Usually, we will have saved this when we calculated
        # the gradient.
        if reuse_first:
            base_param = self.base_param
            base_data = self.base_data
        else:
            # Pull initial state from driver's parameters
            for key, item in self._parent.get_parameters().iteritems():
                base_param[key] = item.evaluate()
                    
            base_data = self._run_point(base_param)
            
        # Assemble input data
        # Cases : ondiag [fp, fm]
        deltas = [1, -1]
        for param in self.param_names:
            
            pcase = []
            for j_step, delta in enumerate(deltas):
                
                case = base_param.copy()
                case[param] += delta*stepsize[param]
                pcase.append({ 'param': case })
                
            self.hessian_ondiag_case[param] = pcase
            
        # Assemble input data
        # Cases : offdiag [fpp, fpm, fmp, fmm]
        deltas = [[1, 1],
                  [1, -1],
                  [-1, 1],
                  [-1, -1]]
        for i, param1 in enumerate(self.param_names):
            
            offdiag = {}
            for param2 in self.param_names[i+1:]:
            
                pcase = []
                for delta in deltas:
                    
                    case = base_param.copy()
                    case[param1] += delta[0]*stepsize[param1]
                    case[param2] += delta[1]*stepsize[param2]
                    pcase.append({ 'param': case })
                offdiag[param2] = pcase
                    
            self.hessian_offdiag_case[param1] = offdiag
            
        # Run all "cases".
        # TODO - Integrate OpenMDAO's concurrent processing capability once it
        # is formalized. This operation is inherently paralellizable.
        
        # We don't need to re-run on-diag cases if the gradients were
        # calculated with Central Difference.
        if reuse_first and self.form=='central':
            for key, case in self.hessian_ondiag_case.iteritems():
                
                gradient_case = self.gradient_case[key]
                for ipcase, pcase in enumerate(case):
                    
                    gradient_ipcase = gradient_case[ipcase]
                    pcase['data'] = gradient_ipcase['data'] 
        else:
            for case in self.hessian_ondiag_case.values():
                for pcase in case:
                    data = self._run_point(pcase['param'])
                    pcase['data'] = data

        # Off-diag cases must always be run.
        for cases in self.hessian_offdiag_case.values():
            for case in cases.values():
                for pcase in case:
                    pcase['data'] = self._run_point(pcase['param'])

                    
        # Calculate Hessians - On Diagonal
        for key, case in self.hessian_ondiag_case.iteritems():
            
            eps = stepsize[key]
            
            for name in list(self.objective_names + \
                             self.eqconst_names + \
                             self.ineqconst_names):
                self.hessian[key][key][name] = \
                    diff_2nd_xx(case[0]['data'][name],
                                base_data[name],
                                case[1]['data'][name], eps)
                
        # Calculate Hessians - Off Diagonal
        for key1, cases in self.hessian_offdiag_case.iteritems():
            
            eps1 = stepsize[key1]
            for key2, case in cases.iteritems():
                
                eps2 = stepsize[key2]
                
                for name in list(self.objective_names + \
                                 self.eqconst_names + \
                                 self.ineqconst_names):
                    self.hessian[key1][key2][name] = \
                        diff_2nd_xy(case[0]['data'][name],
                                    case[1]['data'][name],
                                    case[2]['data'][name],
                                    case[3]['data'][name],
                                    eps1, eps2)
                    
                    # Symmetry
                    # (Should ponder whether we should even store it.)
                    self.hessian[key2][key1][name] = \
                        self.hessian[key1][key2][name]
                    
    
    def _run_point(self, data_param):
        """Runs the model at a single point and captures the results. Note that 
        some differences require the baseline point."""

        dvals = [float(val) for val in data_param.values()]
        self._parent.set_parameters(dvals)
        
        # Run the model
        super(type(self._parent), self._parent).run_iteration()
        
        data = {}

        # Get Objectives
        for key, item in self._parent.get_objectives().iteritems():
            data[key] = item.evaluate(self._parent.parent)

        # Get Inequality Constraints
        if self.ineqconst_names:
            for key, item in self._parent.get_ineq_constraints().iteritems():
                val = item.evaluate(self._parent.parent)
                if '>' in val[2]:
                    data[key] = val[1]-val[0]
                else:
                    data[key] = val[0]-val[1]
        
        # Get Equality Constraints
        if self.eqconst_names:
            for key, item in self._parent.get_eq_constraints().iteritems():
                val = item.evaluate(self._parent.parent)
                if '>' in val[2]:
                    data[key] = val[1]-val[0]
                else:
                    data[key] = val[0]-val[1]
        
        return data
                    

    def reset_state(self):
        """Finite Difference does not leave the model in a clean state. If you
        require one, then run this method."""
        
        dvals = [float(val) for val in self.base_param.values()]
        self._parent.set_parameters(dvals)
        super(type(self._parent), self._parent).run_iteration()

        
    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        name = find_name(self._parent, self)
        self._parent.raise_exception("%s: %s" % (name,msg), exception_class)
