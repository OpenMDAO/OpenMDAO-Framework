""" Differentiates a driver's workflow using the finite difference method. A
variety of difference types are available for both first and second order."""


# pylint: disable-msg=E0611,F0401
from numpy import zeros, ones

from enthought.traits.api import HasTraits
from openmdao.lib.datatypes.api import implements, Enum, Float
from openmdao.main.interfaces import IDifferentiator
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
        
        self.n_eqconst = 0
        self.n_ineqconst = 0
        self.n_param = 0
        self.n_objective = 0
        
        self.gradient_case = []
        self.gradient_obj = zeros(0, 'd')
        self.gradient_ineq_const = zeros(0, 'd')
        self.gradient_eq_const = zeros(0, 'd')
        
        self.hessian_ondiag_case = []
        self.hessian_offdiag_case = []
        self.hessian_obj = zeros(0, 'd')
        self.hessian_ineq_const = zeros(0, 'd')
        self.hessian_eq_const = zeros(0, 'd')
        
        # Some drivers will use the HasObjectives delegate, so we must allow
        # for the possibility of multiple objectives.
        self.multi_obj = False
    
    def setup(self):
        """Sets some dimensions."""

        self.n_param = len(self._parent._hasparameters._parameters)
        
        if hasattr(self._parent, '_hasobjectives'):
            self.n_objective = len(self._parent._hasobjectives._objectives)
            self.multi_obj = True
        else:
            self.n_objective = 1
            
        if hasattr(self._parent, '_hasineqconstraints'):
            self.n_ineqconst = len(self._parent._hasineqconstraints._constraints)
        if hasattr(self._parent, '_haseqconstraints'):
            self.n_eqconst = len(self._parent._haseqconstraints._constraints)
        if hasattr(self._parent, '_hasconstraints'):
            self.n_eqconst = len(self._parent._hasconstraints._eq._constraints)
            self.n_ineqconst = len(self._parent._hasconstraints._ineq._constraints)
        
    def calc_gradient(self):
        """Returns the gradient vectors for this Driver's workflow."""
        
        self.setup()

        # Dimension the matrices that will store the answers
        if self.multi_obj:
            self.gradient_obj = zeros([self.n_param, self.n_objective], 'd')
        else:
            self.gradient_obj = zeros(self.n_param, 'd')
        self.gradient_ineq_const = zeros([self.n_param, self.n_ineqconst], 'd')
        self.gradient_eq_const = zeros([self.n_param, self.n_eqconst], 'd')
        
        # Pull initial state and stepsizes from driver's parameters
        base_param = zeros(self.n_param, 'd')
        stepsize = ones(self.n_param, 'd')*self.default_stepsize
        for i_param, item in enumerate(self._parent.get_parameters().values()):
            base_param[i_param] = item.expreval.evaluate()
            
            fd_step = item.fd_step
            if fd_step:
                stepsize[i_param] = fd_step

        # For Forward or Backward diff, we want to save the baseline
        # objective and constraints. These are also needed for the
        # on-diagonal Hessian terms, so we will save them in the class
        # later.
        base_obj, base_ineqconst, base_eqconst = \
                self._run_point(base_param)
        
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

        self.gradient_case = []

        # Assemble input data
        for i_param in range(0, self.n_param):
            
            pcase = []
            for j_step, delta in enumerate(deltas):
                
                case = base_param.copy()
                case[i_param] += delta*stepsize[i_param]
                pcase.append({ 'param': case })
                
            self.gradient_case.append(pcase)
            
        # Run all "cases".
        # TODO - Integrate OpenMDAO's concurrent processing capability once it
        # is formalized. This operation is inherently paralellizable.
        for case in self.gradient_case:
            for ipcase, pcase in enumerate(case):
                if deltas[ipcase]:
                    data_obj, data_ineqconst, data_eqconst = \
                            self._run_point(pcase['param'])
                    
                    pcase['obj'] = data_obj
                    pcase['ineqconst'] = data_ineqconst
                    pcase['eqconst'] = data_eqconst
                else:
                    pcase['obj'] = base_obj
                    pcase['ineqconst'] = base_ineqconst
                    pcase['eqconst'] = base_eqconst
                
        
        # Calculate gradients
        for icase, case in enumerate(self.gradient_case):
            
            eps = stepsize[icase]
            
            # Calculate gradients
            if self.multi_obj:
                for j in range(0, self.n_objective):
                    self.gradient_obj[icase, j] = \
                        func(case[0]['obj'][j], case[1]['obj'][j], eps)
            else:
                self.gradient_obj[icase] = \
                    func(case[0]['obj'], case[1]['obj'], eps)

            for j in range(0, self.n_ineqconst):
                self.gradient_ineq_const[icase, j] = \
                    func(case[0]['ineqconst'][j],
                         case[1]['ineqconst'][j], eps)
                
            for j in range(0, self.n_eqconst):
                self.gradient_eq_const[icase, j] = \
                    func(case[0]['eqconst'][j],
                         case[1]['eqconst'][j], eps)

        # Save these for Hessian calculation
        self.base_param = base_param
        self.base_obj = base_obj
        self.base_eqconst = base_eqconst
        self.base_ineqconst = base_ineqconst
        
    def calc_hessian(self, reuse_first=False):
        """Returns the Hessian matrix for this Driver's workflow.
        
        reuse_first: bool
            Switch to reuse some data from the gradient calculation so that
            we don't have to re-run some points we already ran (namely the
            baseline, +eps, and -eps cases.) Obviously you do this when the
            driver needs gradient and hessian information at the same point.
        """
        
        self.setup()
        
        # Dimension the matrices that will store the answers
        self.hessian_obj = zeros([self.n_param, self.n_param], 'd')
        self.hessian_ineq_const = zeros([self.n_param, self.n_param, 
                                         self.n_ineqconst], 'd')
        self.hessian_eq_const = zeros([self.n_param, self.n_param, 
                                       self.n_eqconst], 'd')
        
        self.hessian_ondiag_case = []
        self.hessian_offdiag_case = []

        # Pull stepsizes from driver's parameters
        stepsize = ones(self.n_param, 'd')*self.default_stepsize
        for i_param, item in enumerate(self._parent.get_parameters().values()):
            fd_step = item.fd_step
            if fd_step:
                stepsize[i_param] = fd_step

        # Diagonal terms in Hessian always need base point
        # Usually, we will have saved this when we calculated
        # the gradient.
        if reuse_first:
            base_param = self.base_param
            base_obj = self.base_obj
            base_eqconst = self.base_eqconst
            base_ineqconst = self.base_ineqconst
        else:
            
            # Pull initial state from driver's parameters
            base_param = zeros(self.n_param, 'd')
            for i_param, item in enumerate(self._parent.get_parameters().values()):
                base_param[i_param] = item.expreval.evaluate()
                    
            base_obj, base_ineqconst, base_eqconst = \
                    self._run_point(base_param)
            
        # Assemble input data
        # Cases : ondiag [fp, fm]
        deltas = [1, -1]
        for i_param in range(0, self.n_param):
            
            pcase = []
            for j_step, step in enumerate(deltas):
                
                case = base_param.copy()
                case[i_param] += step*stepsize[i_param]
                pcase.append({ 'param': case })
                
            self.hessian_ondiag_case.append(pcase)
            
        # Assemble input data
        # Cases : offdiag [fpp, fpm, fmp, fmm]
        deltas = [[1, 1],
                  [1, -1],
                  [-1, 1],
                  [-1, -1]]
        for i_param in range(0, self.n_param):
            
            offdiag = []
            for j_param in range(0, i_param):
            
                pcase = []
                for delta in deltas:
                    
                    case = base_param.copy()
                    case[i_param] += delta[0]*stepsize[i_param]
                    case[j_param] += delta[1]*stepsize[j_param]
                    pcase.append({ 'param': case })
                offdiag.append(pcase)
                    
            self.hessian_offdiag_case.append(offdiag)
            
        # Run all "cases".
        # TODO - Integrate OpenMDAO's concurrent processing capability once it
        # is formalized. This operation is inherently paralellizable.
        if reuse_first and self.form=='central':
            for icase, case in enumerate(self.hessian_ondiag_case):
                
                gradient_case = self.gradient_case[icase]
                for ipcase, pcase in enumerate(case):
                    
                    gradient_ipcase = gradient_case[ipcase]
                    pcase['obj'] = gradient_ipcase['obj'] 
                    pcase['ineqconst'] = gradient_ipcase['ineqconst'] 
                    pcase['eqconst'] = gradient_ipcase['eqconst'] 
        else:
            for case in self.hessian_ondiag_case:
                for pcase in case:
                    
                    data_obj, data_ineqconst, data_eqconst = \
                            self._run_point(pcase['param'])
                    
                    pcase['obj'] = data_obj
                    pcase['ineqconst'] = data_ineqconst
                    pcase['eqconst'] = data_eqconst
                
        for cases in self.hessian_offdiag_case:
            for case in cases:
                for pcase in case:
                    
                    data_obj, data_ineqconst, data_eqconst = \
                            self._run_point(pcase['param'])
                    
                    pcase['obj'] = data_obj
                    pcase['ineqconst'] = data_ineqconst
                    pcase['eqconst'] = data_eqconst
                
        # Calculate Hessians
        for icase, case in enumerate(self.hessian_ondiag_case):
            
            eps = stepsize[icase]
            
            # Calculate Hessians
            self.hessian_obj[icase, icase] = \
                 diff_2nd_xx(case[0]['obj'], base_obj, case[1]['obj'], eps)
                 
            for j in range(0, self.n_ineqconst):
                self.hessian_ineq_const[icase, icase, j] = \
                    diff_2nd_xx(case[0]['ineqconst'][j],
                                base_ineqconst[j],
                                case[1]['ineqconst'][j], eps)
                
            for j in range(0, self.n_eqconst):
                self.hessian_eq_const[icase, icase, j] = \
                    diff_2nd_xx(case[0]['eqconst'][j],
                                base_eqconst[j],
                                case[1]['eqconst'][j], eps)
                
        for icase, cases in enumerate(self.hessian_offdiag_case):
            
            eps1 = stepsize[icase]
            for jcase, case in enumerate(cases):
            
                eps2 = stepsize[jcase]
                
                # Calculate Hessians
                self.hessian_obj[icase, jcase] = \
                     diff_2nd_xy(case[0]['obj'], 
                                 case[1]['obj'],
                                 case[2]['obj'],
                                 case[3]['obj'],
                                 eps1, eps2)
                
                self.hessian_obj[jcase, icase] = self.hessian_obj[icase, jcase]
                     
                for j in range(0, self.n_ineqconst):
                    self.hessian_ineq_const[icase, jcase, j] = \
                        diff_2nd_xy(case[0]['ineqconst'][j],
                                    case[1]['ineqconst'][j],
                                    case[2]['ineqconst'][j],
                                    case[3]['ineqconst'][j],
                                    eps1, eps2)
                    
                    self.hessian_ineq_const[jcase, icase, j] = \
                        self.hessian_ineq_const[icase, jcase, j]
                    
                for j in range(0, self.n_eqconst):
                    self.hessian_eq_const[icase, jcase, j] = \
                        diff_2nd_xy(case[0]['eqconst'][j],
                                    case[1]['eqconst'][j],
                                    case[2]['eqconst'][j],
                                    case[3]['eqconst'][j],
                                    eps1, eps2)
                
                    self.hessian_eq_const[jcase, icase, j] = \
                        self.hessian_eq_const[icase, jcase, j]
                    
    
    def _run_point(self, data_param):
        """Runs the model at a single point and captures the results. Note that 
        some differences require the baseline point."""

        # Temp storage of all responses
        data_ineqconst = zeros([self.n_ineqconst], 'd')
        data_eqconst = zeros([self.n_eqconst], 'd')

        dvals = [float(val) for val in data_param]
        self._parent.set_parameters(dvals)
        
        # Runs the model
        super(type(self._parent), self._parent).run_iteration()

        if self.multi_obj:
            data_obj = self._parent.eval_objectives()
        else:
            data_obj = self._parent.eval_objective()

        if self.n_ineqconst:
            for j, v in enumerate(self._parent.get_ineq_constraints().values()):
                val = v.evaluate()
                if '>' in val[2]:
                    data_ineqconst[j] = val[1]-val[0]
                else:
                    data_ineqconst[j] = val[0]-val[1]
        
        if self.n_eqconst:
            for j, v in enumerate(self._parent.get_eq_constraints().values()):
                val = v.evaluate()
                if '>' in val[2]:
                    data_eqconst[j] = val[1]-val[0]
                else:
                    data_eqconst[j] = val[0]-val[1]
                    
        return data_obj, data_ineqconst, data_eqconst
                    
                    
    def raise_exception(self, msg, exception_class=Exception):
        """Raise an exception."""
        name = find_name(self._parent, self)
        self._parent.raise_exception("%s: %s" % (name,msg), exception_class)
