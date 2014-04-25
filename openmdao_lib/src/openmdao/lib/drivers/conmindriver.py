"""
    ``conmindriver.py`` - Driver for the CONMIN optimizer.

    The CONMIN driver can be a good example of how to wrap another Driver for
    OpenMDAO. However, there are some things to keep in mind.

    1. This implementation of the CONMIN Fortran driver is interruptable, in
    that control is returned every time an objective or constraint evaluation
    is needed. Most external optimizers just expect a function to be passed
    for these, so they require a slightly different driver implementation than
    this.

    2. The CONMIN driver is a direct wrap, and all inputs are passed using
    Numpy arrays. This means there are a lot of locally stored variables
    that you might not need for a pure Python optimizer.

    Ultimately, if you are wrapping a new optimizer for OpenMDAO, you should
    endeavour to understand the purpose for each statement so that your
    implementation doesn't do any unneccessary or redundant calculation.
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['CONMINdriver']

import logging

# pylint: disable-msg=E0611,F0401
try:
    from numpy import zeros, ones
    from numpy import int as numpy_int
except ImportError as err:
    logging.warn("In %s: %r", __file__, err)
    # to keep class decl from barfing before being stubbed out
    zeros = lambda *args, **kwargs: None
    numpy_int = int

from openmdao.main.driver_uses_derivatives import Driver
from openmdao.main.exceptions import RunStopped
from openmdao.main.datatypes.api import Array, Bool, Enum, Float, Int
from openmdao.main.interfaces import IHasParameters, IHasIneqConstraints, \
                                     IHasObjective, implements, IOptimizer
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasIneqConstraints
from openmdao.main.hasobjective import HasObjective
from openmdao.util.decorators import add_delegate, stub_if_missing_deps

import conmin.conmin as conmin


class _cnmn1(object):
    """Just a primitive data structure for storing cnmnl common block data.

    We save the common blocks to prevent collision in the case where there are
    multiple instances of CONMIN running in our model."""

    def __init__(self):
        self.clear()

    def clear(self):
        """ Clear values. """

        # pylint: disable-msg=W0201
        self.ndv = 0
        self.ncon = 0
        self.nside = 0
        self.infog = 0
        self.info = 0
        self.nfdg = 0
        self.icndir = 0
        self.nscal = 0
        self.fdch = 0.0
        self.fdchm = 0.0
        self.ct = 0.
        self.ctmin = 0.
        self.ctlmin = 0.
        self.theta = 0.
        self.delfun = 0.
        self.dabfun = 1.e-8
        self.linobj = 0
        self.itrm = 0
        self.alphax = 0.
        self.abobj1 = 0.
        self.ctl = 0.
        self.igoto = 0
        self.nac = 0
        self.iter = 0
        self.iprint = 0
        self.itmax = 0
        # pylint: enable-msg=W0201

class _consav(object):
    """Just a primitive data structure for storing consav common block data.

    We save the common blocks to prevent collision in the case where there are
    multiple instances of CONMIN running in our model."""

    def __init__(self):
        self.clear()

    def clear(self):
        """ Clear values. """

        # pylint: disable-msg=W0201
        self.dm1 = 0.0
        self.dm2 = 0.0
        self.dm3 = 0.0
        self.dm4 = 0.0
        self.dm5 = 0.0
        self.dm6 = 0.0
        self.dm7 = 0.0
        self.dm8 = 0.0
        self.dm9 = 0.0
        self.dm10 = 0.0
        self.dm11 = 0.0
        self.dm12 = 0.0
        self.dct = 0.0
        self.dctl = 0.0
        self.phi = 0.0
        self.abobj = 0.0
        self.cta = 0.0
        self.ctam = 0.0
        self.ctbm = 0.0
        self.obj1 = 0.0
        self.slope = 0.0
        self.dx = 0.0
        self.dx1 = 0.0
        self.fi = 0.0
        self.xi = 0.0
        self.d_objtdf1 = 0.0
        self.alp = 0.0
        self.fff = 0.0
        self.a1 = 0.0
        self.a2 = 0.0
        self.a3 = 0.0
        self.a4 = 0.0
        self.f1 = 0.0
        self.f2 = 0.0
        self.f3 = 0.0
        self.f4 = 0.0
        self.cv1 = 0.0
        self.cv2 = 0.0
        self.cv3 = 0.0
        self.cv4 = 0.0
        self.app = 0.0
        self.alpca = 0.0
        self.alpfes = 0.0
        self.alpln = 0.0
        self.alpmin = 0.0
        self.alpnc = 0.0
        self.alpsav = 0.0
        self.alpsid = 0.0
        self.alptot = 0.0
        self.rspace = 0.0
        self.idm1 = 0
        self.idm2 = 0
        self.idm3 = 0
        self.jdir = 0
        self.iobj = 0
        self.kobj = 0
        self.kcount = 0
        self.ncal = [0, 0]
        self.nfeas = 0
        self.mscal = 0
        self.ncobj = 0
        self.nvc = 0
        self.kount = 0
        self.icount = 0
        self.igood1 = 0
        self.igood2 = 0
        self.igood3 = 0
        self.igood4 = 0
        self.ibest = 0
        self.iii = 0
        self.nlnc = 0
        self.jgoto = 0
        self.ispace = [0, 0]
        # pylint: enable-msg=W0201

@stub_if_missing_deps('numpy', 'conmin')
@add_delegate(HasParameters, HasIneqConstraints, HasObjective)
class CONMINdriver(Driver):
    """ Driver wrapper of Fortran version of CONMIN.

    Note on self.cnmn1.igoto, which reports CONMIN's operation state:
        0: Initial and final state

        1: Initial evaluation of objective and constraint values

        2: Evaluate gradients of objective and constraints (internal only)

        3: Evaluate gradients of objective and constraints

        4: One-dimensional search on unconstrained function

        5: Solve 1D search problem for unconstrained function

    """
    # I don't see an IUsesGradients
    implements(IHasParameters, IHasIneqConstraints, IHasObjective, IOptimizer)

    # pylint: disable-msg=E1101
    # Control parameters for CONMIN.
    # CONMIN has quite a few parameters to give the user control over aspects
    # of the solution.

    cons_is_linear = Array(zeros(0, 'i'), dtype=numpy_int, iotype='in',
        desc='Array designating whether each constraint is linear.')

    iprint = Enum(0, [0, 1, 2, 3, 4, 5, 101], iotype='in', desc='Print '
                    'information during CONMIN solution. Higher values are '
                    'more verbose. 0 suppresses all output.')
    itmax = Int(10, iotype='in', desc='Maximum number of iterations before '
                    'termination.')
    fdch = Float(.01, iotype='in', desc='Relative change in parameters '
                      'when calculating finite difference gradients'
                      ' (only when CONMIN calculates gradient).')
    fdchm = Float(.01, iotype='in', desc='Minimum absolute step in finite '
                      'difference gradient calculations'
                      ' (only when CONMIN calculates gradient).')
    icndir = Float(0, iotype='in', desc='Conjugate gradient restart. '
                      'parameter.')
    ct = Float(-0.1, iotype='in', desc='Constraint thickness parameter.')
    ctmin = Float(0.004, iotype='in', desc='Minimum absolute value of ct '
                      'used in optimization.')
    ctl = Float(-0.01, iotype='in', desc='Constraint thickness parameter for '
                      'linear and side constraints.')
    ctlmin = Float(0.001, iotype='in', desc='Minimum absolute value of ctl '
                      'used in optimization.')
    theta = Float(1.0, iotype='in', desc='Mean value of the push-off factor '
                      'in the method of feasible directions.')
    phi = Float(5.0, iotype='in', desc='Participation coefficient - penalty '
                      'parameter that pushes designs towards the feasible '
                      'region.')
    delfun = Float(0.001, iotype='in', low=0.0,
                   desc='Relative convergence tolerance.')
    dabfun = Float(0.001, iotype='in', low=1.0e-10,
                   desc='Absolute convergence tolerance.')
    linobj = Bool(False, iotype='in', desc='Linear objective function flag. '
                    'Set to True if objective is linear.')
    conmin_diff = Bool(False, iotype='in', desc='Set to True to let CONMIN'
                       'calculate the gradient.')
    itrm = Int(3, iotype='in', desc='Number of consecutive iterations to '
                      'indicate convergence (relative or absolute).')


    def __init__(self):
        super(CONMINdriver, self).__init__()

        # Save data from common blocks into our CONMINdriver object
        self.cnmn1 = _cnmn1()
        self.consav = _consav()

        self.iter_count = 0

        # Since CONMIN is an external Fortran code, it requires data to be
        # passed via several Numpy arrays. We definie all these arrays here
        # and initialize to zero length. They are later resized in
        # config_conmin.

        # basic stuff
        self.design_vals = zeros(0, 'd')
        self._scal = zeros(0, 'd')
        self.cons_active_or_violated = zeros(0, 'i')
        self._cons_is_linear = zeros(0, 'i')
        self.d_const = zeros(0, 'd')

        # gradient of objective w.r.t x[i]
        self.d_obj = zeros(0, 'd')

        # move direction in the optimization space
        self.s = zeros(0, 'd')

        # temp storage
        self._b = zeros(0, 'd')
        self._c = zeros(0, 'd')
        self._ms1 = zeros(0, 'i')

        # temp storage for constraints
        self.g1 = zeros(0, 'd')
        self.g2 = zeros(0, 'd')

        self._lower_bounds = zeros(0, 'd')
        self._upper_bounds = zeros(0, 'd')

    def start_iteration(self):
        """Perform initial setup before iteration loop begins."""

        # Inital run to make sure the workflow executes
        super(CONMINdriver, self).run_iteration()

        self._config_conmin()
        self.cnmn1.igoto = 0
        self.iter_count = 0

        # get the initial values of the parameters
        self.design_vals[:-2] = self.eval_parameters(self.parent)

        # check if any min/max constraints are violated by initial values
        start = 0
        for param in self.get_parameters().values():
            size = param.size
            low = param.get_low()
            high = param.get_high()

            for i in range(size):
                index = start + i
                vlow = low[i]
                vhigh = high[i]
                dval = self.design_vals[index]
                if dval > vhigh:
                    if (dval - vhigh) < self.ctlmin:
                        self.design_vals[index] = vhigh
                    elif size == 1:
                        self.raise_exception(
                            'initial value of: %s is greater than maximum'
                            % param.target, ValueError)
                    else:
                        self.raise_exception(
                            'initial value of: %s (offset %s) is greater'
                            ' than maximum' % (param.target, i), ValueError)
                if dval < vlow:
                    if (vlow - dval) < self.ctlmin:
                        self.design_vals[index] = vlow
                    elif size == 1:
                        self.raise_exception(
                            'initial value of: %s is less than minimum'
                            % param.target, ValueError)
                    else:
                        self.raise_exception(
                            'initial value of: %s (offset %s) is less'
                            ' than minimum' % (param.target, i), ValueError)
            start += size


    def continue_iteration(self):
        """Returns True if iteration should continue."""

        return self.cnmn1.igoto != 0 or self.iter_count == 0


    def pre_iteration(self):
        """Checks for RunStopped."""

        super(CONMINdriver, self).pre_iteration()
        if self._stop:
            self.raise_exception('Stop requested', RunStopped)


    def run_iteration(self):
        """ The CONMIN driver iteration."""

        #self._logger.debug('iter_count = %d' % self.iter_count)
        #self._logger.debug('objective = %f' % self.cnmn1.obj)
        #self._logger.debug('design vals = %s' % self.design_vals[:-2])

        # TODO: 'step around' ill-behaved cases.

        self._load_common_blocks()

        #print "Iteration %s: " % self.get_pathname(), self.iter_count
        #print "Before"
        #print self.design_vals
        try:
            (self.design_vals,
             self._scal, self.d_const, self.s,
             self.g1, self.g2, self._b, self._c,
             self._cons_is_linear,
             self.cons_active_or_violated, self._ms1) = \
                 conmin.conmin(self.design_vals,
                               self._lower_bounds, self._upper_bounds,
                               self.constraint_vals,
                               self._scal, self.d_obj,
                               self.d_const,
                               self.s, self.g1, self.g2, self._b, self._c,
                               self._cons_is_linear,
                               self.cons_active_or_violated, self._ms1)
        except Exception as err:
            self._logger.error(str(err))
            raise

        self._save_common_blocks()

        # calculate objective and constraints
        if self.cnmn1.info == 1:

            # Note. CONMIN is driving the finite difference estimation of the
            # gradient. However, we still take advantage of a component's
            # user-defined gradients via Fake Finite Difference.
            if self.cnmn1.igoto == 3:

                # update the parameters in the model
                self.set_parameters(self.design_vals[:-2])

                # Run model under Fake Finite Difference
                self.calc_derivatives(first=True, savebase=True)
                self.ffd_order = 1
                super(CONMINdriver, self).run_iteration()
                self.ffd_order = 0
            else:
                # update the parameters in the model
                self.set_parameters(self.design_vals[:-2])

                # Run the model for this step
                super(CONMINdriver, self).run_iteration()

            # calculate objective
            self.cnmn1.obj = self.eval_objective()

            # update constraint value array
            self.constraint_vals[0:self.total_ineq_constraints()] = \
                self.eval_ineq_constraints()

            #self._logger.debug('constraints = %s' % self.constraint_vals)

        # calculate gradient of constraints and gradient of objective
        # We also have to determine which constraints are active/violated, and
        # only return gradients of active/violated constraints.
        elif self.cnmn1.info == 2 and self.cnmn1.nfdg == 1:

            # Sometimes, CONMIN wants the derivatives at a different point.
            self.set_parameters(self.design_vals[:-2])
            super(CONMINdriver, self).run_iteration()

            inputs = self.list_param_group_targets()
            obj = self.list_objective_targets()
            con = self.list_ineq_constraint_targets()

            J = self.workflow.calc_gradient(inputs, obj + con)

            nobj = len(obj)
            self.d_obj[:-2] = J[0:nobj, :].ravel()

            for i in range(len(self.cons_active_or_violated)):
                self.cons_active_or_violated[i] = 0

            self.cnmn1.nac = 0
            for i in range(self.total_ineq_constraints()):
                if self.constraint_vals[i] >= self.cnmn1.ct:
                    self.cons_active_or_violated[self.cnmn1.nac] = i+1
                    self.d_const[:-2, self.cnmn1.nac] = J[nobj+i, :]
                    self.cnmn1.nac += 1
        else:
            self.raise_exception('Unexpected value for flag INFO returned'
                                 ' from CONMIN.', RuntimeError)


    def post_iteration(self):
        """ Checks CONMIN's return status and writes out cases."""

        super(CONMINdriver, self).post_iteration()

        # Iteration count comes from CONMIN. You can't just count over the
        # loop because some cycles do other things (e.g., numerical
        # gradient calculation)
        if (self.iter_count != self.cnmn1.iter) or self.cnmn1.igoto == 0:
            self.iter_count = self.cnmn1.iter


    def _config_conmin(self):
        """Set up arrays for the Fortran conmin routine, perform some
        validation, and make sure that array sizes are consistent.
        """

        self.cnmn1.clear()
        self.consav.clear()

        # size arrays based on number of parameters
        num_dvs = self.total_parameters()
        if num_dvs < 1:
            self.raise_exception('no parameters specified', RuntimeError)
        self.design_vals = zeros(num_dvs+2, 'd')

        # create lower_bounds array
        self._lower_bounds = zeros(num_dvs+2)
        self._lower_bounds[:-2] = self.get_lower_bounds()

        # create upper bounds array
        self._upper_bounds = zeros(num_dvs+2)
        self._upper_bounds[:-2] = self.get_upper_bounds()

        # create array for CONMIN's internal scaling
        # we no longer use these, but the still need the empty arrays
        self._scal = ones(num_dvs+2)
        self.s = zeros(num_dvs+2, 'd')

        # size constraint related arrays
        length = self.total_ineq_constraints() + 2*num_dvs
        self.constraint_vals = zeros(length, 'd')

        # temp storage of constraint and design vals
        self.g1 = zeros(length, 'd')

        # temp storage of constraint vals
        self.g2 = zeros(length, 'd')

        # if constraint i is known to be a linear function of design vars,
        # the user can set cons_is_linear[i] to 1, otherwise set it to 0. This
        # is not essential and is for efficiency only.
        self._cons_is_linear = zeros(length, 'i')
        if len(self.cons_is_linear) > 0:
            if len(self.cons_is_linear) != self.total_ineq_constraints():
                self.raise_exception('size of cons_is_linear (%d) does not'
                                     ' match number of constraints (%d)' %
                                     (len(self.cons_is_linear),
                                      self.total_ineq_constraints()), ValueError)
            else:
                for i, val in enumerate(self.cons_is_linear):
                    self._cons_is_linear[i] = val

        self.cnmn1.ndv = num_dvs
        self.cnmn1.ncon = self.total_ineq_constraints()

        self.cnmn1.nside = 2*num_dvs

        nacmx1 = max(num_dvs, self.cnmn1.ncon+self.cnmn1.nside)+1

        n1 = num_dvs+2
        n3 = nacmx1
        n4 = max(n3, num_dvs)
        n5 = 2*n4

        # array of active or violated constraints (ic in CONMIN)
        self.cons_active_or_violated = zeros(n3, 'i')

        # Let CONMIN calc gradients
        if self.conmin_diff:
            self.nfdg = 0
        else:
            self.nfdg = 1

        self.d_const = zeros((int(n1), int(n3)), 'd')
        self.d_obj = zeros(num_dvs+2, 'd')

        # these are all temp storage
        self._b = zeros((int(n3), int(n3)), 'd')
        self._c = zeros(n4, 'd')
        self._ms1 = zeros(n5, 'i')

        # Load all of the user-changeable parameters into the common block
        self.cnmn1.iprint = self.iprint
        self.cnmn1.itmax = self.itmax
        self.cnmn1.fdch = self.fdch
        self.cnmn1.fdchm = self.fdchm
        self.cnmn1.icndir = self.icndir
        self.cnmn1.nscal = 0
        self.cnmn1.nfdg = self.nfdg
        self.cnmn1.ct = self.ct
        self.cnmn1.ctmin = self.ctmin
        self.cnmn1.ctl = self.ctl
        self.cnmn1.ctlmin = self.ctlmin
        self.cnmn1.theta = self.theta
        self.consav.phi = self.phi
        self.cnmn1.dabfun = self.dabfun
        self.cnmn1.delfun = self.delfun
        if self.linobj:
            self.cnmn1.linobj = 1
        else:
            self.cnmn1.linobj = 0
        self.cnmn1.itrm = self.itrm


    def _load_common_blocks(self):
        """ Reloads the common blocks using the intermediate info saved in the
            class.
            """

        for name, value in self.cnmn1.__dict__.items():
            setattr(conmin.cnmn1, name, value)

        for name, value in self.consav.__dict__.items():
            setattr(conmin.consav, name, value)


    def _save_common_blocks(self):
        """" Saves the common block data to the class to prevent trampling by
        other instances of CONMIN.
        """

        common = self.cnmn1
        for name, value in common.__dict__.items():
            setattr(common, name, type(value)(getattr(conmin.cnmn1, name)))

        consav = self.consav
        for name, value in consav.__dict__.items():
            setattr(consav, name, type(value)(getattr(conmin.consav, name)))

