"""
    ``newsumtdriver.py`` - Driver for the NEWSUMT optimizer.

"""

# disable complaints about Module 'numpy' has no 'array' member
# pylint: disable=E1101

# Disable complaints Invalid name "setUp" (should match [a-z_][a-z0-9_]{2,30}$)
# pylint: disable=C0103

# Disable complaints about not being able to import modules that Python
#     really can import
# pylint: disable=F0401,E0611

# Disable complaints about Too many arguments (%s/%s)
# pylint: disable=R0913

# Disable complaints about Too many local variables (%s/%s) Used
# pylint: disable=R0914

#public symbols
__all__ = ['NEWSUMTdriver']

from numpy import zeros, ones, putmask
from numpy import int as numpy_int


from openmdao.main.datatypes.api import Array, Float, Int, Bool
from openmdao.main.exceptions import RunStopped
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasIneqConstraints
from openmdao.main.hasobjective import HasObjective
from openmdao.main.driver_uses_derivatives import Driver
from openmdao.util.decorators import add_delegate
from openmdao.main.interfaces import IHasParameters, IHasIneqConstraints, \
                                     IHasObjective, implements, IOptimizer

import newsumt.newsumtinterruptible as newsumtinterruptible


# code for redirecting unit stderr and stdout
#  output from newsumt Fortran code
# Not using it now

# save = None
# null_fds = None

# def redirect_fortran_stdout_to_null():
#     '''
#       capture the output intended for
#       stdout and just send it to dev/null
#     '''

#     global save, null_fds

#     sys.stdout.flush()

#     #sys.stdout = open(os.devnull, 'w')
#     #sys.stdout = WritableObject()
#     # open 2 fds
#     null_fds = [os.open(os.devnull, os.O_RDWR), os.open(os.devnull, os.O_RDWR)]
#     # save the current file descriptors to a tuple
#     save = os.dup(1), os.dup(2)
#     # put /dev/null fds on 1 and 2
#     os.dup2(null_fds[0], 1)
#     os.dup2(null_fds[1], 2)

# def restore_fortran_stdout():
#     '''
#     restore stdout to the
#     value it has before the call to
#     redirect_fortran_stdout_to_null
#     '''

#     global save, null_fds

#     sys.stdout.flush()

#     #sys.stdout == sys.__stdout__
#     # restore file descriptors so I can print the results
#     os.dup2(save[0], 1)
#     os.dup2(save[1], 2)
#     # close the temporary fds
#     os.close(null_fds[0])
#     os.close(null_fds[1])


# Disable complaints about Unused argument
# pylint: disable=W0613
def user_function(info, x, obj, dobj, ddobj, g, dg, n2, n3, n4, imode, driver):
    """
       Calculate the objective functions, constraints,
         and gradients of those. Call back to the driver
         to get the values that were plugged
         in.

       Note, there is some evidence of loss of precision on the output of
       this function.
    """

    # evaluate objective function or constraint function
    if info in [1, 2]:

        if imode == 1:

            # We are in a finite difference step drive by NEWSUMT

            # Note, NEWSUMT estimates 2nd-order derivatives from
            # the first order differences.

            # Save baseline states and calculate derivatives
            #if driver.baseline_point:
            #    driver.calc_derivatives(first=True, savebase=True)
            #driver.baseline_point = False

            # update the parameters in the model
            driver.set_parameters(x)
            super(NEWSUMTdriver, driver).run_iteration()

        else:

            # Optimization step
            driver.set_parameters(x)
            super(NEWSUMTdriver, driver).run_iteration()

        # evaluate objectives
        if info == 1:
            obj = driver.eval_objective()

        # evaluate constraint functions
        if info == 2:
            for i, v in enumerate(driver.eval_ineq_constraints(driver.parent)):
                g[i] = -v

        # save constraint values in driver if this isn't a finite difference
        if imode != 1:
            driver.constraint_vals = g

    elif info == 3:
        # evaluate the first and second order derivatives
        #     of the objective function

        # NEWSUMT bug: sometimes we end up here when ifd=-4
        if driver.newsumt_diff:
            return obj, dobj, ddobj, g, dg

        msg = "Hessians currently not supported by OpenMDAO differentiator"
        raise NotImplementedError(msg)

        #driver.differentiator.calc_gradient()
        #driver.differentiator.calc_hessian(reuse_first=True)

        #obj_name = driver.get_objectives().keys()[0]
        #dobj = driver.differentiator.get_gradient(obj_name)

        #i_current = 0
        #names = driver.get_parameters().keys()
        #for row, name1 in enumerate(names):
            #for name2 in names[0:row+1]:
                #ddobj[i_current] = driver.differentiator.get_2nd_derivative(obj_name, wrt=(name1, name2))
                #i_current += 1

    elif info in [4, 5]:
        # evaluate gradient of nonlinear or linear constraints.

        # Linear gradients are only called once, at startup
        if info == 5:
            # NEWSUMT bug - During initial run, NEWSUMT will ask for analytic
            # derivatives of the linear constraints even when ifd=-4. The only
            # thing we can do is return zero.
            if driver.newsumt_diff:
                return obj, dobj, ddobj, g, dg

            raise RuntimeError("newsumt ifd value of %d is not handled" % info)
            #driver.differentiator.calc_gradient()

        #i_current = 0
        #for param_name in driver.get_parameters():
            #for con_name in driver.get_ineq_constraints():
                #dg[i_current] = -driver.differentiator.get_derivative(con_name, wrt=param_name)
                #i_current += 1

    return obj, dobj, ddobj, g, dg
# pylint: enable=W0613


class _contrl(object):
    """Just a primitive data structure for storing contrl common block data.

    We save the common blocks to prevent collision in the case where there are
    multiple instances of NEWSUMT running in our model."""

    def __init__(self):
        self.clear()

    def clear(self):
        """ Clear values. """

        # pylint: disable=W0201
        self.c = 0.0
        self.epsgsn = 0.0
        self.epsodm = 0.0
        self.epsrsf = 0.0
        self.fdch = 0.0
        self.g0 = 0.0
        self.ifd = 0
        self.iflapp = 0
        self.iprint = 0
        self.jsigng = 0
        self.lobj = 0
        self.maxgsn = 0
        self.maxodm = 0
        self.maxrsf = 0
        self.mflag = 0
        self.ndv = 0
        self.ntce = 0
        self.p = 0.0
        self.ra = 0.0
        self.racut = 0.0
        self.ramin = 0.0
        self.stepmx = 0.0
        self.tftn = 0.0
        # pylint: enable=W0201


class _countr(object):
    """Just a primitive data structure for storing countr common block data.

    We save the common blocks to prevent collision in the case where there are
    multiple instances of NEWSUMT running in our model."""

    def __init__(self):
        self.clear()

    def clear(self):
        """ Clear values. """

        # pylint: disable=W0201
        self.iobjct = 0
        self.iobapr = 0
        self.iobgrd = 0
        self.iconst = 0
        self.icongr = 0
        self.inlcgr = 0
        self.icgapr = 0
        # pylint: enable=W0201


# pylint: disable=R0913,R0902
@add_delegate(HasParameters, HasIneqConstraints, HasObjective)
class NEWSUMTdriver(Driver):
    """ Driver wrapper of Fortran version of NEWSUMT.


.. todo:: Check to see if this itmax variable is needed.
            NEWSUMT might handle it for us.

    """

    implements(IHasParameters, IHasIneqConstraints, IHasObjective, IOptimizer)

    itmax = Int(10, iotype='in', desc='Maximum number of iterations before '
                                       'termination.')

    default_fd_stepsize = Float(0.01, iotype='in', desc='Default finite '
                                'difference stepsize. Parameters with '
                                'specified values override this.')

    ilin = Array(dtype=numpy_int, default_value=zeros(0, 'i4'), iotype='in',
                 desc='Array designating whether each constraint is linear.')

    # Control parameters for NEWSUMT.
    # NEWSUMT has quite a few parameters to give the user control over aspects
    # of the solution.
    epsgsn = Float(0.001, iotype='in', desc='Convergence criteria '
                      'of the golden section algorithm used for the '
                      'one dimensional minimization.')
    epsodm = Float(0.001, iotype='in', desc='Convergence criteria '
                      'of the unconstrained minimization.')
    epsrsf = Float(0.001, iotype='in', desc='Convergence criteria '
                      'for the overall process.')
    g0 = Float(0.1, iotype='in', desc='Initial value of the transition '
                      'parameter.')
    ra = Float(1.0, iotype='in', desc='Penalty multiplier. Required if mflag=1')
    racut = Float(0.1, iotype='in', desc='Penalty multiplier decrease ratio. '
                      'Required if mflag=1.')
    ramin = Float(1.0e-13, iotype='in', desc='Lower bound of '
                      'penalty multiplier. Required if mflag=1.')
    stepmx = Float(2.0, iotype='in', desc='Maximum bound imposed on the '
                      'initial step size of the one-dimensional '
                      'minimization.')

    iprint = Int(0, iotype='in', desc='Print information during NEWSUMT '
                    'solution. Higher values are more verbose. If 0,'
                    'print initial and final designs only.', high=4, low=0)
    lobj = Int(0, iotype='in', desc='Set to 1 if linear objective function.')
    maxgsn = Int(20, iotype='in', desc='Maximum allowable number of golden '
                    'section iterations used for 1D minimization.')
    maxodm = Int(6, iotype='in', desc='Maximum allowable number of one '
                    'dimensional minimizations.')
    maxrsf = Int(15, iotype='in', desc='Maximum allowable number of '
                     'unconstrained minimizations.')
    mflag = Int(0, iotype='in', desc='Flag for penalty multiplier. '
                     'If 0, initial value computed by NEWSUMT. '
                     'If 1, initial value set by ra.')
    newsumt_diff = Bool(True, iotype='in', desc='Set to True to let NEWSUMT'
                       'calculate the gradient.')

    def __init__(self):
        super(NEWSUMTdriver, self).__init__()

        self.iter_count = 0

        # Save data from common blocks into the driver
        self.contrl = _contrl()
        self.countr = _countr()

        # define the NEWSUMTdriver's private variables
        # note, these are all resized in config_newsumt

        # basic stuff
        self.design_vals = zeros(0, 'd')
        self.constraint_vals = []

        # temp storage
        self.__design_vals_tmp = zeros(0, 'd')
        self._ddobj = zeros(0)
        self._dg = zeros(0)
        self._dh = zeros(0)
        self._dobj = zeros(0)
        self._g = zeros(0)
        self._gb = zeros(0)
        self._g1 = zeros(0)
        self._g2 = zeros(0)
        self._g3 = zeros(0)
        self._s = zeros(0)
        self._sn = zeros(0)
        self._x = zeros(0)
        self._iik = zeros(0, dtype=int)
        self._ilin = zeros(0, dtype=int)

        self._lower_bounds = zeros(0)
        self._upper_bounds = zeros(0)
        self._iside = zeros(0)
        self.fdcv = zeros(0)

        # Just defined here. Set elsewhere
        self.n1 = self.n2 = self.n3 = self.n4 = 0

        # Ready inputs for NEWSUMT
        self._obj = 0.0
        self._objmin = 0.0

        self.isdone = False
        self.resume = False

    def check_config(self, strict=False):
        """ OpenMDAO Hessian unsupported right now. """

        super(NEWSUMTdriver, self).check_config(strict=strict)

        if self.newsumt_diff is False:
            msg = "Hessians currently not supported by OpenMDAO differentiator"
            raise NotImplementedError(msg)

    def start_iteration(self):
        """Perform the optimization."""

        # Inital run to make sure the workflow executes
        super(NEWSUMTdriver, self).run_iteration()

        # set newsumt array sizes and more...
        self._config_newsumt()

        self.iter_count = 0

        # get the values of the parameters
        # check if any min/max constraints are violated by initial values
        self.design_vals = self.eval_parameters(self.parent)
        self.__design_vals_tmp = self.design_vals.copy()

        # Call the interruptible version of SUMT in a loop that we manage
        self.isdone = False
        self.resume = False

    def continue_iteration(self):
        """Returns True if iteration should continue."""

        return not self.isdone and self.iter_count < self.itmax

    def pre_iteration(self):
        """Checks or RunStopped and evaluates objective."""

        super(NEWSUMTdriver, self).pre_iteration()
        if self._stop:
            self.raise_exception('Stop requested', RunStopped)


    def run_iteration(self):
        """ The NEWSUMT driver iteration."""

        self._load_common_blocks()

        try:
            (fmin, self._obj, self._objmin, self.design_vals,
             self.__design_vals_tmp, self.isdone, self.resume) = \
              newsumtinterruptible.newsuminterruptible(user_function,
                   self._lower_bounds, self._upper_bounds,
                   self._ddobj, self._dg, self._dh, self._dobj,
                   self.fdcv, self._g,
                   self._gb, self._g1, self._g2, self._g3,
                   self._obj, self._objmin,
                   self._s, self._sn, self.design_vals, self.__design_vals_tmp,
                   self._iik, self._ilin, self._iside,
                   self.n1, self.n2, self.n3, self.n4,
                   self.isdone, self.resume, analys_extra_args=(self,))

        except Exception as err:
            self._logger.error(str(err))
            raise

        self._save_common_blocks()

        self.iter_count += 1

        # Update the parameters and run one final time with what it gave us.
        # This update is needed because I obeserved that the last callback to
        # user_function is the final leg of a finite difference, so the model
        # is not in sync with the final design variables.
        if not self.continue_iteration():
            self.set_parameters(self.design_vals)
            super(NEWSUMTdriver, self).run_iteration()

    def _config_newsumt(self):
        """Set up arrays for the Fortran newsumt routine, and perform some
        validation and make sure that array sizes are consistent.
        """

        ndv = self.total_parameters()
        if ndv < 1:
            self.raise_exception('no parameters specified', RuntimeError)

        # Create some information arrays using our Parameter data

        self._lower_bounds = self.get_lower_bounds()
        self._upper_bounds = self.get_upper_bounds()

        # The way Parameters presently work, we always specify an
        # upper and lower bound
        self._iside = ones(ndv) * 3

        # Update default stepsize where specified as non-zero.
        self.fdcv = ones(ndv) * self.default_fd_stepsize
        fd_steps = self.get_fd_steps(dtype=None)
        putmask(self.fdcv, fd_steps, fd_steps)

        if self.newsumt_diff:
            ifd = -4
        else:
            ifd = 0

        self.n1 = ndv
        ncon = self.total_ineq_constraints()
        if ncon > 0:
            self.n2 = ncon
        else:
            self.n2 = 1
        self.n3 = (ndv * (ndv + 1)) / 2
        if ncon > 0:
            self.n4 = ndv * ncon
        else:
            self.n4 = 1

        self.design_vals = zeros(ndv)
        self.constraint_vals = zeros(ncon)

        # Linear constraint setting
        if len(self.ilin) == 0:
            if ncon > 0:
                self.ilin = zeros(ncon, dtype=int)
                self._ilin = self.ilin
            else:
                self._ilin = zeros(1, dtype=int)
        elif len(self.ilin) != ncon:
            msg = "Dimension of NEWSUMT setting 'ilin' should be equal to " \
                  "the number of constraints."
            self.raise_exception(msg, RuntimeError)
        else:
            self._ilin = self.ilin

        # Set initial values in the common blocks
        self.countr.clear()
        self.contrl.clear()
        self.contrl.c = 0.2
        self.contrl.epsgsn = self.epsgsn
        self.contrl.epsodm = self.epsodm
        self.contrl.epsrsf = self.epsrsf
        self.contrl.fdch = 0.05
        self.contrl.g0 = self.g0
        self.contrl.ifd = ifd
        self.contrl.iflapp = 0
        self.contrl.jprint = self.iprint - 1
        self.contrl.jsigng = 1
        self.contrl.lobj = self.lobj
        self.contrl.maxgsn = self.maxgsn
        self.contrl.maxodm = self.maxodm
        self.contrl.maxrsf = self.maxrsf
        self.contrl.mflag = self.mflag
        self.contrl.ndv = ndv
        self.contrl.ntce = ncon
        self.contrl.p = 0.5
        self.contrl.ra = self.ra
        self.contrl.racut = self.racut
        self.contrl.ramin = self.ramin
        self.contrl.stepmx = self.stepmx
        self.contrl.tftn = 0.0

        # work arrays
        self.__design_vals_tmp = zeros(self.n1, 'd')
        self._ddobj = zeros(self.n3)
        self._dg = zeros(self.n4)
        self._dh = zeros(self.n1)
        self._dobj = zeros(self.n1)
        self._g = zeros(self.n2)
        self._gb = zeros(self.n2)
        self._g1 = zeros(self.n2)
        self._g2 = zeros(self.n2)
        self._g3 = zeros(self.n2)
        self._s = zeros(self.n1)
        self._sn = zeros(self.n1)
        self._iik = zeros(self.n1, dtype=int)

    def _load_common_blocks(self):
        """ Reloads the common blocks using the intermediate info saved in the
        class.
        """

        for name, value in self.contrl.__dict__.items():
            setattr(newsumtinterruptible.contrl, name, value)

        for name, value in self.countr.__dict__.items():
            setattr(newsumtinterruptible.countr, name, value)

    def _save_common_blocks(self):
        """ Saves the common block data to the class to prevent trampling by
        other instances of NEWSUMT.
        """

        common = self.contrl
        for name, value in common.__dict__.items():
            setattr(common, name,
                    type(value)(getattr(newsumtinterruptible.contrl, name)))

        common = self.countr
        for name, value in common.__dict__.items():
            setattr(common, name,
                    type(value)(getattr(newsumtinterruptible.countr, name)))

    def requires_derivs(self):
        return True

