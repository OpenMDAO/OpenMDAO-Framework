"""
    newsumtdriver.py - Driver for the NEWSUMT optimizer.
    
    (See Appendix B for additional information on the :ref:`NEWSUMTDriver`.)    
"""

# disable complaints about Module 'numpy' has no 'array' member
# pylint: disable-msg=E1101 

# Disable complaints Comma not followed by a space
# pylint: disable-msg=C0324

# disable complaints about .__init__: Use super on an old style class
# pylint: disable-msg=E1002

# Disable complaints Invalid name "setUp" (should match [a-z_][a-z0-9_]{2,30}$)
# pylint: disable-msg=C0103

# Disable complaints about not being able to import modules that Python
#     really can import
# pylint: disable-msg=F0401,E0611

# Disable complaints about Too many arguments (%s/%s)
# pylint: disable-msg=R0913

# Disable complaints about Too many local variables (%s/%s) Used
# pylint: disable-msg=R0914 

# Disable complaints about Unused argument %r
#       Used when a function or method argument is not used.
# pylint: disable-msg=W0613

#public symbols
__all__ = ['NEWSUMTdriver']


import os
import sys

from numpy import zeros
from numpy import float as numpy_float
from numpy import int as numpy_int

from enthought.traits.api import Array
                                 
from openmdao.main.api import Driver
from openmdao.main.exceptions import RunStopped
from openmdao.main.hasparameters import HasParameters
from openmdao.main.hasconstraints import HasConstraints
from openmdao.main.hasobjective import HasObjective
from openmdao.util.decorators import add_delegate
from openmdao.lib.datatypes.float import Float
from openmdao.lib.datatypes.int import Int

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


def user_function(info,x,obj,dobj,ddobj,g,dg,n2,n3,n4,nrandm,niandm,driver):
    """
       Calculate the objective functions, constraints,
         and gradients of those. Call back to the driver
         to get the values that were plugged 
         in.

    """

    driver.set_parameters(x)

    super(NEWSUMTdriver, driver).run_iteration()

    if info == 1:
        # evaluate objective function
        obj = driver.eval_objective()
    elif info == 2 :
        # evaluate constraint functions
        for i, v in enumerate(driver.get_ineq_constraints().values()):
            val = v.evaluate()
            if '>' in val[2]:
                g[i] = -(val[1]-val[0])
            else:
                g[i] = -(val[0]-val[1])

    elif info == 3 :
        # evaluate the first and second order derivatives
        #     of the objective function
        pass # for now
    elif info == 4:
        # evaluate gradient of nonlinear constraints
        driver.raise_exception('NEWSUMT does not yet support analytic constraint gradients',
                               RuntimeError)
    elif info == 5:
        # evaluate gradient of linear constraints
        pass # for now
    
    return obj,dobj,ddobj,g,dg


# pylint: disable-msg=R0913,R0902
@add_delegate(HasParameters, HasConstraints, HasObjective)
class NEWSUMTdriver(Driver):
    """ Driver wrapper of Fortran version of NEWSUMT. 
        
    
.. todo:: Make NEWSUMT's handling of user calculated gradients 
          accessible through NEWSUMTdriver.
            
.. todo:: Check to see if this itmax variable is needed.
            NEWSUMT might handle it for us.
            
    """

    fdcv = Array(dtype=numpy_float, value=zeros(0,'d'), iotype='in', 
        desc='Step size of the finite difference steps for each design \
              variable.')

    ilin = Array(dtype=numpy_int, value=zeros(0,'i4'), iotype='in', 
        desc='Array designating whether each constraint is linear.')
                 
    iside = Array(dtype=numpy_int, value=zeros(0,'i4'), iotype='in', 
        desc='Array indicating what kind of side constraint (aka lower \
        and upper bounds ): 0=no bounds, 1=lower bounds only, 2= upper \
        bounds only, 3= lower and upper bounds.')

    itmax = Int(10, iotype='in', desc='Maximum number of iterations before \
                    termination.')

    # Control parameters for NEWSUMT.
    # NEWSUMT has quite a few parameters to give the user control over aspects
    # of the solution. 
    epsgsn = Float(0.001, iotype='in', desc='Convergence criteria \
                      of the golden section algorithm used for the \
                      one dimensional minimization.')
    epsodm = Float(0.001, iotype='in', desc='Convergence criteria \
                      of the unconstrained minimization.')
    epsrsf = Float(0.001, iotype='in', desc='Convergence criteria \
                      for the overall process.')
    g0 = Float(0.1, iotype='in', desc='Initial value of the transition \
                      parameter.')
    ra = Float(1.0, iotype='in', desc='Penalty multiplier. Required if mflag=1')
    racut = Float(0.1, iotype='in', desc='Penalty multiplier decrease ratio. \
                      Required if mflag=1.')
    ramin = Float(1.0e-13, iotype='in', desc='Lower bound of \
                      penalty multiplier. \
                      Required if mflag=1.')
    stepmx = Float(2.0, iotype='in', desc='Maximum bound imposed on the \
                      initial step size of the one-dimensional \
                      minimization.')
    
    ifd = Int(4, iotype='in', desc='Flag for finite difference gradient control.\
                      If 0, all gradients computed by user analysis program. \
                      If > 0, use default finite difference stepsize of 0.1. \
                      If < 0, use user defined finite difference stepsize.\
                         FDCV must be specified.\
                      If 1, gradient of objective function computed by \
                         differences. \
                      If 2, gradient of all constraints computed by \
                         differences. \
                      If 3, gradient of nonlinear constraints computed\
                         by finite differences. \
                      If 4, combination of 1 and 2. \
                      If 5, combination of 1 and 3. \
                      ')
    jprint = Int(0, iotype='in', desc='Print information during NEWSUMT \
                    solution. Higher values are more verbose. If 0,\
                    print initial and final designs only.')
    lobj = Int(0, iotype='in', desc='If 1, linear objective function.')
    maxgsn = Int(20, iotype='in', desc='Maximum allowable number of golden \
                    section iterations used for 1D minimization.')
    maxodm = Int(6, iotype='in', desc='Maximum allowable number of one \
                    dimensional minimizations.')
    maxrsf = Int(15, iotype='in', desc='Maximum allowable number of \
                     unconstrained minimizations.')
    mflag = Int(15, iotype='in', desc='Flag for penalty multiplier. \
                     If 0, initial value computed by NEWSUMT. \
                     If 1, initial value set by ra.')
    ndv = Int(0, iotype='in', desc='Number of design variables.' )
    ntce = Int(0, iotype='in', desc='Number of constraints considered.' )
    
    def __init__(self, doc=None):
        super(NEWSUMTdriver, self).__init__( doc)
        
        self.iter_count = 0

        # define the NEWSUMTdriver's private variables
        # note, these are all resized in config_newsumt
        
        # basic stuff
        self.design_vals = zeros(0,'d')
        
        # temp storage
        self.__design_vals_tmp = zeros(0,'d')
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
        self._iik = zeros(0,dtype=int)
        self._ran = zeros(0)
        self._ian = zeros(0,dtype=int)

        self._lower_bounds = zeros(0)
        self._upper_bounds = zeros(0)

        # Just defined here. Set elsewhere
        self.n1 = self.n2 = self.n3 = self.n4 = 0
        self.constraint_vals = []
        
        # Ready inputs for NEWSUMT
        self._obj = 0.0
        self._objmin = 0.0

        # Size of some temp arrays needed by the routine that computes
        #   the objective functions and constraints
        self.nrandm = 1
        self.niandm = 1

        self.isdone = False
        self.resume = False
        
    def start_iteration(self):
        """Perform the optimization."""

        # set newsumt array sizes and more...
        self._config_newsumt()

        self.iter_count = 0
        
        # Right now, we only support numerical gradient calculation
        if self.ifd not in [4, -4]:
            msg = 'NEWSUMT does not yet support analytic constraint gradients'
            self.raise_exception(msg, RuntimeError)
        
        # get the values of the parameters
        # check if any min/max constraints are violated by initial values
        for i, val in enumerate(self.get_parameters().values()):
            self.design_vals[i] = val.expreval.evaluate()
            # next line is specific to NEWSUMT
            self.__design_vals_tmp[i] =  val.expreval.evaluate() 
            
#             if dval > val.high:
#                 if (dval - val.high) < self.ctlmin:
#                     self.design_vals[i] = val.high
#                 else:
#                     self.raise_exception('maximum exceeded for initial value'
#                                          ' of: %s' % str(val.expreval),
#                                          ValueError)
#             if dval < val.low:
#                 if (val.low - dval) < self.ctlmin:
#                     self.design_vals[i] = val.low
#                 else:
#                     self.raise_exception('minimum exceeded for initial value'
#                                          ' of: %s' % str(val.expreval),
#                                          ValueError)

        # perform an initial run for self-consistency
        super(NEWSUMTdriver, self).run_iteration()

        # update constraint value array
        for i, v in enumerate(self.get_ineq_constraints().values()):
            val = v.evaluate()
            if '>' in val[2]:
                self.constraint_vals[i] = -( val[1]-val[0] )
            else:
                self.constraint_vals[i] = -( val[0]-val[1] )


        # update objective
        self._obj = self.eval_objective()

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

        try:
            #redirect_fortran_stdout_to_null()
            
            ( fmin,self._obj,self._objmin,self.design_vals, 
              self.__design_vals_tmp,self.isdone,self.resume) = \
              newsumtinterruptible.newsuminterruptible(user_function,
                   self._lower_bounds, self._upper_bounds,
                   self._ddobj,self._dg,self._dh,self._dobj,
                   self.fdcv,self._g,
                   self._gb,self._g1,self._g2,self._g3,
                   self._obj,self._objmin,
                   self._s,self._sn,self.design_vals,self.__design_vals_tmp,
                   self._iik,self.ilin,self.iside,
                   self.n1,self.n2,self.n3,self.n4,
                   self._ran,self.nrandm,self._ian,self.niandm,
                   self.isdone,self.resume,analys_extra_args = (self,))
            
            #restore_fortran_stdout()

        except Exception, err:
            self._logger.error(str(err))
            raise
            
        self.iter_count += 1
                        
        # update the parameters in the model
        dvals = [float(val) for val in self.design_vals]
        self.set_parameters(dvals)
        
    def _config_newsumt(self):
        """Set up arrays for the Fortran newsumt routine, and perform some
        validation and make sure that array sizes are consistent.

        .. todo:: Save and restore common block data so that
                    this code can be used in workflows where
                    there are multiple instances of newsumt running.
            
        """

        params = self.get_parameters().values()
        ndv = len( params )
        if ndv < 1:
            self.raise_exception('no parameters specified', RuntimeError)
            
        # create lower_bounds array
        self._lower_bounds = zeros(ndv)
        for i, param in enumerate(params):
            self._lower_bounds[i] = param.low
            
        # create upper bounds array
        self._upper_bounds = zeros(ndv)
        for i, param in enumerate(params):
            self._upper_bounds[i] = param.high

        self.n1 = ndv
        ncon = len( self.get_ineq_constraints() )
        if ncon > 0:
            self.n2 = ncon
        else:
            self.n2 = 1
        self.n3 = ( ndv * ( ndv + 1 )) / 2
        if ncon > 0:
            self.n4 = ndv * ncon
        else:
            self.n4 = 1

        self.design_vals = zeros(ndv)
        self.constraint_vals = zeros(ncon)

        # Even though in some cases, these arrays are not used
        #    we need to set it to the proper array size
        #    because the wrapped newsumt expects an array
        #    sized properly as if it is really used

        if not len( self.fdcv ):
            self.fdcv = zeros(ndv)

        if len( self.ilin ) == 0 :
            if ncon > 0: 
                self.raise_exception('ilin not specified', RuntimeError)
                #self.ilin = zeros(ncon,dtype=int)
            else:
                self.ilin = zeros(1,dtype=int)

        if len(self.iside) == 0 :
            self.iside = zeros(ndv,dtype=int)

        # Set values in the common block
        newsumtinterruptible.contrl.epsgsn = self.epsgsn
        newsumtinterruptible.contrl.epsgsn = self.epsgsn
        newsumtinterruptible.contrl.epsodm = self.epsodm
        newsumtinterruptible.contrl.epsrsf = self.epsrsf
        newsumtinterruptible.contrl.g0 = self.g0
        newsumtinterruptible.contrl.ra = self.ra
        newsumtinterruptible.contrl.racut = self.racut
        newsumtinterruptible.contrl.ramin = self.ramin
        newsumtinterruptible.contrl.stepmx = self.stepmx
        newsumtinterruptible.contrl.ifd = self.ifd
        newsumtinterruptible.contrl.jprint = self.jprint
        newsumtinterruptible.contrl.lobj = self.lobj
        newsumtinterruptible.contrl.maxgsn = self.maxgsn
        newsumtinterruptible.contrl.maxodm = self.maxodm
        newsumtinterruptible.contrl.maxrsf = self.maxrsf
        newsumtinterruptible.contrl.mflag = self.mflag

        newsumtinterruptible.contrl.ndv = ndv
        newsumtinterruptible.contrl.ntce = ncon

        # work arrays
        self.__design_vals_tmp = zeros(self.n1,'d')
        self._ddobj = zeros( self.n3 )
        self._dg = zeros( self.n4 )
        self._dh = zeros( self.n1 )
        self._dobj = zeros( self.n1 )
        self._g = zeros( self.n2 )
        self._gb = zeros( self.n2 )
        self._g1 = zeros( self.n2 )
        self._g2 = zeros( self.n2 )
        self._g3 = zeros( self.n2 )
        self._s = zeros( self.n1 )
        self._sn = zeros( self.n1 )
        self._ran = zeros( self.nrandm )
        self._ian = zeros( self.niandm, dtype = int )
        self._iik = zeros( self.n1, dtype=int )


        
