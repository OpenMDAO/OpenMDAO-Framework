""" Object that can take a subsection of a model and perform finite difference
on it."""

#import cPickle
#import StringIO
from copy import deepcopy

from openmdao.lib.casehandlers.api import ListCaseRecorder
from openmdao.lib.drivers.distributioncasedriver import \
         DistributionCaseDriver, FiniteDifferenceGenerator


class FDhelper(object):
    
    def __init__(self, model, comps, wrt, outs, stepsize=1.0e-6, order=1,
                 form='CENTRAL'):
        ''' Takes a model and a list of component names in that model. The
        model is pickled and unpickled to create a copy. All but the needed
        comps are removed from the model.
        
        model: Assembly
            Parent assembly of the components we want to finite difference
            
        comps: list( string )
            List of component names that we want to finite difference as a
            group
            
        wrt: list( string )
            List of variable paths to use as finite difference inputs
            
        outs: list( string )
            List of variable paths to return as outputs
            
        stepsize: float
            Default stepsize to use
            
        order: int
            Finite Difference order. Only first order is supported right now.
            
        form: string
            Choose from 'CENTRAL', 'FORWARD', and "BACKWARD'. Default is central
            differencing
        '''
        
        # Copy model. We need to null out the reference to the parent before
        # we copy.
        save_parent = model.parent
        model.parent = None
        try:
            self.model = deepcopy(model)
        finally:
            model.parent = save_parent
            

        # Get rid of the comps we don't need
        for item in self.model.list_containers():
            if item not in comps + ['driver']:
                self.model.remove(item)
        
        # Distribution driver to drive the finite difference calculation
        self.model.add('driver', DistributionCaseDriver())
        gen = FiniteDifferenceGenerator(self.model.driver)
        self.model.driver.distribution_generator = gen
        self.model.driver.workflow.add(comps)
        
        for item in wrt:
            self.model.driver.add_parameter(item, low=-1e99, high=1e99,
                                            fd_step=stepsize)
        
        self.model.driver.case_outputs = outs
        self.model.driver.ffd_order = 1
            
        gen.num_parameters = len(wrt)
        gen.form = form
        gen.order = order
        
    def run(self, init_vals):
        """ Performs finite difference of our submodel with respect to wrt.
        Variables are intialized with init_vals. 
        
        init_vals: dict( string : value )
            Dictionary of all input variables for the coponents in this
            subsection of the model.
        """
        
        # Set all initial values
        for varname, value in init_vals.iteritems():
            self.model.set(varname, value)
            
        self.model.driver.recorders = [ListCaseRecorder()]
        
        # Calculate finite differences.
        # FFAD mode is supported.
        self.model.driver.calc_derivatives(first=True)
        self.model.run()
        
        # Return all needed derivatives
        print self.model.driver.recorders[0]
