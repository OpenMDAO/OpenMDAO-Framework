""" Object that can take a subsection of a model and perform finite difference
on it."""

#import cPickle
#import StringIO
from copy import deepcopy

# pylint: disable-msg=E0611,F0401
from openmdao.lib.casehandlers.api import ListCaseRecorder
from openmdao.lib.drivers.distributioncasedriver import \
         DistributionCaseDriver, FiniteDifferenceGenerator


class FDhelper(object):
    ''' An object that takes a subsection of a model and performs a finite
    difference. The cases are run with a point distribution generator. Thus,
    we can take advantage of multiprocessing if it is available.
    '''
    
    def __init__(self, model, comps, wrt, outs, stepsize=1.0e-6, order=1,
                 form='CENTRAL'):
        ''' Takes a model and a list of component names in that model. The
        model is deepcopied to create a copy. All but the needed comps are
        removed from the model.
        
        model: Assembly
            Parent assembly of the components we want to finite difference.
            
        comps: list( string )
            List of component names that we want to finite difference as a
            group.
            
        wrt: list( string )
            List of variable paths to use as finite difference inputs.
            
        outs: list( string )
            List of variable paths to return as outputs.
            
        stepsize: float
            Default stepsize to use.
            
        order: int
            Finite Difference order. Only first order is supported right now.
            
        form: string
            Choose from 'CENTRAL', 'FORWARD', and "BACKWARD'. Default is central
            differencing.
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
                
        # Remove all connections to the assembly boundary
        bdy_inputs = self.model.list_inputs()
        bdy_outputs = self.model.list_outputs()
        for conn in self.model.list_connections():
            if conn[0] in bdy_inputs or conn[1] in bdy_outputs:
                self.model.disconnect(conn[0], conn[1])
        
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
        
        # Save a reference to the original model so that we can increment the
        # execution counter as needed.
        self.copy_source = model
        
        # All execution counts should be reset to zero.
        for comp in self.model.driver.workflow.__iter__():
            comp.exec_count = 0
            comp.derivative_exec_count = 0
        
        
    def run(self, input_dict, output_dict):
        """ Performs finite difference of our submodel with respect to wrt.
        Variables are intialized with init_vals. 
        
        input_dict: dict( string : value )
            Dictionary of baseline values for input paramters.
            
        input_dict: dict( string : value )
            Dictionary of baseline values for desired outputs.
        """
        
        # Set all initial values
        for varname, value in input_dict.iteritems():
            self.model.set(varname, value)
            
        self.model.driver.recorders = [ListCaseRecorder()]
        
        if self.model.driver.distribution_generator.form != 'CENTRAL':
            self.model.driver.distribution_generator.skip_baseline = True
            
        # Calculate finite differences.
        # FFAD mode is supported.
        self.model.driver.calc_derivatives(first=True, savebase=True)
        self.model.run()
        
        # Return all needed derivatives
        cases = self.model.driver.recorders[0].cases
        
        icase = 0
        derivs = {}
        for out in self.model.driver.case_outputs:
            derivs[out] = {}
        
        for wrt, val in self.model.driver.get_parameters().iteritems():
            
            if self.model.driver.distribution_generator.form == 'CENTRAL':
                
                delx = cases[icase][wrt] - cases[icase+1][wrt]
                for out in self.model.driver.case_outputs:
                    
                    derivs[out][wrt] = \
                        (cases[icase][out] - cases[icase+1][out])/delx
                    

                icase += 2
                
            else:
                
                delx = cases[icase][wrt] - input_dict[wrt]
                for out in self.model.driver.case_outputs:
                    
                    derivs[out][wrt] = \
                        (cases[icase][out] - output_dict[out])/delx
                        
                icase += 1
                
        # Add the execution count from the copies to the originals.
        for comp in self.model.driver.workflow.__iter__():
            source_comp = self.copy_source.get(comp.name)
            source_comp.exec_count += comp.exec_count
            comp.exec_count = 0
            source_comp.derivative_exec_count += comp.derivative_exec_count
            comp.derivative_exec_count = 0
        
        return derivs
    
    def list_wrt(self):
        """ Returns a list of variable paths that we are differencing with
        respect to.
        """
        return self.model.driver.get_parameters().keys()

    def list_outs(self):
        """ Returns a list of variable paths that we are differencing.
        """
        return self.model.driver.case_outputs
