
#public symbols
__all__ = ['Component']

__version__ = "0.1"

import os
import os.path
import copy

from zope.interface import implements

from openmdao.main.interfaces import IComponent, IVariable
from openmdao.main import Container, String
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.constants import SAVE_PICKLE
from openmdao.main.exceptions import RunFailed
from openmdao.main.filevar import FileVariable
from openmdao.main.util import filexfer

# Execution states.
STATE_UNKNOWN = -1
STATE_IDLE    = 0
STATE_RUNNING = 1
STATE_WAITING = 2

class Component (Container):
    """This is the base class for all objects containing Variables that are 
       accessible to the OpenMDAO framework and are 'runnable'.
    """

    implements(IComponent)
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(Component, self).__init__(name, parent, doc)
        
        self.state = STATE_IDLE
        self._stop = False
        self._updated = False  # if False, component needs to run in order
                               # for its output Variables to be correct
        self._need_check_config = True
        self._dir_stack = []
        
        # List of meta-data dictionaries.
        self.external_files = []

        String('directory', self, INPUT, default=directory,
               doc='If non-null, the directory to execute in.')

# pylint: disable-msg=E1101
        if self.directory:
            if not os.path.exists(self.directory):
# TODO: Security!
                try:
                    os.makedirs(self.directory)
                except OSError, exc:
                    self.error("Could not create directory '%s': %s",
                               self.directory, exc.strerror)
            else:
                if not os.path.isdir(self.directory):
                    self.error("Path '%s' is not a directory.", self.directory)
# pylint: enable-msg=E1101

    def check_config (self):
        """Verify that the configuration of this component is correct. This function is
        called once prior to the first execution of this component, and may be called
        explicitly at other times if desired.
        """
        pass         
    
    def _pre_execute (self):
        """Update input variables and anything else needed prior 
        to execution. """
        if self._need_check_config:
            self.check_config()
            self._need_check_config = False
    
    def _execute_if_needed(self):
        """Only call execute() if any of our outputs are invalid."""
        if self.parent is None: # if parent is None, we're not part of an Assembly
                                # so Variable validity doesn't apply. Just execute.
            self.execute()
        else:
            for obj in self.values():
                if IVariable.providedBy(obj) and obj.valid == False:
                    if IComponent.providedBy(self.parent):
                        self.parent.update_inputs(self)
            
                    self.execute()
                    return
            
    def execute (self):
        """Perform calculations or other actions, assuming that inputs 
        have already been set. This should be overridden in derived classes.
        """
        pass
    
    def _post_execute (self):
        """Update output variables and anything else needed after execution."""
        for obj in [x for x in self.values() if IVariable.providedBy(x)]:
            obj.valid == True
    
    def run (self, force=False):
        """Run this object. This should include fetching input variables,
        executing, and updating output variables. Do not override this function.
        """
        directory = self.get_directory()
        try:
            self.push_dir(directory)
        except OSError, exc:
            msg = "Could not move to execution directory '%s': %s" % \
                  (directory, exc.strerror)
            self.raise_exception(msg, RunFailed)

        self.state = STATE_RUNNING
        self._stop = False
        try:
            self._pre_execute()
            self._execute_if_needed()
            self._post_execute()
        finally:
            self.state = STATE_IDLE
            self.pop_dir()

    def get_directory (self):
        """ Return absolute path of execution directory. """
        path = self.directory
        if not os.path.isabs(path):
            if self.parent is not None and IComponent.providedBy(self.parent):
                parent_dir = self.parent.get_directory()
            else:
                parent_dir = os.getcwd()
            path = os.path.join(parent_dir, path)
        return path

    def push_dir (self, directory):
        """Change directory to dir, remembering current for later pop_dir()."""
        if not directory:
            directory = '.'
        cwd = os.getcwd()
# TODO: Security!
        os.chdir(directory)
        self._dir_stack.append(cwd)

    def pop_dir (self):
        """ Return to previous directory saved by push_dir(). """
        os.chdir(self._dir_stack.pop())

    def checkpoint (self, outstream, format=SAVE_PICKLE):
        """Save sufficient information for a restart. By default, this
        just calls save()
        """
        self.save(outstream, format)

    def restart (self, instream):
        """Restore state using a checkpoint file. The checkpoint file is
        typically a delta from a full saved state file. If checkpoint is
        overridden, this should also be overridden.
        """
        self.load(instream)

    def step (self):
        """For Components that contain Workflows (e.g., Model), this will run
        one Component in the Workflow and return. For simple components, it is
        the same as run().
        """
        self.run()

    def stop (self):
        """ Stop this component. """
        self._stop = True

    def get_invalidated_outputs(self, invars):
        """Return a list of outputs we've invalidates based on the list of input 
        Variables.  An Assembly may invalidate only a subset of its outputs, but
        we're a simple Component, so we'll invalidate all valid ones, regardless
        of which invars are changed.  We don't return outputs that were already 
        invalid before the call.
        """
        outs = []
        for out in [x for x in self.values() 
                         if IVariable.providedBy(x) and 
                            x.iostatus==OUTPUT and x.valid is True]:
            out.valid = False
            outs.append(out)
        return outs
            
    
    def invalidate_deps(self, var):
        """If we have a parent Assembly, pass this call up 
        to it since we don't have a dependency graph."""
        if self.parent:
            self.parent.ivalidate_deps(var)
    
# TODO: uncomment require_gradients and require_hessians after they're better thought out
    
    #def require_gradients (self, varname, gradients):
        #"""Requests that the component be able to provide (after execution) a
        #list of gradients w.r.t. a list of variables. The format
        #of the gradients list is [dvar_1, dvar_2, ..., dvar_n]. The component
        #should return a list with entries of either a name, a tuple of the
        #form (name,index) or None.  None indicates that the component cannot
        #compute the specified derivative. name indicates the name of a
        #scalar variable in the component that contains the gradient value, and
        #(name,index) indicates the name of an array variable and the index of
        #the entry containing the gradient value. If the component cannot
        #compute any gradients of the requested varname, it can just return
        #None.
        #"""
        #return None

    #def require_hessians (self, varname, deriv_vars):
        #""" Requests that the component be able to provide (after execution)
        #the hessian w.r.t. a list of variables. The format of
        #deriv_vars is [dvar_1, dvar_2, ..., dvar_n]. The component should
        #return one of the following:

            #1) a name, which would indicate that the component contains
               #a 2D array variable or matrix containing the hessian

            #2) an array of the form 

               #[[dx1dx1, dx1dx2, ... dx1dxn],
               #[           ...             ],
               #[dxndx1, dxndx2, ... dxndxn]]

               #with entries of either name, (name,index), or None. name
               #indicates that a scalar variable in the component contains the
               #desired hessian matrix entry. (name,index) indicates that
               #an array variable contains the value at the specified index.
               #If index is a list with two entries, that indicates that
               #the variable containing the entry is a 2d array or matrix.

            #3) None, which means the the component cannot compute any values
               #of the hessian.

             #"""
        #return None
    
    
