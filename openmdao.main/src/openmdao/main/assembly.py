#public symbols
__all__ = ["Assembly"]

__version__ = "0.1"

import os
import copy

from zope.interface import implements

from openmdao.main.interfaces import IAssembly, IComponent, IVariable
from openmdao.main import Component
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.filevar import FileVariable
from openmdao.main.tarjan import strongly_connected_components
from openmdao.main.util import filexfer


def connected_dest_vars(obj):
    return IVariable.providedBy(obj) and obj.source is not None
        
def _add_dependency(dep_graph, srccompname, destcompname):
    if destcompname in dep_graph:
        if srccompname not in dep_graph[destcompname]:
            dep_graph[destcompname].append(srccompname)
    else:                
        dep_graph[destcompname] = [srccompname]
        
    if srccompname not in dep_graph:
        dep_graph[srccompname] = []
    

class Assembly(Component):
    """A container for Components, a Driver, and a Workflow. It manages
    connections between Components."""
   
    implements(IAssembly)
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(Assembly, self).__init__(name, parent, doc=doc,
                                       directory=directory)
        #self._connections = {} # dependencies between Components
        self.driver = self.create('openmdao.main.driver.Driver', 'driver')
        self.workflow = self.create('openmdao.main.workflow.Workflow',
                                    'workflow')
        self._dep_graph = None

        
    def get_dependency_graph(self):
        """Return a component dependency graph in the form of a dictionary"""
        if self._dep_graph is not None:
            return self._dep_graph
        
        dep_graph = {}
        for src,dest in self.list_connections():
            srccompname, srccomp, srcvarname, srcvar = self.split_varpath(src)
            if srccompname is None:
                continue
            destcompname, destcomp, destvarname, destvar = self.split_varpath(dest)
            if destcompname is None:
                continue
            
            _add_dependency(dep_graph, destcompname, srccompname)
                
        return dep_graph
    
    def _check_circular_deps(self, incomp, invar, outname):
        """Create a dependency graph based on the current graph with the
        addition of the proposed new connection, and raise an exception if a 
        circular dependency is detected.
        """
        dep_graph = self.get_dependency_graph()
        
        # now add the new dep
        out = outname.split('.')[0]
        
        if incomp == out:
            self.raise_exception('Cannot connect a component ('+
                                 incomp+') to itself', RuntimeError)

        new_graph = copy.deepcopy(dep_graph)
        _add_dependency(new_graph, incomp, out)
        
        sccomps = strongly_connected_components(new_graph)
        for comp in sccomps:
            if len(comp) > 1:
                self.raise_exception('Circular dependency would be '+
                                     'created by connecting '+
                                     incomp+'.'+invar+' to '+outname, 
                                     RuntimeError)
        return new_graph

    def split_varpath(self, path):
        """Return a tuple of compname,component,varname,variable given a path
        name of the form 'compname.varname'. If the name is of the form 'varname'
        then compname will be None and comp is self.
        """
        try:
            compname, varname = path.split('.', 1)
        except ValueError:
            compname = None
            comp = self
            varname = path
            var = getattr(self, varname)
        else:
            comp = getattr(self, compname)
            if not IComponent.providedBy(comp):
                self.raise_exception('%s is not a Component' % compname, TypeError)               
            var = comp.getvar(varname)
        if not IVariable.providedBy(var):
            self.raise_exception('%s is not a Variable' % varname, TypeError)
        return (compname, comp, varname, var)

    
    def connect(self, srcpath, destpath):
        """Connect one src Variable to one destination Variable. This could be
        a normal connection (output to input) or a passthru connection."""

        srccompname, srccomp, srcvarname, srcvar = self.split_varpath(srcpath)
        destcompname, destcomp, destvarname, destvar = self.split_varpath(destpath)
        
        # if the Variables involved in the connection are from different
        # scoping levels, then it's a passthru connection
        if srccomp is self or destcomp is self:
            if srccomp is destcomp:
                self.raise_exception('cannot connect "%s" to "%s" on same component' %
                                      (srcvarname, destvarname), RuntimeError)
            else:
                passthru = True
        else:
            passthru = False                   
            if srcvar.iostatus != OUTPUT:
                self.raise_exception(srcvar.get_pathname()+
                                     ' must be an OUTPUT variable',
                                     RuntimeError)
            if destvar.iostatus != INPUT:
                self.raise_exception(destvar.get_pathname()+
                                     ' must be an INPUT variable',
                                     RuntimeError)
        
        if destvar.is_destination():
            self.raise_exception(destpath+' is already connected',
                                 RuntimeError)
                    
        # test compatability
        destvar.validate_var(srcvar)
        
        new_graph = self._check_circular_deps(destcompname, destvarname, srcpath) 
        
        destvar.connect_src(srcpath)
        self._dep_graph = new_graph
            
    def disconnect(self, varpath):
        """Remove all connections from a given variable."""
        var = self.getvar(varpath)
        found = False
        if var.source is None: # may be a source, so try to find destination
            for src,dest in self.list_connections():
                if src == varpath:
                    found = True
                    self.getvar(dest).disconnect_src()
                    
        if found is False:
            var.disconnect_src()
            
        self._dep_graph = None  # force regeneration of dep_graph

    def execute(self):
        """run this Assembly by handing control to the driver"""
        return self.driver.run()

    def step(self):
        """Execute a single step."""
        self.driver.step()

    def stop(self):
        """ Stop by telling the driver to stop. """
        self._stop = True
        self.driver.stop()

    def remove_child(self, name):
        """Remove the named object from this container and notify any 
        observers.
        """
        
        # TODO: notify observers of removal...
                 
        for src,dest in self.list_connections():
            srccompname,srccomp,srcvarname,srcvar = self.split_varpath(src)
            destcompname,destcomp,destvarname,destvar = self.split_varpath(dest)
            # disconnect any vars reading from vars of this component, and all
            # destination vars in the component being removed
            if srccompname == name or destcompname == name:
                destvar.disconnect_src()
            
        self.workflow.remove_node(getattr(self, name))           
        Component.remove_child(self, name)

    def list_connections(self, fullpath=False):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        for comp in self.get_objs(IComponent.providedBy):
            for var in comp.get_objs(connected_dest_vars, pub=True):
                if fullpath is True:
                    inname = var.get_pathname()
                    outname = self.getvar(var.source).get_pathname()
                else:
                    inname = '.'.join([var.parent.name, var.name])
                    outname = var.source
                conns.append((outname, inname))
        
        return conns
    
    def update_inputs(self, incomp):
        """Transfer input data to the specified component.
        Note that we're called after incomp has set its execution directory,
        so we'll need to account for this during file transfers."""
        
        for var in incomp.get_objs(connected_dest_vars, pub=True):
            srccompname, srccomp, srcvarname, srcvar = self.split_varpath(var.source)
            if isinstance(var, FileVariable):
                incomp.pop_dir()
                try:
                    self.xfer_file(srccomp, srcvar, incomp, var)
                    var.metadata = srcvar.metadata.copy()
                except Exception, exc:
                    msg = "cannot transfer file from '%s' to '%s': %s" % \
                          (var.source,
                           '.'.join((incomp.name, var.name)), str(exc))
                    self.raise_exception(msg, type(exc))
                finally:
                    incomp.push_dir(incomp.get_directory())
            else:
                srcvar = self.getvar(var.source)
                try:
                    var.setvar(None, srcvar)
                except Exception, exc:
                    msg = "cannot set '%s' from '%s': %s" % \
                          ('.'.join((incomp.name, var.name)),
                           srcvar, str(exc))
                    self.raise_exception(msg, type(exc))

    @staticmethod
    def xfer_file(src_comp, src_var, dst_comp, dst_var):
        """ Transfer src_comp.src_ref file to dst_comp.dst_ref file. """
        src_path = os.path.join(src_comp.get_directory(), src_var.value)
        dst_path = os.path.join(dst_comp.get_directory(), dst_var.value)
        if src_path != dst_path:
            if src_var.metadata['binary']:
                mode = 'b'
            else:
                mode = ''
            filexfer(None, src_path, None, dst_path, mode)

