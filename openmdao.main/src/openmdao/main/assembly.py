
#public symbols
__all__ = ['Assembly']

__version__ = "0.1"

import os
import os.path
import copy
from collections import deque

from zope.interface import implements
import networkx as nx

from openmdao.main.interfaces import IAssembly, IComponent, IDriver, IVariable
from openmdao.main import Container, String
from openmdao.main.component import Component, STATE_IDLE
from openmdao.main.dataflow import Dataflow
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.constants import SAVE_PICKLE
from openmdao.main.exceptions import RunFailed, CircularDependencyError
from openmdao.main.filevar import FileVariable
from openmdao.main.util import filexfer


class Assembly (Component):
    """This is a container of Components. It understands how
    to connect their inputs and outputs and how to handle
    Sockets.
    """

    implements(IAssembly)
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(Assembly, self).__init__(name, parent, doc)
        
        self.state = STATE_IDLE
        self._stop = False
        self._dir_stack = []
        self._sockets = {}
        self._child_io_graphs = {}
        self._need_child_io_update = True
        
        # A graph of Variable names (local path), with connections between 
        # Variables as directed edges.  Children are queried for dependencies
        # between their inputs and outputs so they can also be represented
        # in the graph.  Each node in the graph also contains the actual
        # Variable object as data.
        self._var_graph = nx.LabeledDiGraph()
        
        self._comp_graph = nx.DiGraph()
        
        # add any Variables we may have inherited from our base classes
        # to our graph, since our version of add_child wasn't active yet
        for missing in [v for v in self.values() if IVariable.providedBy(v)
                                                 and v.name not in self._var_graph]:
            self._var_graph.add_node(missing.name, data=missing)
        
        
        self._dataflow = Dataflow('dataflow', self)

        # List of meta-data dictionaries.
        self.external_files = []


    def _get_socket_plugin(self, name):
        """Return plugin for the named socket"""
        try:
            plugin = self._sockets[name][1]
        except KeyError:
            self.raise_exception("no such socket '%s'" % name, AttributeError)
        else:
            if plugin is None:
                self.raise_exception("socket '%s' is empty" % name,
                                     RuntimeError)
            return plugin

    def _set_socket_plugin(self, name, plugin):
        """Set plugin for the named socket"""
        try:
            iface, current, required, doc = self._sockets[name]
        except KeyError:
            self.raise_exception("no such socket '%s'" % name, AttributeError)
        else:
            if plugin is not None and iface is not None:
                if not iface.providedBy(plugin):
                    self.raise_exception("plugin does not support '%s'" % \
                                         iface.__name__, ValueError)
            self._sockets[name] = (iface, plugin, required, doc)

    def add_socket (self, name, iface, doc='', required=True):
        """Specify a named placeholder for a component with the given
        interface or prototype.
        """
        assert isinstance(name, basestring)
        self._sockets[name] = (iface, None, required, doc)
        setattr(self.__class__, name,
                property(lambda self : self._get_socket_plugin(name),
                         lambda self, plugin : self._set_socket_plugin(name, plugin),
                         None, doc))

    def socket_filled (self, name):
        """Return True if socket is filled"""
        try:
            return self._sockets[name][1] is not None
        except KeyError:
            self.raise_exception("no such socket '%s'" % name, AttributeError)

    def remove_socket (self, name):
        """Remove an existing Socket"""
        # TODO: what about the property we've installed in the class?
        del self._sockets[name]

    def add_child(self, obj, private=False):
        """Update dependency graph and call base class add_child"""
        super(Assembly, self).add_child(obj)
        if IComponent.providedBy(obj):
            # This is too early to get accurate Variable info from 
            # the child since it's __init__ function may not be complete
            # yet (in the case of auto-registration by the Container base class),
            # so just put a None entry in the _child_io_graphs dict and fill
            # it in later
            self._child_io_graphs[obj.name] = None
            self._need_child_io_update = True
            self.get_comp_graph().add_node(obj.name)
        return obj
    
    def get_var_graph(self):
        """Returns the Variable dependency graph, after updating it with child
        info if necessary.
        """
        if self._need_child_io_update:
            self._update_child_io_graph_info()
        return self._var_graph
    
    def get_comp_graph(self):
        """Return our component graph, creating it from our var graph if necessary."""
        if self._comp_graph is None:
            self._comp_graph = nx.DiGraph()
            for varname,var in self._dep_graph.nodes(data=True):
                if var.parent is not self:
                    self._comp_graph.add_node(var.parent.name)
             
            seen = set()
            for u,v in self._dep_graph.edges():
                uparts = u.split('.')
                vparts = v.split('.')
                if uparts[0] != vparts[0] and len(uparts) > 1 and len(vparts) > 1:
                    key = ' '.join([uparts[0],vparts[0]])
                    if key not in seen:
                        self._comp_graph.add_edge(uparts[0],vparts[0])
                        seen.add(key)
                    
        return self._comp_graph
    
    def _update_child_io_graph_info(self):
        for childname in [name for name,val in self._child_io_graphs.items() if val is None]:
            graph = getattr(self, childname).get_io_graph()
            self._child_io_graphs[childname] = graph.copy()
            self._var_graph.add_nodes_from(graph.nodes_iter(data=True))
            self._var_graph.add_edges_from(graph.edges())
        self._need_child_io_update = False
            
    def remove_child(self, name):
        """Remove the named object from this container and notify any 
        observers.
        """
        if '.' in name:
            self.raise_exception('remove_child does not allow dotted path names like %s' %
                                 name, ValueError)        
        obj = self.get(name)
        if IComponent.providedBy(obj):
            self.get_comp_graph().remove_node(obj.name)
            if name in self._child_io_graphs:
                childgraph = self._child_io_graphs[name]
                if childgraph is not None:
                    self._var_graph.remove_nodes_from(childgraph)
                del self._child_io_graphs[name]
            
        if name in self._sockets:
            setattr(self, name, None)
            # set delete to False, otherwise delattr will fail because
            # named object is a property
            super(Assembly, self).remove_child(name, delete=False)
        else:
            super(Assembly, self).remove_child(name)

    def create_passthru(self, varname, alias=None):
        """Create a Variable that's a copy of var, make it a public member of self,
        and create a passthru connection between it and var.  If alias is not None,
        the name of the 'promoted' Variable will be the alias.
        """
        # varname must have two parts
        compname, vname = varname.split('.')
        
        comp = getattr(self, compname)
        
        # check to see if var is already connected
        if self.is_destination(varname):
            self.raise_exception('%s is already connected' % 
                                 varname, RuntimeError)        
        name = alias or vname
        
        # make sure name isn't a dotted path
        if '.' in name:
            self.raise_exception('%s must be a simple name, not a dotted path' %
                                 name)
            
        # check to see if a public Variable already exists with the given varname
        if self.contains(name):
            self.raise_exception('%s is already a public Variable' % name, 
                                 RuntimeError)        
        var = comp.getvar(vname)
        newvar = var.create_passthru(self, name=name)
        self.make_public(newvar)
        self._var_graph.add_node(newvar.name, data=newvar)
        
        # create the passthru connection 
        if var.iostatus == INPUT:
            self.connect(name, varname)
        elif var.iostatus == OUTPUT:
            self.connect(varname, name)
        else:
            self.raise_exception('unknown iostatus %s' % str(var.iostatus))
        
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
            var = self._pub[varname]
        else:
            comp = getattr(self, compname)
            if not IComponent.providedBy(comp):
                self.raise_exception('%s is not a Component' % compname, TypeError)
            if '.' in varname:
                self.raise_exception('%s must be a simple name, not a dotted path' %
                                     varname, NameError)
            var = comp.getvar(varname)
        if not IVariable.providedBy(var):
            self.raise_exception('%s is not a Variable' % varname, TypeError)
        return (compname, comp, varname, var)
    
    def connect(self, srcpath, destpath):
        """Connect one src Variable to one destination Variable. This could be
        a normal connection (output to input) or a passthru connection."""

        srccompname, srccomp, srcvarname, srcvar = self.split_varpath(srcpath)
        destcompname, destcomp, destvarname, destvar = self.split_varpath(destpath)
        
        if srccompname == destcompname:
            self.raise_exception('Cannot connect %s to %s. Both are on same component.' %
                                 (srcpath,destpath), RuntimeError)
        if srccomp is self or destcomp is self: # it's a passthru connection
            if srccomp is destcomp:
                self.raise_exception('Cannot connect "%s" to "%s" on same component' %
                                      (srcvarname, destvarname), RuntimeError)
        else: # it's not a passthru connection so must connect OUTPUT to INPUT
            if srcvar.iostatus != OUTPUT:
                self.raise_exception(srcvar.get_pathname()+
                                     ' must be an OUTPUT variable',
                                     RuntimeError)
            if destvar.iostatus != INPUT:
                self.raise_exception(destvar.get_pathname()+
                                     ' must be an INPUT variable',
                                     RuntimeError)        
        if self.is_destination(destpath):
            self.raise_exception(destpath+' is already connected',
                                 RuntimeError)                    
        # test compatability
        destvar.validate_var(srcvar)
        
        if self._need_child_io_update:
            self._update_child_io_graph_info() # make sure we have all of the child io graph data
        self._var_graph.add_edge(srcpath, destpath)
        self._logger.debug('adding edge %s --> %s' % (srcpath,destpath))
        strongly_connected = nx.algorithms.traversal.strongly_connected_components(self._var_graph)
        for strcon in strongly_connected:
            if len(strcon) > 1:
                self._var_graph.remove_edge(srcpath, destpath)
                self.raise_exception('Circular dependency (%s) would be created by connecting %s to %s' %
                                     (str(strcon), srcpath, destpath), RuntimeError) 
                
        if destcomp is not self and srccomp is not self: # neither var is on boundary
            self.get_comp_graph().add_edge(srccompname, destcompname)
            
        # invalidate destvar if necessary
        if destcomp is self and destvar.iostatus == OUTPUT: # boundary output
            if destvar.valid is True and srcvar.valid is False:
                self._logger.debug('(connect) invalidating %s' % destvar.get_pathname())
            destvar.valid = srcvar.valid
                
            #if destvar.valid is False and self.parent:
                ## tell the parent that anyone connected to our boundary output 
                ## is invalid.
                ## Note that it's a dest var in this scope, but a src var in the 
                ## parent scope.
                #self.parent.invalidate_deps([destvar])
        elif srccomp is self and srcvar.iostatus == INPUT: # passthru input
            if srcvar.valid is True and destvar.valid is False:
                self._logger.debug('(connect) invalidating %s' % srcvar.get_pathname())
            srcvar.valid = destvar.valid
        else:
            self._logger.debug('(connect) invalidating %s' % destvar.get_pathname())
            destvar.valid = False
            self.invalidate_deps([destvar])
            
        self._io_graph = None

    def disconnect(self, varpath, varpath2=None):
        """Remove all connections to/from a given variable in the current scope. 
        This does not remove connections to boundary Variables from the parent scope.
        """
        if self._need_child_io_update:
            self._update_child_io_graph_info() # make sure we have all of the child io graph data
        
        if varpath2 is not None:
            self._var_graph.remove_edge(varpath, varpath2)
        else:  # remove all connections from the Variable
            graph = self._var_graph
            edges = [(v, varpath) for v in graph.pred[varpath].keys()]
            edges.extend([(varpath, v) for v in graph.succ[varpath].keys()])
            graph.remove_edges_from(edges)
        self._io_graph = None  # the io graph has changed, so have to remake it
        self._comp_graph = None # remake the component graph too.

    def is_destination(self, varpath):
        """Return True if the Variable specified by varname is a destination according
        to our graph. This means that either it's an input connected to an output, or 
        it's the destination part of a passtru connection.
        """
        if self._need_child_io_update:
            self._update_child_io_graph_info() # make sure we have all of the child io graph data
        if '.' in varpath:
            var = self._var_graph.label.get(varpath, None)
            return var is not None and var.iostatus == INPUT and (len(self._var_graph.pred[varpath]) > 0)
            
        return len(self._var_graph.pred[varpath]) > 0

    def execute (self):
        """By default, run child components in data flow order."""
        self._dataflow.run()
    
    def list_connections(self, show_passthru=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        graph = self._var_graph
        for outname, inname in graph.edges():
            outvar = graph.label[outname]
            invar = graph.label[inname]
            if outvar.parent is self or invar.parent is self:
                if show_passthru:
                    conns.append((outname, inname))
            elif outvar.iostatus == OUTPUT:
                    conns.append((outname, inname))
        return conns
    
    def update_inputs(self, varnames):
        """Transfer input data to the specified component.
        Note that we're called after incomp has set its execution directory,
        so we'll need to account for this during file transfers."""
        
        for vname in varnames:
            preds = self._var_graph.pred.get(vname, None)
            var = self._var_graph.label[vname]
            if preds: # if var has a source
                for srcname in preds.keys():
                    srcvar = self._var_graph.label[srcname]
                    if not srcvar.valid:
                        # need to backtrack to get a valid source value
                        if srcvar.parent is self: # a boundary var
                            if self.parent and IAssembly.providedBy(self.parent):
                                self.parent.update_inputs(['.'.join([self.name,srcvar.name])])
                            else:
                                self.raise_exception(
                                    'invalid source Variable found (%s), with no way to update it' 
                                    % srcname, RuntimeError)
                        else: # a non-boundary var
                            self._logger.debug('**update_inputs causing %s to run'%srcvar.parent.get_pathname())
                            srcvar.parent.run()
                    incomp = var.parent
                    if isinstance(var, FileVariable):
                        incomp.pop_dir()
                        try:
                            self.xfer_file(srcvar.parent, srcvar, incomp, var)
                            var.metadata = srcvar.metadata.copy()
                        except Exception, exc:
                            msg = "cannot transfer file from '%s' to '%s': %s" % \
                                  ('.'.join((src, srcvar.name)),
                                   '.'.join((dest, var.name)), str(exc))
                            self.raise_exception(msg, type(exc))
                        finally:
                            incomp.push_dir(incomp.get_directory())
                    else:
                        try:
                            var.setvar(None, srcvar)
                        except Exception, exc:
                            msg = "cannot set '%s' from '%s': %s" % \
                                  ('.'.join((incomp.name, var.name)),
                                   srcvar, str(exc))
                            self.raise_exception(msg, type(exc))
            self._logger.debug('(update_inputs) validating %s' % var.get_pathname())
            var.valid = True

    def check_config (self):
        """Verify that the configuration of this component is correct. This function is
        called once prior to the first execution of this Assembly, and prior to execution
        if any children are added or removed, or if self._need_check_config is True.
        """
        super(Assembly, self).check_config()
        for name,sock in self._sockets.items():
            iface, current, required = sock
            if current is None and required is True:
                self.raise_exception("required plugin '%s' is not present" % name,
                                     ValueError)

    #def var_preds(self, vars):
        #"""Return a set of Variables on our incoming boundary that are connected
        #to the given Variables on the outgoing boundary. All entries in vars must be 
        #children of this object and they must be destinations according to this object's 
        #graph.  vars must be an iterable.
        #"""
        #if self._need_child_io_update:
            #self._update_child_io_graph_info() # make sure we have all of the child io graph data
        #varnames = [v.get_pathname(rel_to_scope=self) for v in vars]
        #pred = self._var_graph.pred   #predecessor nodes in the graph
        #stack = set(varnames)
        #pred_vars = set()
        #while len(stack) > 0:
            #name = stack.pop()
            #for vname in pred[name]:
                #if '.' not in vname:
                    #pred_vars.add(vname)
                #stack.add(vname)
        #return pred_vars
    
    #def var_successors(self, vars):
        #"""Return a list of OUTPUT boundary Variables that are successors to the 
        #INPUT Variables given.
        #"""
        #if self._need_child_io_update:
            #self._update_child_io_graph_info() # make sure we have all of the child io graph data
        #varnames = [v.get_pathname(rel_to_scope=self) for v in vars]
        #succ = self._var_graph.succ  #successor nodes in the graph
        #stack = set(varnames)
        #succ_vars = set()
        #while len(stack) > 0:
            #name = stack.pop()
            #for vname in succ[name]:
                #if '.' not in vname:
                    #succ_vars.add(vname)
                #stack.add(vname)
        #return succ_vars
        
    def invalidate_deps(self, vars):
        """Mark all Variables invalid that depend on vars."""
        if self._need_child_io_update:
            self._update_child_io_graph_info() # make sure we have all of the child io graph data
        varnames = [v.get_pathname(rel_to_scope=self) for v in vars]
        succ = self._var_graph.succ  #successor nodes in the graph
        label = self._var_graph.label # node data
        stack = set(varnames)
        while len(stack) > 0:
            name = stack.pop()
            for vname in succ[name]:
                var = label[vname]
                if var.valid is True:
                    self._logger.debug('(invalidate_deps) invalidating %s' % var.get_pathname())
                    var.valid = False
                    stack.add(vname)

    def get_io_graph(self):
        """Return a graph connecting our input variables to our output variables.
        """
        # for now, just use the dumb Component version, which says that all inputs
        # map to all outputs.
        # TODO: make this truly reflect the IO relationships in the assembly so we
        #       won't run any children unnecessarily
        return super(Assembly, self).get_io_graph()
    
    @staticmethod
    def xfer_file(src_comp, src_var, dst_comp, dst_var):
        """ Transfer src_comp.src_ref file to dst_comp.dst_ref file. """
        src_path = os.path.join(src_comp.get_directory(), src_var.get_value())
        dst_path = os.path.join(dst_comp.get_directory(), dst_var.get_value())
        if src_path != dst_path:
            if src_var.metadata['binary']:
                mode = 'b'
            else:
                mode = ''
            filexfer(None, src_path, None, dst_path, mode)

