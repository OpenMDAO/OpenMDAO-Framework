
#public symbols
__all__ = ['Assembly']

__version__ = "0.1"

import os
import os.path
import copy
from itertools import chain

from zope.interface import implements
import networkx as nx

from openmdao.main.interfaces import IAssembly, IComponent, IVariable
from openmdao.main import Container, Component, String
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


class Assembly (Component):
    """This is the base class for all objects containing Variables that are 
       accessible to the OpenMDAO framework and are 'runnable'.
    """

    implements(IAssembly)
    
    def __init__(self, name, parent=None, doc=None, directory=''):
        super(Assembly, self).__init__(name, parent, doc)
        
        self.state = STATE_IDLE
        self._stop = False
        self._input_changed = True
        self._dir_stack = []
        self._sockets = {}
        
        # a graph of Components, with connections between Variables
        # as directed edges.  MultiDiGraph supports multiple edges
        # between the same two nodes, so each connection between
        # two Variables is a separate edge.
        # Each edge has data of the form (srcvar,destvar) and a
        # key of the form (srcvarname, destvarname). The key
        # is used to differentiate between edges that connect the
        # same two nodes.
        self._dep_graph = nx.MultiDiGraph(name=name)

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
            iface, current, required = self._sockets[name]
        except KeyError:
            self.raise_exception("no such socket '%s'" % name, AttributeError)
        else:
            if plugin is not None and iface is not None:
                if not iface.providedBy(plugin):
                    self.raise_exception("plugin does not support '%s'" % \
                                         iface.__name__, ValueError)
            self._sockets[name] = (iface, plugin, required)

    def add_socket (self, name, iface, doc='', required=True):
        """Specify a named placeholder for a component with the given
        interface or prototype.
        """
        assert isinstance(name, basestring)
        self._sockets[name] = (iface, None, required)
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
        del self._sockets[name]

    def add_child(self, obj, private=False):
        """Update Compnent graph and call base class add_child"""
        super(Assembly, self).add_child(obj)
        if IComponent.providedBy(obj):
            self._dep_graph.add_node(obj.name)
        
    def remove_child(self, name):
        """Remove the named object from this container and notify any 
        observers.
        """
        if '.' in name:
            self.raise_exception('remove_child does not allow dotted path names like %s' %
                                 name, ValueError)
        
        obj = self.get(name)
        if IComponent.providedBy(obj):
            self._dep_graph.remove_node(name)
            
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
        
        # create the passthru connection 
        if var.iostatus is INPUT:
            self.connect(name, varname)
        elif var.iostatus is OUTPUT:
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
        
        # update the graph
        # NOTE: We can't use the same name, for example '', for passthru sources and destinations
        # because they'll show up as circular dependencies, so instead we're using #dest and #src
        # to indicate passthru variables at the boundary of the current scope.
        self._dep_graph.add_edge(srccompname or '#src', destcompname or '#dest', data=(srcvar, destvar),
                                 key=(srcvarname, destvarname))
        strongly_connected = nx.algorithms.traversal.strongly_connected_components(self._dep_graph)
        for strcon in strongly_connected:
            if len(strcon) > 1:
                self._dep_graph.remove_edge(srccompname or '#src', destcompname or '#dest', 
                                            key=(srcvarname, destvarname))
                self.raise_exception('Circular dependency (%s) would be created by connecting %s to %s' %
                                     (str(strcon), srcpath, destpath), RuntimeError)    
                
        # invalidate destvar if necessary
        if destcomp is self and destvar.iostatus == OUTPUT: # passthru output
            destvar.valid = srcvar.valid
            if destvar.valid is False and self.parent:
                # tell the parent that anyone connected to our boundary output is invalid.
                # Note that it's a dest var in this scope, but a src var in the parent scope.
                dests = self.parent.find_destinations(destvar)
                if len(dests) > 0:
                    self.parent.invalidate_deps(dests)
        else:
            destvar.valid = False
            self.invalidate_deps([destvar])

    def disconnect(self, varpath):
        """Remove all connections to/from a given variable in the current scope. 
        This does not remove connections to boundary Variables from the parent scope.
        """
        try:
            srcname, varname = varpath.split('.')
            destname = srcname
        except ValueError:
            if '.' in varpath: # this means we have at least 2 dots
                self.raise_exception('%s must be a simple name, not a dotted path' %
                                     varpath.split('.',1)[1], NameError)
            srcname = '#src'
            destname = '#dest'
            
        to_remove = []
        graph = self._dep_graph
        # loop over outgoing edges of the variable's parent component
        for srccompname,destcompname,key in graph.edges(srcname, keys=True):
            if varname == key[0]: # it's a source
                to_remove.append((srccompname,destcompname,key))
        # loop over incoming edges
        for srccompname,destcompname,key in graph.in_edges_iter(destname, keys=True):
            if varname == key[1]: # it's a destination
                to_remove.append((srccompname,destcompname,key))
                           
        if len(to_remove) == 0:
            self.raise_exception('%s is not connected' % varpath, RuntimeError)
        else:
            graph.remove_edges_from(to_remove)

    def find_destinations(self, var):
        """For the given source, return a list of destination Variables."""
        dests = []
        if var.parent == self:
            for src,dest,data in self._dep_graph.in_edges_iter('#src', data=True):
                if var == data[0]:
                    dests.append(data[1])
        else:
            for src,dest,data in self._dep_graph.in_edges_iter(var.parent.name, data=True):
                if var == data[0]:
                    dests.append(data[1])
        return dests
        
        
    def is_destination(self, varpath):
        """Return True if the Variable specified by varname is a destination. This means
        that either it's an input connected to an output, or it's the destination part of
        a passtru connection.
        """
        try:
            compname, varname = varpath.split('.')
        except ValueError:
            if '.' in varpath: # this means we have at least 2 dots
                self.raise_exception('%s must be a simple name, not a dotted path' %
                                     varpath.split('.',1)[1], NameError)

            for src,dest,key in self._dep_graph.in_edges_iter('#dest', keys=True):
                if varpath == key[1]:
                    return True
            
            if self.parent is None:
                return False
            else: # check to see if this input var on our boundary is a destination in 
                  # the parent scope
                return self.parent.is_destination('.'.join([self.name, varpath]))
            
        
        # look at incoming connections to the var's parent Component, and if
        # var is on the destination side of any of them, it's a destination
        for src,dest,key in self._dep_graph.in_edges_iter(compname, keys=True):
            if varname == key[1]:
                return True
        return False

    def execute (self):
        """Run child components in data flow order."""
        for comp in self.get_component_iterator():
            comp.run()
    
    def get_component_iterator(self, compnames=None):
        """Return a dataflow ordered iterator over the set of Components 
        specified by compnames, or over all of our child Components if 
        compnames is None.
        """
        for compname in nx.topological_sort(self._dep_graph):
            if (compnames is None or compname in compnames) and not compname.startswith('#'):
                yield getattr(self, compname)
        
    def list_connections(self, show_passthru=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        for srccomp,destcomp,key in self._dep_graph.edges(keys=True):
            if srccomp.startswith('#'): srccomp = ''
            if destcomp.startswith('#'): destcomp = ''
            if show_passthru is True or (srccomp != '' and destcomp != ''):
                conns.append(('.'.join([srccomp,key[0]]), '.'.join([destcomp,key[1]])))
        return conns
    
    def update_inputs(self, incomp):
        """Transfer input data to the specified component.
        Note that we're called after incomp has set its execution directory,
        so we'll need to account for this during file transfers."""
        
        for src,dest,data in self._dep_graph.in_edges_iter(incomp.name,
                                                           data=True):
            srcvar,var = data
            if var.valid:  # don't need to update valid inputs
                continue 
            # Variables at the scope boundary have their component specified as
            # either #dest or #src depending on whether they're an input or an output.
            # If we used the same name for their component, for example '', then
            # the graph would think it had a circular dependency.
            if src.startswith('#'):
                srccomp = self
            else:
                srccomp = self.get(src)
            if isinstance(var, FileVariable):
                incomp.pop_dir()
                try:
                    self.xfer_file(srccomp, srcvar, incomp, var)
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

    def get_invalidated_outputs(self, invars):
        return self.invalidate_deps(invars)
                
    def invalidate_deps(self, vars):
        """Mark all Variables that depend on vars as invalid. vars must be INPUT Variables
        and they must all have the same parent. Return a list of invalidated OUTPUT Variables
        based on the set of invalidated INPUTs.
        """
        # TODO: find a cleaner way to do this
        
        assert(len(vars) > 0)
        assert(vars[0].iostatus == INPUT)
        
        if vars[0].parent == self:
            pname = '#src'
        else:
            pname = vars[0].parent.name
        nodes = { pname: vars }      
        visited_nodes = set(['#dest']) #add '#dest' so we'll skip it
        
        my_outs = []
        while len(nodes) > 0:
            nodename, vars = nodes.popitem()
            if nodename == '#src':  # INPUT var on our boundary
                for src,dest,data in self._dep_graph.edges('#src', data=True):
                    srcvar,destvar = data
                    if destvar.valid == True and srcvar in vars: # only look at provided inputs
                        destvar.valid == False
                        cname = destvar.parent.name
                        if cname not in visited_nodes:
                            if cname not in nodes:
                                nodes[cname] = []
                            else:
                                nodes[cname].append(destvar)
            else:
                node = getattr(self, nodename)
                outs = node.get_invalidated_outputs(vars)
                for src,dest,data in self._dep_graph.edges(nodename, data=True):
                    srcvar,destvar = data
                    if srcvar.iostatus == INPUT or srcvar in outs: # source is newly marked
                        if destvar.valid == True:
                            destvar.valid = False  # invalidate the destination
                            comp = destvar.parent
                            if comp == self:   # destvar is an OUTPUT on our boundary
                                my_outs.append(destvar)
                                cname = '#dest'
                            else:
                                cname = comp.name
                            if cname not in visited_nodes:
                                if cname not in nodes:
                                    nodes[cname] = []
                                else:
                                    nodes[cname].append(destvar)
            visited_nodes.add(nodename)
            
        return my_outs


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

