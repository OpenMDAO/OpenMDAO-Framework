
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
from openmdao.main.tarjan import strongly_connected_components
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
        
        #if srccomp is not self and destcomp is not self:
        self._dep_graph.add_edge(srccompname or '', destcompname or '', data=(srcvar, destvar),
                                 key=(srcvarname, destvarname))
        strongly_connected = nx.algorithms.traversal.strongly_connected_components(self._dep_graph)
        for strcon in strongly_connected:
            if len(strcon) > 1:
                self._dep_graph.remove_edge(srccompname or '', destcompname or '', 
                                            key=(srcvarname, destvarname))
                self.raise_exception('Circular dependency (%s) would be created by connecting %s to %s' %
                                     (str(strcon), srcpath, destpath), RuntimeError)       
            
    def disconnect(self, varpath):
        """Remove all connections from a given variable."""
        compname, comp, varname, var = self.split_varpath(varpath)
        compname,varname = varpath.split('.')
        to_remove = []
        graph = self._dep_graph
        # loop over output and input edges of the variable's parent component
        for srccompname,destcompname,key in chain(graph.edges(compname, keys=True),
                                                  graph.in_edges_iter(compname, keys=True)):
            srcvarname,destvarname = key
            if varname == srcvarname or varname == destvarname:
                to_remove.append((srccompname,destcompname,key))
                
        for rem in to_remove:
            graph.remove_edge(rem[0],rem[1],key=rem[2])
            
        if len(to_remove) == 0:
            self.raise_exception('%s is not connected' % varpath, RuntimeError)

    def is_destination(self, varpath):
        """Return True if the Variable specified by varname is a destination. This means
        that either it's an input connected to an output, or it's the destination part of
        a passtru connection.
        """
        compname, comp, varname, var = self.split_varpath(varpath)
        if compname is None:
            compname = ''
        for src,dest,key in self._dep_graph.in_edges_iter(compname, keys=True):
            if varname == key[1]:
                return True
        if compname == '' and self.parent is not None: # need to check if we're connected from higher scope
            return self.parent.is_destination('.'.join([self.name,varname]))
        return False
    
    def list_connections(self, show_passthru=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        for srccomp,destcomp,key in self._dep_graph.edges(keys=True):
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
            if src == '':
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
                          (var.source,
                           '.'.join((incomp.name, var.name)), str(exc))
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
            
        #vars = [x[1] for x in incomp.items(recurse=False) if
                               #IVariable.providedBy(x[1]) and x[1].is_destination()]
        
        #for var in vars:
            #srccompname, srccomp, srcvarname, srcvar = self.split_varpath(var.source)
            #if isinstance(var, FileVariable):
                #incomp.pop_dir()
                #try:
                    #self.xfer_file(srccomp, srcvar, incomp, var)
                    #var.metadata = srcvar.metadata.copy()
                #except Exception, exc:
                    #msg = "cannot transfer file from '%s' to '%s': %s" % \
                          #(var.source,
                           #'.'.join((incomp.name, var.name)), str(exc))
                    #self.raise_exception(msg, type(exc))
                #finally:
                    #incomp.push_dir(incomp.get_directory())
            #else:
                #srcvar = self.getvar(var.source)
                #try:
                    #var.setvar(None, srcvar)
                #except Exception, exc:
                    #msg = "cannot set '%s' from '%s': %s" % \
                          #('.'.join((incomp.name, var.name)),
                           #srcvar, str(exc))
                    #self.raise_exception(msg, type(exc))

    def check_config (self):
        """Verify that the configuration of this component is correct. This function is
        called once prior to the first execution of this Assembly, and prior to execution
        if any children are added or removed, or if self._need_check_config is True.
        """
        for name,sock in self._sockets.items():
            iface, current, required = sock
            if current is None and required is True:
                self.raise_exception("required plugin '%s' is not present" % name,
                                     ValueError)

    def get_workflow(self, name=None):
        """Return a Workflow object that will execute whatever Components are necessary
        to make the specified name valid. The specified name can be an output Variable
        or a Component. If name is None, then all child Components will be executed in
        data flow order.
        """
        workflow = self._workflows.get(name)
        if workflow is not None:
            return workflow
        else: # calculate a new Workflow for name
            return DataFlow(self, name)
                    
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

