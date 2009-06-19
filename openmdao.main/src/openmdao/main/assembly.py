
#public symbols
__all__ = ['Assembly']

__version__ = "0.1"

import os
import os.path
import copy
import inspect

from enthought.traits.api import implements, Str, List, Instance, TraitError
import networkx as nx
from networkx.algorithms.traversal import is_directed_acyclic_graph, strongly_connected_components

from openmdao.main.interfaces import IAssembly, IComponent, IDriver, IWorkflow
from openmdao.main.api import Container
from openmdao.main.component import Component, STATE_IDLE
from openmdao.main.dataflow import Dataflow
from openmdao.main.constants import SAVE_PICKLE
from openmdao.main.exceptions import CircularDependencyError
from openmdao.main.util import filexfer
from openmdao.main.filevar import FileVariable

        
class Assembly (Component):
    """This is a container of Components. It understands how
    to connect inputs and outputs between its children 
    and how to handle Sockets.
    """
    implements(IAssembly)
    
    drivers = List(IDriver)
    workflow = Instance(IWorkflow)
    
    def __init__(self, name=None, parent=None, doc=None, directory='',
                       workflow=None):
        self.state = STATE_IDLE
        self._stop = False
        self._dir_stack = []
        self._child_io_graphs = {}
        self._need_child_io_update = True
        
        # A hybrid graph of Variable names (local path) and component names, 
        # with connections between Variables/Components as directed edges.  
        # Children are queried for dependencies between their inputs and outputs 
        # so they can also be represented in the graph. 
        #
        # Below is an example of what a _var_graph might look like. There
        # are two boundary variables (in & out), and two components (c1 & c2).
        # c1 has two outputs (out1 & out2)
        #
        # c1.in ---> c1 ---> c1.out1
        #   /\        |
        #   |         |
        #   |         \/  
        #  in      c1.out2               out
        #             |                  /\
        #             |                  |
        #             \/                 |
        #           c2.in ---> c2 ---> c2.out
        #
        self._var_graph = nx.DiGraph()
        
        super(Assembly, self).__init__(name, parent, doc=doc,
                                       directory=directory)
        
        # add any Variables we may have inherited from our base classes
        # to our _var_graph..
        for v in self.keys(iostatus=lambda x: x is not None):
            if v not in self._var_graph:
                self._var_graph.add_node(v)
        
        self._dataflow = Dataflow('dataflow', self)
        
        if workflow is None:
            workflow = self._dataflow

        self.workflow = workflow

        # List of meta-data dictionaries.
        self.external_files = []

    def get_component_graph(self):
        return self._dataflow.get_graph()
    
    def get_var_graph(self):
        """Returns the Variable dependency graph, after updating it with child
        info if necessary.
        """
        if self._need_child_io_update:
            vargraph = self._var_graph
            childiographs = self._child_io_graphs
            for childname in [name for name,val in childiographs.items() if val is None]:
                graph = getattr(self, childname).get_io_graph()
                childiographs[childname] = graph.copy()
                vargraph.add_nodes_from(graph.nodes_iter())
                vargraph.add_edges_from(graph.edges_iter())
            self._need_child_io_update = False
        return self._var_graph
        
    def get_io_graph(self):
        """For now, just return our base class version of get_io_graph."""
        # TODO: make this return an actual graph of inputs to outputs based on 
        #       the contents of this Assembly instead of a graph where all outputs
        #       depend on all inputs
        return super(Assembly, self).get_io_graph()
    
    def add_child(self, obj):
        """Update dependency graph and call base class add_child."""
        super(Assembly, self).add_child(obj)
        if isinstance(obj, Component):
            # This is too early to get accurate Variable info from 
            # the child since it's __init__ function may not be complete
            # yet (in the case of auto-registration by the Container base class),
            # so just put a None entry in the _child_io_graphs dict and fill
            # it in later
            self._child_io_graphs[obj.name] = None
            self._need_child_io_update = True
            self._dataflow.add_node(obj.name)
        try:
            self.drivers.append(obj)  # will fail if it's not an IDriver
        except TraitError:
            pass
        return obj
        
    def remove_child(self, name):
        """Remove the named object from this container and notify any observers.
        """
        if '.' in name:
            self.raise_exception('remove_child does not allow dotted path names like %s' %
                                 name, ValueError)        
        obj = self.get(name)
        if isinstance(obj, Component):
            self._dataflow.remove_node(obj.name)
            if name in self._child_io_graphs:
                childgraph = self._child_io_graphs[name]
                if childgraph is not None:
                    self._var_graph.remove_nodes_from(childgraph)
                del self._child_io_graphs[name]
        
        if obj in self.drivers:
            self.drivers.remove(obj)
            
        for drv in self.drivers:
            drv.graph_regen_needed()

    def create_passthru(self, varname, alias=None):
        """Create a Variable that's a copy of var, make it a public member of self,
        and create a passthru connection between it and var.  If alias is not None,
        the name of the 'promoted' Variable will be the alias.
        """
        # check to see if var is already connected
        if self.is_destination(varname):
            self.raise_exception('%s is already connected' % 
                                 varname, RuntimeError) 

        # varname must have two parts
        compname, vname = varname.split('.')
        comp = getattr(self, compname)
        name = alias or vname
        
        # make sure name isn't a dotted path
        if '.' in name:
            self.raise_exception('%s must be a simple name, not a dotted path' %
                                 name)
            
        # check to see if a public Variable already exists with the given varname
        if self.contains(name):
            self.raise_exception('%s is already a public Variable' % name, 
                                 RuntimeError) 
        
        # create the passthru connection 
        iostatus = comp.trait(vname).iostatus
        self.add_trait(name, comp.trait(vname))
        if iostatus == 'in':
            self.connect(name, varname)
        elif iostatus == 'out':
            self.connect(varname, name)
        else:
            self.raise_exception('unknown iostatus %s' % iostatus)
        
    def split_varpath(self, path):
        """Return a tuple of compname,component,varname,trait given a path
        name of the form 'compname.varname'. If the name is of the form 'varname'
        then compname will be None and comp is self.
        """
        try:
            compname, varname = path.split('.', 1)
        except ValueError:
            return (None, self, path, self.trait(path))
        
        if '.' in varname:
            self.raise_exception('%s must be a simple name, not a dotted path' %
                                 varname, NameError)
        comp = getattr(self, compname)
        return (compname, comp, varname, comp.trait(varname))
    
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
        else: # it's not a passthru connection so must connect output to input
            if srcvar.iostatus != 'out':
                self.raise_exception(srcvar.get_pathname()+
                                     ' must be an output variable',
                                     RuntimeError)
            if destvar.iostatus != 'in':
                self.raise_exception(destvar.get_pathname()+
                                     ' must be an input variable',
                                     RuntimeError)        
        if self.is_destination(destpath):
            self.raise_exception(destpath+' is already connected',
                                 RuntimeError)                    
        # test compatability
        destvar.validate(destcomp, destvarname, getattr(srccomp, srcvarname))
        
        if destcomp is not self and srccomp is not self: # neither var is on boundary
            self._dataflow.connect(srccompname, destcompname, srcvarname, destvarname)
        
        vgraph = self.get_var_graph()
        vgraph.add_edge(srcpath, destpath)
            
        # invalidate destvar if necessary
        if destcomp is self and destvar.iostatus == 'out': # boundary output
            if destcomp.get_valid(destvarname) and srccomp.get_valid(srcvarname) is False:
                if self.parent:
                    # tell the parent that anyone connected to our boundary output 
                    # is invalid.
                    # Note that it's a dest var in this scope, but a src var in the 
                    # parent scope.
                    self.parent.invalidate_deps([destpath], True)
            self.set_valid(destpath, False)
        elif srccomp is self and srcvar.iostatus == 'in': # boundary input
            self.set_valid(srcpath, False)
        else:
            destcomp.set_valid(destvarname, False)
            self.invalidate_deps([destpath])
        
        self._io_graph = None

        for drv in self.drivers:
            drv.graph_regen_needed()

    def disconnect(self, varpath, varpath2=None):
        """Remove all connections to/from a given variable in the current scope. 
        This does not remove connections to boundary Variables from the parent scope.
        """
        vargraph = self.get_var_graph()
        if self.trait(varpath).iostatus is None:
            self.raise_exception("'%s' is not a linkable attribute" %
                                 varpath, RuntimeError)
        if varpath2 is not None:
            if '.' in varpath and '.' in varpath2:
                self._dataflow.disconnect(varpath.split('.')[0], 
                                          varpath2.split('.')[0])
            elif self.trait(varpath2).iostatus is None:
                self.raise_exception("'%s' is not a linkable attribute" %
                                     varpath2, RuntimeError)
            if varpath2 in vargraph[varpath]:
                vargraph.remove_edge(varpath, varpath2)
            elif varpath not in vargraph[varpath2]:
                vargraph.remove_edge(varpath2, varpath)
            else:
                self.raise_exception('%s is not connected to %s' % 
                                     (varpath, varpath2), RuntimeError)
        else:  # remove all connections from the Variable
            # remove outgoing edges
            to_remove = []
            for u,v in vargraph.edges_iter(varpath):
                if not self._dataflow.has_node(v):
                    to_remove.append((u,v))
            # remove incoming edges
            for u,v in vargraph.in_edges_iter(varpath):
                if not self._dataflow.has_node(u):
                    to_remove.append((u,v))
                        
            for u,v in to_remove:
                if '.' not in u and '.' not in v:
                    self._dataflow.disconnect(u.split('.',1)[0], 
                                              v.split('.',1)[0])
                
            vargraph.remove_edges_from(to_remove)
                
        self._io_graph = None  # the io graph has changed, so have to remake it
        for drv in self.drivers:
            drv.graph_regen_needed()


    def is_destination(self, varpath):
        """Return True if the Variable specified by varname is a destination according
        to our graph. This means that either it's an input connected to an output, or 
        it's the destination part of a passtru connection.
        """
        try:
            iostatus = self.trait(varpath).iostatus
        except:
            return False
        return iostatus is not None and len(self._var_graph.pred.get(varpath,'')) > 0

    def execute (self):
        """By default, run child components in data flow order."""
        self._dataflow.run()
        
    def step(self):
        """Execute a single child component and return."""
        self._dataflow.step()
    
    def list_connections(self, show_passthru=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        graph = self.get_var_graph()
        for outname, inname in graph.edges_iter():
            # only list variable to variable connections, not component to variable
            if self._dataflow.has_node(outname) or self._dataflow.has_node(inname):
                continue
            if '.' in outname or '.' in inname:
                if show_passthru:
                    conns.append((outname, inname))
            else:
                conns.append((outname, inname))
        return conns

    def parent_of(self, name):
        splt = name.split('.',1)
        if len(splt) > 1:
            return getattr(self, splt[0])
        else:
            return this
        
    def parent_name_of(self, name):
        splt = name.split('.',1)
        if len(splt) > 1:
            return splt[0]
        else:
            return None
        
    def update_inputs(self, compname, varnames=None):
        """Transfer input data to input variables on the specified component.
        If varnames is not None, only the variables in the list will be updated.
        Note that we're called after incomp has set its execution directory,
        so we'll need to account for this during file transfers.
        """
        updated = False  # this becomes True if we actually update any inputs
        parent = self.parent
        vargraph = self.get_var_graph()
        pred = vargraph.pred
        comp = getattr(self, compname)
        
        if varnames is None:
            varnames = [n.split('.',1)[1] for n in pred.get(compname, [])]
        
        for vname in varnames:
            preds = pred.get('.'.join([compname, vname]), '')
            if len(preds) == 0: 
                continue
            elif len(preds) > 1:
                self.raise_exception("variable '%s' has multiple sources %s" %
                                     (vname, preds.keys()), RuntimeError)
                
            if comp.get_valid(vname) is False: # if var has a source and var is invalid
                updated = True
                srcname = preds.keys()[0]
                srccompname,srccomp,srcvarname,srctrait = self.split_varpath(srcname)
                
                if srccomp.get_valid(srcvarname) is False:  # source is invalid 
                    # need to backtrack to get a valid source value
                    if srccompname is None: # a boundary var
                        if parent:
                            parent.update_inputs(self.name, [srcname])
                        else:
                            srccomp.set_valid(srcvarname, True) # validate source
                    else:
                        srccomp.update_outputs([srcvarname])
                trait = self.trait(vname)
                if isinstance(trait, FileVariable):
                    if comp.directory:
                        comp.pop_dir()
                    try:
                        self.xfer_file(srccomp, srcvarname, comp, vname)
                        #var.metadata = srcvar.metadata.copy()
                    except Exception, exc:
                        msg = "cannot transfer file from '%s' to '%s': %s" % \
                              (srcname, vname, exc)
                        self.raise_exception(msg, type(exc))
                    finally:
                        if comp.directory:
                            comp.push_dir(comp.get_directory())
                else:
                    try:
                        setattr(comp, vname, getattr(srccomp, srcvarname))
                    except Exception, exc:
                        msg = "cannot set '%s' from '%s': %s" % \
                            (vname, srcname, exc)
                        self.raise_exception(msg, type(exc))
                comp.set_valid(vname, True)
            
        return updated

    def update_outputs(self, outnames):
        """Execute any necessary internal or predecessor components in order to
        make the specified output variables valid.
        """
        self.update_inputs(outnames)

    def check_config (self):
        """Verify that the configuration of this component is correct. This function is
        called once prior to the first execution of this Assembly, and prior to execution
        if any children are added or removed, or if self._need_check_config is True.
        """
        super(Assembly, self).check_config()
        #for name, tup in self._sockets.items():
            #sock, current = tup
            #if sock.required and current is None:
                #self.raise_exception("required plugin '%s' is not present" % name,
                                     #ValueError)                
        
    def invalidate_deps(self, varnames, notify_parent=False):
        """Mark all Variables invalid that depend on vars.
        Returns a list of our newly invalidated boundary outputs.
        """
        vargraph = self.get_var_graph()
        succ = vargraph.succ  #successor nodes in the graph
        stack = set(varnames)
        outs = []
        while len(stack) > 0:
            name = stack.pop()
            successors = succ.get(name, [])
            for vname in successors:
                tup = vname.split('.',1)
                if len(tup) == 1:  #boundary var or Component
                    trait = self.trait(vname)
                    if trait.iostatus == 'out': # an output boundary var
                        outs.append(vname)
                    elif trait.iostatus == None:  # a Component
                        pass
                else:  # a var from a child component 
                    compname, compvar = tup
                    comp = getattr(self, compname)
                    if comp.get_valid(compvar):  # node is a valid Variable
                        for newvar in comp.invalidate_deps([compvar]):
                            stack.add('.'.join([compname, newvar]))
                        stack.add(vname)
                        
        if notify_parent and self.parent and len(outs)>0:
            self.parent.invalidate_deps(outs, notify_parent)
        return outs

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

