
#public symbols
__all__ = ['Assembly']


from enthought.traits.api import List, Instance, TraitError
from enthought.traits.api import TraitType, Undefined
from enthought.traits.trait_base import not_none

import networkx as nx

from openmdao.main.interfaces import IDriver
from openmdao.main.component import Component
from openmdao.main.container import Container
from openmdao.main.workflow import Workflow
from openmdao.main.dataflow import Dataflow

def _filter_internal_edges(edges):
    """Return a copy of the given list of edges with edges removed that are
    connecting two variables on the same component.
    """
    return [(u,v) for u,v in edges
                          if u.split('.', 1)[0] != v.split('.', 1)[0]]


def _path_exists(G, source, target):
    """Returns True if there is a path from source to target."""
    # does BFS from both source and target and meets in the middle
    if source is None or target is None:
        raise nx.NetworkXException(\
            "Bidirectional shortest path called without source or target")
    if target == source:  return source in G.predecessors(source)

    Gpred=G.predecessors_iter
    Gsucc=G.successors_iter

    # predecesssor and successors in search
    pred={source:None}
    succ={target:None}

    # initialize fringes, start with forward
    forward_fringe=[source] 
    reverse_fringe=[target]  

    while forward_fringe and reverse_fringe:
        this_level=forward_fringe
        forward_fringe=[]
        for v in this_level:
            for w in Gsucc(v):
                if w not in pred:
                    forward_fringe.append(w)
                    pred[w]=v
                if w in succ:  return True # found path
        this_level=reverse_fringe
        reverse_fringe=[]
        for v in this_level:
            for w in Gpred(v):
                if w not in succ:
                    succ[w]=v
                    reverse_fringe.append(w)
                if w in pred:  return True # found path

    return False  # no path found


class PassthroughTrait(TraitType):
    """A trait that can use another trait for validation, but otherwise is
    just a trait that lives on an Assembly boundary and can be connected
    to other traits within the Assembly.
    """
    
    def validate(self, obj, name, value):
        if self.validation_trait:
            return self.validation_trait.validate(obj, name, value)
        return value

class Assembly (Component):
    """This is a container of Components. It understands how to connect inputs
    and outputs between its children.  When executed, it runs the components
    in its Workflow.
    """
    drivers = List(IDriver)
    workflow = Instance(Workflow)
    
    def __init__(self, doc=None, directory=''):
        self._child_io_graphs = {}
        self._need_child_io_update = True
        
        # A graph of Variable names (local path), 
        # with connections between Variables as directed edges.  
        # Children are queried for dependencies between their inputs and outputs
        # so they can also be represented in the graph. 
        self._var_graph = nx.DiGraph()
        
        super(Assembly, self).__init__(doc=doc, directory=directory)
        
        # add any Variables we may have inherited from our base classes
        # to our _var_graph..
        for v in self.keys(iotype=not_none):
            if v not in self._var_graph:
                self._var_graph.add_node(v)
        
        self.workflow = Dataflow(scope=self)

    def get_component_graph(self):
        """Retrieve the dataflow graph of child components."""
        return self.workflow.get_graph()
    
    def get_var_graph(self):
        """Returns the Variable dependency graph, after updating it with child
        info if necessary.
        """
        if self._need_child_io_update:
            vargraph = self._var_graph
            childiographs = self._child_io_graphs
            for childname,val in childiographs.items():
                graph = getattr(self, childname).get_io_graph()
                if graph is not val:  # child io graph has changed
                    if val is not None:  # remove old stuff
                        vargraph.remove_nodes_from(val)
                    childiographs[childname] = graph
                    vargraph.add_nodes_from(graph.nodes_iter())
                    vargraph.add_edges_from(graph.edges_iter())
            self._need_child_io_update = False
        return self._var_graph
        
    #def get_io_graph(self):
        #"""For now, just return our base class version of get_io_graph."""
        ## TODO: make this return an actual graph of inputs to outputs based on 
        ##       the contents of this Assembly instead of a graph where all 
        ##       outputs depend on all inputs
        ## NOTE: if the io_graph changes, this function must return a NEW graph
        ## object instead of modifying the old one, because object identity
        ## is used in the parent assembly to determine of the graph has changed
        #return super(Assembly, self).get_io_graph()
    
    def add_container(self, name, obj, add_to_workflow=True):
        """Update dependency graph and call base class *add_container*.
        Returns the added Container object.
        """
        obj = super(Assembly, self).add_container(name, obj)
        try:
            self.drivers.append(obj)  # will fail if it's not an IDriver
        except TraitError:  # not an IDriver
            if isinstance(obj, Component):
                # since the internals of the given Component can change after it's
                # added to us, wait to collect its io_graph until we need it
                self._child_io_graphs[obj.name] = None
                self._need_child_io_update = True
                if add_to_workflow:
                    self.workflow.add_node(obj.name)
        return obj
        
    def remove_container(self, name):
        """Remove the named container object from this container."""
        trait = self.trait(name)
        if trait is not None:
            obj = getattr(self, name)
            if obj in self.drivers:
                self.drivers.remove(obj)
            else:
                # if the named object is a Component, then assume it must
                # be removed from our workflow
                if isinstance(obj, Component):
                    self.workflow.remove_node(obj.name)
                    
                    if name in self._child_io_graphs:
                        childgraph = self._child_io_graphs[name]
                        if childgraph is not None:
                            self._var_graph.remove_nodes_from(childgraph)
                        del self._child_io_graphs[name]
                    
        return super(Assembly, self).remove_container(name)


    def create_passthrough(self, pathname, alias=None):
        """Creates a PassthroughTrait that uses the trait indicated by
        pathname for validation (if it's not a property trait), adds it to
        self, and creates a connection between the two. If alias is *None,*
        the name of the "promoted" trait will be the last entry in its
        pathname.  This is different from the *create_alias* function because
        the new trait is only tied to the specified trait by a connection
        in the Assembly. This means that updates to the new trait value will
        not be reflected in the connected trait until the assembly executes.
        The trait specified by pathname must exist.
        """
        if alias:
            newname = alias
        else:
            parts = pathname.split('.')
            newname = parts[-1]

        oldtrait = self.trait(newname)
        if oldtrait:
            self.raise_exception("a trait named '%s' already exists" %
                                 newname, TraitError)
        trait, val = self._find_trait_and_value(pathname)
        if not trait:
            self.raise_exception("the trait named '%s' can't be found" %
                                 pathname, TraitError)
        iotype = trait.iotype
        # the trait.trait_type stuff below is for the case where the trait is actually
        # a ctrait (very common). In that case, trait_type is the actual underlying
        # trait object
        if (getattr(trait,'get') or getattr(trait,'set') or
            getattr(trait.trait_type, 'get') or getattr(trait.trait_type,'set')):
            trait = None  # not sure how to validate using a property
                          # trait without setting it, so just don't use it
        newtrait = PassthroughTrait(iotype=iotype, validation_trait=trait)
        self.add_trait(newname, newtrait)
        setattr(self, newname, val)

        if iotype == 'in':
            self.connect(newname, pathname)
        else:
            self.connect(pathname, newname)

        return newtrait

    def get_dyn_trait(self, pathname, iotype=None):
        """Retrieves the named trait, attempting to create a Passthrough trait
        on-the-fly if the specified trait doesn't exist.
        """
        trait = self.trait(pathname)
        if trait is None:
            newtrait = self.create_passthrough(pathname)
            if iotype is not None and iotype != newtrait.iotype:
                self.raise_exception(
                    "new trait has iotype of '%s' but '%s' was expected" %
                    (newtrait.iotype, iotype), TraitError)
        return trait

    def split_varpath(self, path):
        """Return a tuple of compname,component,varname given a path
        name of the form 'compname.varname'. If the name is of the form 'varname'
        then compname will be None and comp is self. 
        """
        try:
            compname, varname = path.split('.', 1)
        except ValueError:
            return (None, self, path)
        
        return (compname, getattr(self, compname), varname)

    def connect(self, srcpath, destpath):
        """Connect one src Variable to one destination Variable. This could be
        a normal connection (output to input) or a passthrough connection."""

        srccompname, srccomp, srcvarname = self.split_varpath(srcpath)
        srctrait = srccomp.get_dyn_trait(srcvarname, 'out')
        destcompname, destcomp, destvarname = self.split_varpath(destpath)
        desttrait = destcomp.get_dyn_trait(destvarname, 'in')
        
        if srccompname == destcompname:
            self.raise_exception(
                'Cannot connect %s to %s. Both are on same component.' %
                                 (srcpath, destpath), RuntimeError)
        if srccomp is not self and destcomp is not self:
            # it's not a passthrough, so must connect input to output
            if srctrait.iotype != 'out':
                self.raise_exception(
                    '.'.join([srccomp.get_pathname(),srcvarname])+
                    ' must be an output variable',
                    RuntimeError)
            if desttrait.iotype != 'in':
                self.raise_exception(
                    '.'.join([destcomp.get_pathname(),destvarname])+
                    ' must be an input variable',
                    RuntimeError)
                
        if self.is_destination(destpath):
            self.raise_exception(destpath+' is already connected',
                                 RuntimeError)             
            
        # test compatability (raises TraitError on failure)
        if desttrait.validate is not None:
            desttrait.validate(destcomp, destvarname, 
                               getattr(srccomp, srcvarname))
        
        if destcomp is not self:
            destcomp.set_source(destvarname, srcpath)
            if srccomp is not self: # neither var is on boundary
                if hasattr(self.workflow, 'connect'):
                    self.workflow.connect(srccompname, destcompname, 
                                          srcvarname, destvarname)
        
        vgraph = self.get_var_graph()
        vgraph.add_edge(srcpath, destpath)
            
        # invalidate destvar if necessary
        if destcomp is self and desttrait.iotype == 'out': # boundary output
            if destcomp.get_valid(destvarname) and \
               srccomp.get_valid(srcvarname) is False:
                if self.parent:
                    # tell the parent that anyone connected to our boundary
                    # output is invalid.
                    # Note that it's a dest var in this scope, but a src var in
                    # the parent scope.
                    self.parent.invalidate_deps([destpath], True)
            self.set_valid(destpath, False)
        elif srccomp is self and srctrait.iotype == 'in': # boundary input
            self.set_valid(srcpath, False)
        else:
            destcomp.set_valid(destvarname, False)
            self.invalidate_deps([destpath])
        
        self._io_graph = None

        for drv in self.drivers:
            drv.graph_regen_needed()

    def _filter_internal_edges(self, edges):
        """Return a copy of the given list of edges with edges removed that are
        connecting two variables on the same component.
        """
        return [(u,v) for u,v in edges
                              if u.split('.', 1)[0] != v.split('.', 1)[0]]
    
    def disconnect(self, varpath, varpath2=None):
        """If varpath2 is supplied, remove the connection between varpath and
        varpath2. Otherwise, if varpath is the name of a trait, remove all
        connections to/from varpath in the current scope. If varpath is the
        name of a Component, remove all connections from all of its inputs
        and outputs. 
        """
        vargraph = self.get_var_graph()
        if varpath not in vargraph:
            tup = varpath.split('.', 1)
            if len(tup) == 1 and isinstance(getattr(self, varpath), Component):
                comp = getattr(self, varpath)
                for var in comp.list_inputs():
                    self.disconnect('.'.join([varpath, var]))
                for var in comp.list_outputs():
                    self.disconnect('.'.join([varpath, var]))
            else:
                self.raise_exception("'%s' is not a linkable attribute" %
                                     varpath, RuntimeError)
            return
        
        to_remove = []
        if varpath2 is not None:
            if varpath2 in vargraph[varpath]:
                to_remove.append((varpath, varpath2))
            elif varpath in vargraph[varpath2]:
                to_remove.append((varpath2, varpath))
            else:
                self.raise_exception('%s is not connected to %s' % 
                                     (varpath, varpath2), RuntimeError)
        else:  # remove all connections from the Variable
            to_remove.extend(vargraph.edges(varpath)) # outgoing edges
            to_remove.extend(vargraph.in_edges(varpath)) # incoming
        
        for src,sink in self._filter_internal_edges(to_remove):
            vtup = sink.split('.', 1)
            if len(vtup) > 1:
                getattr(self, vtup[0]).remove_source(vtup[1])
                # if its a connection between two children 
                # (no boundary connections) then remove a connection 
                # between two components in the component graph
                utup = src.split('.',1)
                if len(utup)>1 and hasattr(self.workflow, 'disconnect'):
                    self.workflow.disconnect(utup[0], vtup[0])
                
        vargraph.remove_edges_from(to_remove)
        
        # the io graph has changed, so have to remake it
        self._io_graph = None  
        for drv in self.drivers:
            drv.graph_regen_needed()


    def is_destination(self, varpath):
        """Return True if the Variable specified by varname is a destination
        according to our graph. This means that either it's an input connected
        to an output, or it's the destination part of a passthrough connection.
        """
        tup = varpath.split('.',1)
        preds = self._var_graph.pred.get(varpath, {})
        if len(tup) == 1:
            return len(preds) > 0
        else:
            start = tup[0]+'.'
            for pred in preds:
                if not pred.startswith(start):
                    return True
        return False

    def execute (self):
        """Runs its driver, or its workflow if there is no driver."""
        if len(self.drivers) > 0:
            self.drivers[0].run()
        else:
            self.workflow.run()
        self._update_boundary_vars()
        
    def _update_boundary_vars (self):
        """Update output variables on our bounary."""
        invalid_outs = self.list_outputs(valid=False)
        vgraph = self.get_var_graph()
        for out in invalid_outs:
            inedges = vgraph.in_edges(nbunch=out)
            if len(inedges) == 1:
                setattr(self, out, self.get(inedges[0][0]))

    def step(self):
        """Execute a single child component and return."""
        self.workflow.step()
        
    def stop(self):
        """Stop the workflow."""
        self.workflow.stop()
    
    def list_connections(self, show_passthrough=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        if show_passthrough:
            return self._filter_internal_edges(self.get_var_graph().edges())
        else:
            return self._filter_internal_edges([(outname,inname) for outname,inname in 
                                                self.get_var_graph().edges_iter() 
                                                if '.' in outname and '.' in inname])

    def update_inputs(self, varnames):
        """Transfer input data to input variables on the specified component.
        The varnames iterator is assumed to contain names that include the
        component name, for example: ['comp1.a', 'comp1.b'].
        """
        updated = False  # this becomes True if we actually update any inputs
        parent = self.parent
        vargraph = self.get_var_graph()
        pred = vargraph.pred
        
        for vname in varnames:
            preds = pred.get(vname, '')
            if len(preds) == 0: 
                continue
            elif len(preds) > 1:
                self.raise_exception("variable '%s' has multiple sources %s" %
                                     (vname, preds.keys()), RuntimeError)
            updated = True
            srcname = preds.keys()[0]
            srccompname,srccomp,srcvarname = self.split_varpath(srcname)
            destcompname,destcomp,destvarname = self.split_varpath(vname)

            if srccomp.get_valid(srcvarname) is False:  # source is invalid 
                # need to backtrack to get a valid source value
                if srccompname is None: # a boundary var
                    if parent:
                        parent.update_inputs(['.'.join([self.name, srcname])])
                    else:
                        srccomp.set_valid(srcvarname, True) # validate source
                else:
                    srccomp.update_outputs([srcvarname])

            try:
                srcval = srccomp.get_wrapped_attr(srcvarname)
            except Exception, err:
                self.raise_exception(
                    "cannot retrieve value of source attribute '%s'" %
                    srcname, type(err))
            try:
                destcomp.set(destvarname, srcval, srcname=srcname)
            except Exception, exc:
                msg = "cannot set '%s' from '%s': %s" % (vname, srcname, exc)
                self.raise_exception(msg, type(exc))
        
        return updated

    def update_outputs(self, outnames):
        """Execute any necessary internal or predecessor components in order
        to make the specified output variables valid.
        """
        self.update_inputs(outnames)

    def get_valids(self, names):
        """Returns a list of boolean values indicating whether the named
        attributes are valid (True) or invalid (False). Entries in names may
        specify either direct traits of self or those of direct children of
        self, but no deeper in the hierarchy than that.
        """
        valids = []
        for name in names:
            if self.trait(name):
                valids.append(self.get_valid(name))
            else:
                tup = name.split('.', 1)
                if len(tup) > 1:
                    comp = getattr(self, tup[0])
                    valids.append(comp.get_valid(tup[1]))
                else:
                    self.raise_exception("get_valids: unknown variable '%s'" %
                                         name, RuntimeError)
        return valids

    def invalidate_deps(self, varnames, notify_parent=False):
        """Mark all Variables invalid that depend on varnames. Returns a list
        of our newly invalidated boundary outputs.
        """
        vargraph = self.get_var_graph()
        succ = vargraph.succ  #successor nodes in the graph
        stack = set(varnames)
        outs = []
        while len(stack) > 0:
            name = stack.pop()
            if name in vargraph:
                tup = name.split('.', 1)
                if len(tup)==1:
                    self.set_valid(name, False)
                else:
                    getattr(self, tup[0]).set_valid(tup[1], False)
            else:
                self.raise_exception("%s is not an io trait" % name,
                                     RuntimeError)
            for vname in succ.get(name, []):
                tup = vname.split('.', 1)
                if len(tup) == 1:  #boundary var or Component
                    if self.trait(vname).iotype == 'out':
                        # it's an output boundary var
                        outs.append(vname)
                else:  # a var from a child component 
                    compname, compvar = tup
                    comp = getattr(self, compname)
                    if comp.get_valid(compvar):  # node is a valid Variable
                        for newvar in comp.invalidate_deps([compvar]):
                            stack.add('.'.join([compname, newvar]))
                        stack.add(vname)
        
        if len(outs) > 0:
            for out in outs:
                self.set_valid(out, False)
            if notify_parent and self.parent:
                self.parent.invalidate_deps(
                    ['.'.join([self.name,n]) for n in outs], 
                    notify_parent)
        return outs

