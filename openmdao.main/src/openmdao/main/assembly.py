""" Class definition for Assembly """


#public symbols
__all__ = ['Assembly']

import cStringIO

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Instance, TraitError
from enthought.traits.api import TraitType

import networkx as nx
from networkx.algorithms.traversal import is_directed_acyclic_graph, \
                                          strongly_connected_components

from openmdao.main.component import Component
from openmdao.main.driver import Driver
from openmdao.main.expression import Expression, ExpressionList

class PassthroughTrait(TraitType):
    """A trait that can use another trait for validation, but otherwise is
    just a trait that lives on an Assembly boundary and can be connected
    to other traits within the Assembly.
    """
    
    def validate(self, obj, name, value):
        """Validation for the PassThroughTrait"""
        if self.validation_trait:
            return self.validation_trait.validate(obj, name, value)
        return value

class Assembly (Component):
    """This is a container of Components. It understands how to connect inputs
    and outputs between its children.  When executed, it runs the top level
    Driver called 'driver'.
    """
    
    driver = Instance(Driver, allow_none=True,
                      desc="The top level Driver that manages execution of "
                           "this Assembly")
    
    def __init__(self, doc=None, directory=''):
        self.comp_graph = ComponentGraph()
                
        super(Assembly, self).__init__(doc=doc, directory=directory)
        
        # default Driver executes its workflow once
        self.add('driver', Driver())
        
    def add(self, name, obj):
        """Add obj to the component graph and call base class *add*.
        
        Returns the added object.
        """
        obj = super(Assembly, self).add(name, obj)
        if isinstance(obj, Component):
            self.comp_graph.add(obj)
        
        return obj
        
    def remove(self, name):
        """Remove the named container object from this assembly and remove
        it from its workflow (if any)."""
        cont = getattr(self, name)
        self.comp_graph.remove(cont)
        for obj in self.__dict__.values():
            if obj is not cont and isinstance(obj, Driver):
                obj.remove_from_workflow(cont)
                    
        return super(Assembly, self).remove(name)


    def create_passthrough(self, pathname, alias=None):
        """Creates a PassthroughTrait that uses the trait indicated by
        pathname for validation (if it's not a property trait), adds it to
        self, and creates a connection between the two. If alias is *None,*
        the name of the "promoted" trait will be the last entry in its
        pathname. The trait specified by pathname must exist.
        """
        if alias:
            newname = alias
        else:
            parts = pathname.split('.')
            newname = parts[-1]

        if newname in self.__dict__:
            self.raise_exception("'%s' already exists" %
                                 newname, TraitError)
        trait, val = self._find_trait_and_value(pathname)
        if not trait:
            self.raise_exception("the variable named '%s' can't be found" %
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
        """Retrieves the named trait, attempting to create a PassthroughTrait
        on-the-fly if the specified trait doesn't exist.
        """
        trait = self.traits().get(pathname)
        if trait is None:
            trait = self.create_passthrough(pathname)
            if iotype is not None and iotype != trait.iotype:
                self.raise_exception(
                    "new trait has iotype of '%s' but '%s' was expected" %
                    (trait.iotype, iotype), TraitError)
        return trait

    def _split_varpath(self, path):
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

        srccompname, srccomp, srcvarname = self._split_varpath(srcpath)
        srctrait = srccomp.get_dyn_trait(srcvarname, 'out')
        destcompname, destcomp, destvarname = self._split_varpath(destpath)
        desttrait = destcomp.get_dyn_trait(destvarname, 'in')
        
        if srccompname == destcompname:
            self.raise_exception(
                'Cannot connect %s to %s. Both are on same component.' %
                                 (srcpath, destpath), RuntimeError)
        if (srctrait.is_trait_type and (srctrait.is_trait_type(Expression) or srctrait.is_trait_type(ExpressionList))) or \
           (desttrait.is_trait_type and (desttrait.is_trait_type(Expression) or desttrait.is_trait_type(ExpressionList))):
            self.raise_exception('Cannot connect %s to %s because one of them is an Expression or ExpressionList' %
                                 (srcpath,destpath), RuntimeError)
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
            try:
                if desttrait.trait_type.get_val_meta_wrapper:
                    desttrait.validate(destcomp, destvarname, 
                                       srccomp.get_wrapped_attr(srcvarname))
                else:
                    desttrait.validate(destcomp, destvarname, 
                                       getattr(srccomp, srcvarname))
            except TraitError, err:
                self.raise_exception("can't connect '%s' to '%s': %s" % 
                                     (srcpath,destpath,str(err)), TraitError)
        
        if destcomp is not self:
            destcomp.set_source(destvarname, srcpath)
            if srccomp is not self: # neither var is on boundary
                self.comp_graph.connect(srcpath, destpath)
        
        # invalidate destvar if necessary
        if destcomp is self and desttrait.iotype == 'out': # boundary output
            if destcomp.get_valid(destvarname) and \
               srccomp.get_valid(srcvarname) is False:
                if self.parent:
                    # tell the parent that anyone connected to our boundary
                    # output is invalid.
                    # Note that it's a dest var in this scope, but a src var in
                    # the parent scope.
                    self.parent.invalidate_deps(self.name, [destvarname], True)
            self.comp_graph.connect(srcpath, '.'.join(['@out',destvarname]))
            self._valid_dict[destpath] = False
        elif srccomp is self and srctrait.iotype == 'in': # boundary input
            self.comp_graph.connect('.'.join(['@in',srcpath]), destpath)
        else:
            destcomp.invalidate_deps(varnames=[destvarname], notify_parent=True)

    def disconnect(self, varpath, varpath2=None):
        """If varpath2 is supplied, remove the connection between varpath and
        varpath2. Otherwise, if varpath is the name of a trait, remove all
        connections to/from varpath in the current scope. If varpath is the
        name of a Component, remove all connections from all of its inputs
        and outputs. 
        """
        to_remove = []
        if varpath in self.comp_graph: # varpath is a component name
            if varpath2 is not None:
                self.raise_exception("%s is not a valid second argument" %
                                     varpath2, RuntimeError)
            for u,v in self.comp_graph.var_edges(varpath):
                to_remove.append((u, v))
            for u,v in self.comp_graph.var_in_edges(varpath):
                to_remove.append((u, v))
            
        else:   # varpath is not a component name
            if '.' not in varpath:
                if varpath in self.list_inputs():
                    varpath = '.'.join(['@in', varpath])
                else:
                    varpath = '.'.join(['@out', varpath])
            if varpath2 is not None and '.' not in varpath2:
                if varpath2 in self.list_inputs():
                    varpath2 = '.'.join(['@in', varpath2])
                else:
                    varpath2 = '.'.join(['@out', varpath2])
            
            cname, vname = varpath.split('.', 1)
            if varpath2 is None:  # remove all connections to varpath
                dotvname = '.'+vname
                for u,v in self.comp_graph.var_edges(cname):
                    if u.endswith(dotvname):
                        to_remove.append((u, v))
                for u,v in self.comp_graph.var_in_edges(cname):
                    if v.endswith(dotvname):
                        to_remove.append((u, v))
            else:
                to_remove.append((varpath, varpath2))

        for src,sink in to_remove:
            sinkcomp,sinkvar = sink.split('.', 1)
            if sinkcomp[0] != '@':  # sink is not on boundary
                getattr(self, sinkcomp).remove_source(sinkvar)
            self.comp_graph.disconnect(src, sink)

    def is_destination(self, varpath):
        """Return True if the Variable specified by varname is a destination
        according to our graph. This means that either it's an input connected
        to an output, or it's the destination part of a passthrough connection.
        """
        return self.comp_graph.is_destination(varpath)
        
    def execute (self):
        """Runs driver and updates our boundary variables."""
        self.driver.run()
        self._update_boundary_vars()
    
    def _update_boundary_vars (self):
        """Update output variables on our bounary."""
        valids = self._valid_dict
        for srccompname,link in self.comp_graph.in_links('@out'):
            srccomp = getattr(self, srccompname)
            for dest,src in link._dests.items():
                if valids[dest] is False:
                    setattr(self, dest, srccomp.get_wrapped_attr(src))

    def step(self):
        """Execute a single child component and return."""
        self.driver.step()
        
    def stop(self):
        """Stop the calculation."""
        self.driver.stop()
    
    def list_connections(self, show_passthrough=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        return self.comp_graph.list_connections(show_passthrough)

    def update_inputs(self, compname, varnames):
        """Transfer input data to input variables on the specified component.
        The varnames iterator is assumed to contain local names (no component name), 
        for example: ['a', 'b'].
        """
        parent = self.parent
        vset = set(varnames)
        if compname == '@out':
            destcomp = self
        else:
            destcomp = getattr(self, compname)
        for srccompname,srcs,dests in self.comp_graph.in_map(compname, vset):
            if srccompname == '@in':  # boundary inputs
                srccompname = ''
                srccomp = self
                invalid_srcs = [k for k,v in self._valid_dict.items() if v is False and k in srcs]
                if len(invalid_srcs) > 0:
                    if parent:
                        parent.update_inputs(self.name, invalid_srcs)
                    else:
                        for name in invalid_srcs:
                            self._valid_dict[name] = True
            else:
                srccomp = getattr(self, srccompname)
                if not srccomp.is_valid():
                    srccomp.update_outputs(srcs)
            
            # TODO: add multiget/multiset stuff here...
            for src,dest in zip(srcs, dests):
                try:
                    srcval = srccomp.get_wrapped_attr(src)
                except Exception, err:
                    self.raise_exception(
                        "error retrieving value for %s from '%s'" %
                        (src,srccompname), type(err))
                try:
                    if srccomp is self:
                        srcname = src
                    else:
                        srcname = '.'.join([srccompname, src])
                    destcomp.set(dest, srcval, srcname=srcname)
                except Exception, exc:
                    if compname[0] == '@':
                        dname = dest
                    else:
                        dname = '.'.join([compname, dest])
                    msg = "cannot set '%s' from '%s': %s" % (dname, srcname, exc)
                    self.raise_exception(msg, type(exc))
            
    def update_outputs(self, outnames):
        """Execute any necessary internal or predecessor components in order
        to make the specified output variables valid.
        """
        self.update_inputs('@out', outnames)
        
    def get_valids(self, names):
        """Returns a list of boolean values indicating whether the named
        attributes are valid (True) or invalid (False). Entries in names may
        specify either direct traits of self or those of direct children of
        self, but no deeper in the hierarchy than that.
        """
        valids = []
        traits = self.traits()
        for name in names:
            if name in traits:
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

    def invalidate_deps(self, compname=None, varnames=None, notify_parent=False):
        """Mark all Variables invalid that depend on varnames. 
        Returns a list of our newly invalidated boundary outputs.
        """
        outs = set()
        compgraph = self.comp_graph
        if compname is None: # start at @in (boundary inputs)
            compname = '@in'
            if varnames is not None:
                for v in varnames:
                    if v in self._sources:
                        self._valid_dict[v] = False
            
        visited = set()
        partial_visited = {}
        stack = [(compname, varnames)]
            
        while len(stack) > 0:
            cname, vnames = stack.pop()
            if vnames is None:
                for destcname, link in compgraph.out_links(cname):
                    if link in visited: continue
                    
                    if destcname == '@out':
                        outs.update(link.get_dests())
                    else:
                        outnames = getattr(self, destcname).invalidate_deps(varnames=link.get_dests())
                        stack.append((destcname, outnames))
                    visited.add(link)
            else:  # specific varnames are invalidated
                for destcname, link in compgraph.out_links(cname):
                    if link in visited: continue
                    inputs = link.get_dests(varnames)
                    if not inputs: continue
                    if destcname == '@out':
                        outs.update(inputs)
                    else:
                        outnames = getattr(self, destcname).invalidate_deps(varnames=inputs)
                        stack.append((destcname, outnames))
                    partial = partial_visited.setdefault(link, set())
                    partial.update(inputs)
                    if len(partial) == len(link._srcs):
                        visited.add(link)
                        del partial_visited[link]
        
        if len(outs) > 0:
            self.set_valids(outs, False)
            if notify_parent and self.parent:
                self.parent.invalidate_deps(compname=self.name, varnames=outs, notify_parent=True)
        return outs
    
    def exec_counts(self, compnames):
        return [getattr(self,c).exec_count for c in compnames]


class ComponentGraph(object):
    """
    A dependency graph for Components.  Each edge contains a _Link object, which 
    maps all connected inputs and outputs between the two Components.
    """

    def __init__(self):
        self._graph = nx.DiGraph()
        self._graph.add_nodes_from(['@in', '@out']) #fake nodes for boundary vars
        
    def __contains__(self, compname):
        """Return True if this graph contains the given component."""
        return compname in self._graph
    
    def __len__(self):
        return len(self._graph)-2  # subtract 2 because of the 2 'fake' nodes
        
    def subgraph(self, nodelist):
        return self._graph.subgraph(nodelist)
    
    def copy_graph(self):
        graph = self._graph.copy()
        graph.remove_nodes_from(['@in', '@out'])
        return graph
    
    def is_destination(self, varpath):
        tup = varpath.split('.',1)
        if len(tup) > 1:
            for u,v,data in self._graph.in_edges(tup[0], data=True):
                if tup[1] in data['link']._dests:
                    return True
        else:
            for u,v,data in self._graph.in_edges('@out', data=True):
                if varpath in data['link']._dests:
                    return True
        return False

    def iter(self, scope):
        """Iterate through the nodes in dataflow order."""
        for n in nx.topological_sort(self._graph):
            if n[0] != '@':  # skip 'fake' nodes @in and @out
                yield getattr(scope, n)
            
    def add(self, comp):
        """Add the name of a Component to the graph."""
        self._graph.add_node(comp.name)

    def remove(self, comp):
        """Remove the name of a Component from the graph. It is not
        an error if the component is not found in the graph.
        """
        self._graph.remove_node(comp.name)
        
    def list_connections(self, show_passthrough=True):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        if show_passthrough:
            nbunch = None
        else:
            nbunch = [g for g in self._graph.nodes() if g[0] != '@']
        for u,v,data in self._graph.edges(nbunch, data=True):
            link = data['link']
            if u == '@in':
                for dest,src in link._dests.items():
                    conns.append((src, '.'.join([v,dest])))
            elif v == '@out':
                for dest,src in link._dests.items():
                    conns.append(('.'.join([u,src]), dest))
            else:
                for dest,src in link._dests.items():
                    conns.append(('.'.join([u,src]), '.'.join([v,dest])))
        return conns

    def in_map(self, cname, varset):
        """Yield a tuple of lists of the form (compname, srclist, destlist) for each link,
        where all dests in destlist are found in varset.
        """
        for u,v,data in self._graph.in_edges(cname, data=True):
            srcs = []
            dests = []
            link = data['link']
            matchset = varset.intersection(link._dests.keys())
            for dest in matchset:
                dests.append(dest)
                srcs.append(link._dests[dest])
            yield (u, srcs, dests)

    def in_links(self, cname):
        """Return a list of the form [(compname,link), (compname2,link2)...]
        containing each incoming link to the given component and the name
        of the connected component.
        """
        ret = []
        for u,v,data in self._graph.in_edges(cname, data=True):
            ret.append((u,data['link']))
        return ret
    
    def out_links(self, cname):
        """Return a list of the form [(compname,link), (compname2,link2)...]
        containing each outgoing link from the given component and the name
        of the connected component.
        """
        ret = []
        for u,v,data in self._graph.edges(cname, data=True):
            ret.append((v,data['link']))
        return ret

    def var_edges(self, name):
        """Return a list of outgoing edges connecting variables."""
        edges = []
        for u,v,data in self._graph.edges(name, data=True):
            for dest,src in data['link']._dests.items():
                edges.append(('.'.join([u,src]), '.'.join([v,dest])))
        return edges
    
    def var_in_edges(self, name):
        """Return a list of incoming edges connecting variables."""
        edges = []
        for u,v,data in self._graph.in_edges(name, data=True):
            for dest,src in data['link']._dests.items():
                edges.append(('.'.join([u,src]), '.'.join([v,dest])))
        return edges
    
    def connect(self, srcpath, destpath):
        """Add an edge to our Component graph from 
        *srccompname* to *destcompname*.
        """
        graph = self._graph
        srccompname, srcvarname = srcpath.split('.', 1)
        destcompname, destvarname = destpath.split('.', 1)
        try:
            link = graph[srccompname][destcompname]['link']
        except KeyError:
            link=_Link()
            graph.add_edge(srccompname, destcompname, link=link)
            
        if is_directed_acyclic_graph(graph):
            link.connect(srcvarname, destvarname)
        else:   # cycle found
            # do a little extra work here to give more info to the user in the error message
            strongly_connected = strongly_connected_components(graph)
            if len(link) == 0:
                graph.remove_edge(srccompname, destcompname)
            for strcon in strongly_connected:
                if len(strcon) > 1:
                    raise RuntimeError(
                        'circular dependency (%s) would be created by connecting %s to %s' %
                                 (str(strcon), 
                                  '.'.join([srccompname,srcvarname]), 
                                  '.'.join([destcompname,destvarname])))

    def disconnect(self, srcpath, destpath):
        """Disconnect the given variables."""
        graph = self._graph
        srccompname, srcvarname = srcpath.split('.', 1)
        destcompname, destvarname = destpath.split('.', 1)
        link = self._graph[srccompname][destcompname]['link']
        link.disconnect(srcvarname, destvarname)
        if len(link) == 0:
            self._graph.remove_edge(srccompname, destcompname)

    #def push_data(self, srccompname, scope):
        #for destcompname, link in self.out_links(srccompname):
            #link.push(scope, srccompname, destcompname)

            
class _Link(object):
    """A Class for keeping track of all connections between two Components."""
    def __init__(self, connections=None):
        self._srcs = {}
        self._dests = {}
        if connections is not None:
            for src,dest in connections.items():
                self.connect(src, dest)

    def __len__(self):
        return len(self._srcs)

    def connect(self, src, dest):
        if dest in self._dests:
            raise RuntimeError("%s is already connected" % dest)
        if src not in self._srcs:
            self._srcs[src] = []
        self._srcs[src].append(dest)
        self._dests[dest] = src
        
    def disconnect(self, src, dest):
        del self._dests[dest]
        dests = self._srcs[src]
        dests.remove(dest)
        if len(dests) == 0:
            del self._srcs[src]
    
    def invalidate(self, destcomp, varlist=None):
        if varlist is None:
            destcomp.set_valids(self._dests.keys(), False)
        else:
            destcomp.set_valids(varlist, False)

    def get_dests(self, srcs=None):
        """Return the list of destination vars that match the given source vars.
        If srcs is None, return a list of all dest vars.  Ignore any src vars
        that are not part of this link.
        """
        if srcs is None:
            return self._dests.keys()
        else:
            dests = []
            for name in srcs:
                dests.extend(self._srcs.get(name, []))
            return dests
    
    def get_srcs(self, dests=None):
        """Return the list of source vars that match the given dest vars.
        If dests is None, return a list of all src vars.  Ignore any dest vars
        that are not part of this link.
        """
        if dests is None:
            return self._srcs.keys()
        else:
            srcs = []
            for name in dests:
                src = self._dests.get(name)
                if src:
                    srcs.append(src)
            return srcs

    def push(self, scope, srccompname, destcompname):
        """Push the values of all sources to their corresponding destinations
        for this link.
        """
        # TODO: change to one multiset call
        srccomp = getattr(scope, srccompname)
        destcomp = getattr(scope, destcompname)
        
        for src,dests in self._srcs.items():
            for dest in dests:
                try:
                    srcval = srccomp.get_wrapped_attr(src)
                except Exception, err:
                    scope.raise_exception(
                        "error retrieving value for %s from '%s'" %
                        (src,srccompname), type(err))
                try:
                    srcname = '.'.join([srccompname,src])
                    destcomp.set(dest, srcval, srcname=srcname)
                except Exception, exc:
                    dname = '.'.join([destcompname,dest])
                    scope.raise_exception("cannot set '%s' from '%s': %s" % 
                                          (dname, srcname, exc), type(exc))
        

def dump_iteration_tree(obj):
    """Returns a text version of the iteration tree
    of an OpenMDAO object or hierarchy.  The tree
    shows which are being iterated over by which
    drivers.
    """
    def _dump_iteration_tree(obj, f, tablevel):
        if isinstance(obj, Driver):
            f.write(' '*tablevel)
            f.write(obj.get_pathname())
            f.write('\n')
            for comp in obj.workflow:
                if isinstance(comp, Driver) or isinstance(comp, Assembly):
                    _dump_iteration_tree(comp, f, tablevel+3)
                else:
                    f.write(' '*(tablevel+3))
                    f.write(comp.get_pathname())
                    f.write('\n')
        elif isinstance(obj, Assembly):
            f.write(' '*tablevel)
            f.write(obj.get_pathname())
            f.write('\n')
            _dump_iteration_tree(obj.driver, f, tablevel+3)
    f = cStringIO.StringIO()
    _dump_iteration_tree(obj, f, 0)
    return f.getvalue()

