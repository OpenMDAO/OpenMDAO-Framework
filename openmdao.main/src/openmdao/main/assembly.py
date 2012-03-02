""" Class definition for Assembly. """


#public symbols
__all__ = ['Assembly', 'set_as_top']

import cStringIO
import threading

# pylint: disable-msg=E0611,F0401
from enthought.traits.api import Missing
import networkx as nx
from networkx.algorithms.dag import is_directed_acyclic_graph
from networkx.algorithms.components import strongly_connected_components

from openmdao.main.interfaces import implements, IDriver
from openmdao.main.container import find_trait_and_value
from openmdao.main.component import Component
from openmdao.main.variable import Variable
from openmdao.main.datatypes.slot import Slot
from openmdao.main.driver import Driver
from openmdao.main.attrwrapper import AttrWrapper
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import is_instance
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.printexpr import eliminate_expr_ws, ExprNameTransformer

_iodict = { 'out': 'output', 'in': 'input' }


__has_top__ = False
__toplock__ = threading.RLock()


#fake nodes for boundary and passthrough connections
_fakes = ['@xin', '@xout', '@bin', '@bout']

def set_as_top(cont, first_only=False):
    """Specifies that the given Container is the top of a Container hierarchy.
    If first_only is True, then only set it as a top if a global
    top doesn't already exist.
    """
    global __toplock__
    global __has_top__
    with __toplock__:
        if __has_top__ is False and isinstance(cont, Assembly):
            __has_top__ = True
        elif first_only:
            return cont
    if cont._call_cpath_updated:
        cont.cpath_updated()
    return cont

class PassthroughTrait(Variable):
    """A trait that can use another trait for validation, but otherwise is
    just a trait that lives on an Assembly boundary and can be connected
    to other traits within the Assembly.
    """

    def validate(self, obj, name, value):
        """Validation for the PassThroughTrait."""
        if self.validation_trait:
            return self.validation_trait.validate(obj, name, value)
        return value


class PassthroughProperty(Variable):
    """Replacement for PassthroughTrait when the target is a proxy/property
    trait. PassthroughTrait would get a core dump while pickling.
    """
    def __init__(self, target_trait, **metadata):
        self._trait = target_trait
        self._vals = {}
        super(PassthroughProperty, self).__init__(**metadata)

    def get(self, obj, name):
        return self._vals.get(obj, {}).get(name, self._trait.default_value)

    def set(self, obj, name, value):
        if obj not in self._vals:
            self._vals[obj] = {}
        self._vals[obj][name] = self._trait.validate(obj, name, value)


class ExprMapper(object):
    """A mapping between source expressions and destination expressions"""
    def __init__(self, scope):
        self._compgraph = None  # component dependency graph
        self._exprgraph = nx.DiGraph()  # graph of source expressions to destination expressions
        self._refer_cache = {} # keep track of which comps/vars are referred to by exprs
        self._scope = scope
    
    def copy_graph(self):
        graph = self.get_compgraph().copy()
        graph.remove_nodes_from(_fakes)
        return graph

    def get_connected_inputs(self):
        """Return a list of connected variables on the input boundary"""
        conn_inputs = []
        graph = self._exprgraph
        for node, data in graph.nodes(data=True):
            if graph.out_degree(node) > 0:
                if 'parent' in data['expr'].get_referenced_compnames():
                    for succ in graph.successors(node):
                        for vname in graph.node[succ]['expr'].get_referenced_varpaths():
                            if '.' not in vname:
                                conn_inputs.append(vname)
        return conn_inputs
    
    def get_output_exprs(self):
        """Return all destination expressions at the output boundary"""
        exprs = []
        graph = self._exprgraph
        for node, data in graph.nodes(data=True):
            if graph.in_degree(node) > 0:
                expr = data['expr']
                if len(expr.get_referenced_compnames()) == 0:
                    exprs.append(expr)
        return exprs
        
    def get_expr(self, text):
        return self._exprgraph.node[text]['expr']
    
    def get_connected_outputs(self):
        """Return a list of connected variables on the output boundary"""
        conn_outputs = []
        graph = self._exprgraph
        for node, data in graph.nodes(data=True):
            if graph.in_degree(node) > 0:
                conn_outputs.extend([v for v in data['expr'].get_referenced_varpaths() if '.' not in v])
        return conn_outputs
    
    def get_source(self, dest_expr):
        """Returns the source expression that is connected to the given 
        destination expression.
        """
        dct = self._exprgraph.pred.get(dest_expr)
        if dct and len(dct) > 0:
            srctxt = dct.keys()[0]
            return self._exprgraph.node[srctxt]['expr']
        else:
            return None
    
    def get_dests(self, src_expr):
        """Returns the list of destination expressions that are connected to the given 
        source expression.
        """
        graph = self._exprgraph
        return [graph.node(name)['expr'] for name in self._exprgraph.succ[src_expr].keys()]
    
    def get_compgraph(self):
        """Return the cached component graph or create a new one if it doesnt' exist."""
        if self._compgraph is None:
            self._compgraph = self._create_compgraph()
        return self._compgraph
    
    def _update_compgraph(self, graph, srcexpr, destexpr):
        """Update the component graph based on a new connection between a source expression
        and a destination expression.
        """
        src = srcexpr.text
        dest = destexpr.text
        destcomps = destexpr.get_referenced_compnames()
        if len(destcomps) == 0:
            destcomp = '@bout'  # boundary output
        else:
            destcomp = destcomps.pop()
            if destcomp == 'parent':
                destcomp = '@xout'
            elif not isinstance(getattr(self._scope, destcomp), Component): 
                destcomp = '@bout' # allows for refs to attributes of a VarTree on boundary
        
        self._refer_cache.setdefault(destcomp, set())
        self._refer_cache[destcomp].add(dest)
        
        for destpath in destexpr.get_referenced_varpaths():
            self._refer_cache.setdefault(destpath, set())
            self._refer_cache[destpath].add(dest)
            
        for srcpath in srcexpr.get_referenced_varpaths():
            if srcpath.startswith('parent.'):
                srccomp = '@xin'
            else:
                srccomp, _, srcvar = srcpath.partition('.')
                if not srcvar:
                    srccomp = '@bin'
                    
            self._refer_cache.setdefault(srcpath, set())
            self._refer_cache.setdefault(srccomp, set())
            self._refer_cache[srccomp].add(src)
            self._refer_cache[srcpath].add(src)
            graph.add_edge(srccomp, destcomp)
        return graph
    
    def _create_compgraph(self):
        """Create a new component graph based on information in the expression graph."""
        graph = nx.DiGraph()
        graph.add_nodes_from(_fakes)
        for src,dest in self._exprgraph.edges():
            self._update_compgraph(graph, 
                                   self._exprgraph.node(src)['expr'], 
                                   self._exprgraph.node(dest)['expr'])
        return graph
        
    def add(self, compname):
        compgraph = self.get_compgraph()
        compgraph.add_node(compname)
        
    def remove(self, compname):
        """Remove any connections referring to the given component"""
        self._compgraph = None
        self._exprgraph.remove_nodes_from(self.find_referring_exprs(compname))
        self._remove_disconnected_exprs()
        
    def connect(self, src, dest, scope):
        self._exprgraph.add_edge(src, dest)
        
    def find_referring_exprs(self, name):
        """Returns a list of expressions that reference the given name, which
        can refer to either a variable or a component.
        """
        exprs = self._refer_cache.get(name)
        if exprs:
            return exprs
        return [ex for ex,data in self._exprgraph.nodes(data=True) if data['expr'].refers_to(name)]
    
    def _remove_disconnected_exprs(self):
        # remove all expressions that are no longer connected to anything
        to_remove = []
        graph = self._exprgraph
        for expr in graph.nodes():
            if graph.in_degree(expr) == 0 and graph.out_degree(expr) == 0:
                to_remove.append(expr)
        for expr in to_remove:
            graph.remove_node(expr)

    def disconnect(self, srcpath, destpath=None):
        """Disconnect the given expressions/variables/components."""
        self._compgraph = None  # force later rebuild of component graph
        self._refer_cache = {}
        graph = self._exprgraph
        
        if destpath is None:
            if srcpath in graph:
                graph.remove_node(srcpath)
            else:
                for expr in self.find_referring_exprs(srcpath):
                    graph.remove_node(expr)
            self._remove_disconnected_exprs()
            return

        if srcpath in graph and destpath in graph:
            graph.remove_edge(srcpath, destpath)
            self._remove_disconnected_exprs()
        else: # assume they're disconnecting two variables, so find connected exprs that refer to them
            src_exprs = set([exp.text for exp in self.find_referring_exprs(srcpath)])
            dest_exprs = set([exp.text for exp in self.find_referring_exprs(destpath)])
            
            to_remove = []
            for src,dest in graph.edges():
                if src in src_exprs and dest in dest_exprs:
                    to_remove.append((src, dest))
                    
            graph.remove_edges_from(to_remove)
            
    def check_connect(self, src, dest, scope):
        """Connect a source expression to a destination expression."""
        
        # make sure we don't have any whitespace buried within the expression that would cause
        # two versions of the same expression (one with ws and one without) to appear different
        src = eliminate_expr_ws(src)
        dest = eliminate_expr_ws(dest)
        
        if self.get_source(dest) is not None:
            scope.raise_exception("'%s' is already connected to source '%s'" % (dest, self.get_source(dest)),
                                  RuntimeError)
        
        destexpr = ExprEvaluator(dest, scope, default_getter='get_wrapped_attr')
        srcexpr = ExprEvaluator(src, scope, default_getter='get_wrapped_attr')
        
        srccomps = srcexpr.get_referenced_compnames()
        destcomps = destexpr.get_referenced_compnames()
        destvars = destexpr.get_referenced_varpaths()
        
        if len(destvars) != 1:
            scope.raise_exception("destination expr '%s' does not refer to a single variable." % dest,
                                  RuntimeError)
        
        if destcomps and destcomps.pop() in srccomps:
            scope.raise_exception("Cannot connect '%s' to '%s'. Both refer to the same component." % (src,dest),
                                  RuntimeError)
            
        compgraph = self.get_compgraph()
        self._update_compgraph(compgraph, srcexpr, destexpr)
                
        if is_directed_acyclic_graph(compgraph):
            if src not in self._exprgraph:
                self._exprgraph.add_node(src, expr=srcexpr)
            if dest not in self._exprgraph:
                self._exprgraph.add_node(dest, expr=destexpr)
        else:   # cycle found
            self._compgraph = None  # force regeneration of compgraph next time
            strongly_connected = strongly_connected_components(compgraph)
            # do a little extra work here to give more info to the user in the error message
            for strcon in strongly_connected:
                if len(strcon) > 1:
                    scope.raise_exception(
                        'circular dependency (%s) would be created by connecting %s to %s' %
                                 (str(strcon), src, dest), RuntimeError)
        return srcexpr, destexpr

    def _get_invalidated_destexprs(self, scope, compname, varset):
        """For a given set of variable names that has changed (or None),
        return a list of all destination expressions that are invalidated. A
        varset value of None indicates that ALL variables in the given
        component are invalidated.
        """
        exprs = set()
        graph = self._exprgraph
        
        if varset is None:
            exprs.update(self.find_referring_exprs(compname))
        else:
            for varpath in ['.'.join([compname, v]) for v in varset]:
                exprs.update(self.find_referring_exprs(varpath))
        
        invalids = []
        for expr in exprs:
            dct = graph.succ.get(expr)
            if dct:
                for dest in dct.keys():
                    invalids.append(self.get_expr(dest))
                    
        return invalids
        
    def invalidate_deps(self, scope, cnames, varsets, force=False):
        """Walk through all dependent nodes in the graph, invalidating all
        variables that depend on output sets for the given component names.
        
        scope: Component
            Scoping object containing this dependency graph.
            
        cnames: list of str
            Names of starting nodes.
            
        varsets: list of sets of str
            Sets of names of outputs from each starting node.
            
        force: bool (optional)
            If True, force invalidation to continue even if a component in
            the dependency chain was already invalid.
        """
        compgraph = self.get_compgraph()
        stack = zip(cnames, varsets)
        outset = set()  # set of changed boundary outputs
        while(stack):
            srccomp, varset = stack.pop()
            destexprs = self._get_invalidated_destexprs(scope, srccomp, varset)
            compvars = {}
            for destexpr in destexprs:
                varpaths = destexpr.get_referenced_varpaths()
                for vp in varpaths:
                    parts = vp.split('.', 1)
                    if len(parts) > 1:
                        compvars.setdefault(parts[0], set())
                        compvars[parts[0]].add(parts[1])
                    else:
                        outset.add(vp)
            
            for cname in compvars.keys():
                if cname not in compgraph:  # must be some output var that has its own attributes
                    outset.add(cname)
                    del compvars[cname]
                else:
                    comp = getattr(scope, cname)
                    outs = comp.invalidate_deps(varnames=compvars[cname], force=force)
                    if (outs is None) or outs:
                        stack.append((cname, compvars[cname]))
        return outset

    def find_all_connecting(self, start, end):
        """Return the set of all components along all paths between 
        start and end.  The start and end nodes are included
        in the set if they're connected.
        """
        if start == end:
            return set()
        graph = self.get_compgraph()
        fwdset = set()
        backset = set()
        tmpset = set([end])
        while tmpset:
            node = tmpset.pop()
            if node in backset:
                continue
            backset.add(node)
            tmpset.update(graph.predecessors(node))
        
        tmpset = set([start])
        while tmpset:
            node = tmpset.pop()
            if node in fwdset:
                continue
            fwdset.add(node)
            tmpset.update(graph.successors(node))
        
        return fwdset.intersection(backset)

class Assembly (Component):
    """This is a container of Components. It understands how to connect inputs
    and outputs between its children.  When executed, it runs the top level
    Driver called 'driver'.
    """
    
    driver = Slot(IDriver, allow_none=True,
                    desc="The top level Driver that manages execution of "
                    "this Assembly.")
    
    def __init__(self, doc=None, directory=''):
        
        super(Assembly, self).__init__(doc=doc, directory=directory)
        
        self._depgraph = ExprMapper(self)
        
        # default Driver executes its workflow once
        self.add('driver', Driver())
        
        set_as_top(self, first_only=True) # we're the top Assembly only if we're the first instantiated
        
    def add(self, name, obj):
        """Call the base class *add*.  Then,
        if obj is a Component, add it to the component graph.
        
        Returns the added object.
        """
        obj = super(Assembly, self).add(name, obj)
        if is_instance(obj, Component):
            self._depgraph.add(obj.name)
        return obj
        
    def remove(self, name):
        """Remove the named container object from this assembly and remove
        it from its workflow (if any)."""
        cont = getattr(self, name)
        self._depgraph.remove(name)
        for obj in self.__dict__.values():
            if obj is not cont and is_instance(obj, Driver):
                obj.workflow.remove(name)
                    
        return super(Assembly, self).remove(name)

    def create_passthrough(self, pathname, alias=None):
        """Creates a PassthroughTrait that uses the trait indicated by
        pathname for validation, adds it to self, and creates a connection
        between the two. If alias is *None,* the name of the alias trait will
        be the last entry in its pathname. The trait specified by pathname
        must exist.
        """
        parts = pathname.split('.')
        if alias:
            newname = alias
        else:
            newname = parts[-1]

        if newname in self.__dict__:
            self.raise_exception("'%s' already exists" %
                                 newname, KeyError)
        if len(parts) < 2:
            self.raise_exception('destination of passthrough must be a dotted path',
                                 NameError)
        comp = self
        for part in parts[:-1]:
            try:
                comp = getattr(comp, part)
            except AttributeError:
                trait = None
                break
        else:
            trait = comp.get_trait(parts[-1])
            iotype = comp.get_iotype(parts[-1])
            
        if trait:
            ttype = trait.trait_type
            if ttype is None:
                ttype = trait
        else:
            if not self.contains(pathname):
                self.raise_exception("the variable named '%s' can't be found" %
                                     pathname, KeyError)
            iotype = self.get_metadata(pathname, 'iotype')
            
        if trait is not None and not trait.validate:
            trait = None  # no validate function, so just don't use trait for validation
            
        metadata = self.get_metadata(pathname)
        metadata['target'] = pathname
        # PassthroughTrait to a trait with get/set methods causes a core dump
        # in Traits (at least through 3.6) while pickling.
        if "validation_trait" in metadata: 
            if metadata['validation_trait'].get is None:
                newtrait = PassthroughTrait(**metadata)
            else:
                newtrait = PassthroughProperty(metadata['validation_trait'],
                                               **metadata)
        elif trait and ttype.get: 
            newtrait = PassthroughProperty(ttype, **metadata)
        else: 
            newtrait = PassthroughTrait(validation_trait=trait, **metadata)
        self.add_trait(newname, newtrait)
        setattr(self, newname, self.get(pathname))

        if iotype == 'in':
            self.connect(newname, pathname)
        else:
            self.connect(pathname, newname)
            
        return newtrait

    def _split_varpath(self, path):
        """Return a tuple of compname,component,varname given a path
        name of the form 'compname.varname'. If the name is of the form 'varname'
        then compname will be None and comp is self. 
        """
        try:
            compname, varname = path.split('.', 1)
        except ValueError:
            return (None, self, path)
        
        t = self.get_trait(compname)
        if t and t.iotype:
            return (None, self, path)
        return (compname, getattr(self, compname), varname)
        
    @rbac(('owner', 'user'))
    def connect(self, srcpath, destpath):
        """Connect one src Variable to one destination Variable. This could be
        a normal connection between variables from two internal Components, or
        it could be a passthrough connection, which connects across the scope boundary
        of this object.  When a pathname begins with 'parent.', that indicates
        that it is referring to a Variable outside of this object's scope.
        
        srcpath: str
            Pathname of source variable.
            
        destpath: str
            Pathname of destination variable.
        """
        srcexpr, destexpr = self._depgraph.check_connect(srcpath, destpath, self)
        srcvars = srcexpr.get_referenced_varpaths()
        destvar = destexpr.get_referenced_varpaths().pop()
        
        config_changed = False
        internal = False
        
        destcompname, destcomp, destvarname = self._split_varpath(destvar)
        
        if not destvar.startswith('parent.'):
            for srcvar in srcvars:
                if not srcvar.startswith('parent.'):
                    internal = True
                    
                    srccompname, srccomp, srcvarname = self._split_varpath(srcvar)
                    
                    if srccomp is not self and destcomp is not self:
                        config_changed = True
        
                    dest_io = 'out' if destcomp is self else 'in'
                    src_io = 'in' if srccomp is self else 'out'
                    
                    srctrait = srccomp.get_dyn_trait(srcvarname, src_io)
                    desttrait = destcomp.get_dyn_trait(destvarname, dest_io)
                    
                    ttype = desttrait.trait_type
                    if not ttype:
                        ttype = desttrait
                    try:
                        if ttype.get_val_wrapper:
                            srcval = srccomp.get_wrapped_attr(srcvarname)
                        else:
                            srcval = srccomp.get(srcvarname)
                        if ttype.validate:
                            ttype.validate(destcomp, destvarname, srcval)
                        else:
                            pass  # no validate function on destination trait. Most likely
                                  # it's a property trait.  No way to validate without
                                  # unknown side effects.
                    except Exception as err:
                        self.raise_exception("can't connect '%s' to '%s': %s" %
                                             (srcpath, destpath, str(err)), RuntimeError)

        super(Assembly, self).connect(srcpath, destpath)
        
        # if it's an internal connection, could change dependencies, so we have
        # to call config_changed to notify our driver
        if internal:
            if config_changed:
                self.config_changed(update_parent=False)

            outs = destcomp.invalidate_deps(varnames=set([destvarname]), force=True)
            if (outs is None) or outs:
                bouts = self.child_invalidated(destcompname, outs, force=True)

    @rbac(('owner', 'user'))
    def disconnect(self, varpath, varpath2=None):
        """If varpath2 is supplied, remove the connection between varpath and
        varpath2. Otherwise, if varpath is the name of a trait, remove all
        connections to/from varpath in the current scope. If varpath is the
        name of a Component, remove all connections from all of its inputs
        and outputs. 
        """
        if varpath2 is None:
            graph = self._depgraph._exprgraph
            to_remove = set()
            for expr in self._depgraph.find_referring_exprs(varpath):
                for u,v in graph.edges(expr):
                    to_remove.add((u,v))
                for u,v in graph.in_edges(expr):
                    to_remove.add((u,v))
            for u,v in to_remove:
                super(Assembly, self).disconnect(u, v)
        else:
            super(Assembly, self).disconnect(varpath, varpath2)
            
    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed, connections are made
        or removed, etc.
        """
        super(Assembly, self).config_changed(update_parent)
        # driver must tell workflow that config has changed because
        # dependencies may have changed
        if self.driver is not None:
            self.driver.config_changed(update_parent=False)
        
    def execute (self):
        """Runs driver and updates our boundary variables."""
        self.driver.run(ffd_order=self.ffd_order, case_id=self._case_id)
        
        valids = self._valid_dict
        
        # now update boundary outputs
        for expr in self._depgraph.get_output_exprs():
            if valids[expr.text] is False:
                srcexpr = self._depgraph.get_expr(expr.text)
                expr.set(srcexpr.evaluate(), src=srcexpr.text)
                # setattr(self, dest, srccomp.get_wrapped_attr(src))
            else:
                # PassthroughProperty always valid for some reason.
                try:
                    dst_type = self.get_trait(dest).trait_type
                except AttributeError:
                    pass
                else:
                    if isinstance(dst_type, PassthroughProperty):
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
        return self._depgraph.list_connections(show_passthrough)

    #def _cvt_input_srcs(self, sources):
        #link = self._depgraph.get_link('@xin', '@bin')
        #if link is None:
            #return sources
        #else:
            #newsrcs = []
            #dests = link._dests
            #for s in sources:
                #if '.' in s:
                    #newsrcs.append(dests[s])
                #else:
                    #newsrcs.append(s)
            #return newsrcs
        
    @rbac(('owner', 'user'))
    def update_inputs(self, compname, exprs):
        """Transfer input data to input expressions on the specified component.
        The exprs iterator is assumed to contain expression strings that reference
        component variables relative to the component, e.g., 'abc[3][1]' rather
        than 'comp1.abc[3][1]'.
        """
        parent = self.parent
        if compname is None: # update boundary outputs (which are inputs in this scope)
            invalids = [e for e in exprs if not self._valid_dict[e]]
            if len(invalids) > 0:
                if parent:
                    parent.update_inputs(self.name, invalids)
                # invalid inputs have been updated, so mark them as valid
                for name in invalids:
                    self._valid_dict[name] = True
            return
        
        expr_info = []
        invalids = []
        for expr in ['.'.join([compname, n]) for n in exprs]:
            srcexpr = self._depgraph.get_source(expr)
            invalids.extend(srcexpr.invalid_refs())
            expr_info.append((srcexpr, self._depgraph.get_expr(expr)))
            
        # if source exprs reference invalid vars, request an update
        if invalids:
            simple, compmap = partition_names_by_comp(invalids)
            if simple and self.parent:
                self.parent.update_inputs(self.name, simple)
            for cname, vnames in compmap.items():
                getattr(self, cname).update_outputs(vnames)
                #self.set_valid(vnames, True)
            
        for srcexpr, destexpr in expr_info:
            destexpr.set(srcexpr.evaluate(), src=srcexpr.text)
            
    def update_outputs(self, outnames):
        """Execute any necessary internal or predecessor components in order
        to make the specified output variables valid.
        """
        simple, compmap = partition_names_by_comp(outnames)
        if simple:  # boundary outputs
            self.update_inputs(None, simple)
        for cname, vnames in compmap.items():  # auto passthroughs to internal variables
            getattr(self, cname).update_outputs(vnames)
            self.set_valid(vnames, True)
        
    def get_valid(self, names):
        """Returns a list of boolean values indicating whether the named
        variables are valid (True) or invalid (False). Entries in names may
        specify either direct traits of self or those of children.
        """

        ret = [None]*len(names)
        posdict = dict([(name,i) for i,name in enumerate(names)])
        
        simple,compmap = partition_names_by_comp(names)
        if simple:
            vals = super(Assembly, self).get_valid(simple)
            for i,val in enumerate(vals):
                ret[posdict[simple[i]]] = val

        if compmap:
            for compname, varnames in compmap.items():
                vals = getattr(self, compname).get_valid(varnames)
                for i,val in enumerate(vals):
                    full = '.'.join([compname,varnames[i]])
                    ret[posdict[full]] = val
        return ret

    def _input_updated(self, name):
        if self._valid_dict[name]:  # if var is not already invalid
            outs = self.invalidate_deps(varnames=set([name]))
            if ((outs is None) or outs) and self.parent:
                self.parent.child_invalidated(self.name, outs)
            
    def child_invalidated(self, childname, outs=None, force=False):
        """Invalidate all variables that depend on the outputs provided
        by the child that has been invalidated.
        """
        bouts = self._depgraph.invalidate_deps(self, [childname], [outs], force)
        if bouts and self.parent:
            self.parent.child_invalidated(self.name, bouts, force)
        return bouts
                    
    def invalidate_deps(self, varnames=None, force=False):
        """Mark all Variables invalid that depend on varnames. 
        Returns a list of our newly invalidated boundary outputs.
        
        varnames: iter of str (optional)
            An iterator of names of destination variables.
            
        force: bool (optional)
            If True, force the invalidation to proceed beyond the 
            boundary even if all outputs were already invalid.
        """
        valids = self._valid_dict
        conn_ins = set(self.list_inputs(connected=True))
        
        # If varnames is None, we're being called from a parent Assembly
        # as part of a higher level invalidation, so we only need to look
        # at our connected inputs
        if varnames is None:
            names = conn_ins
        else:
            names = varnames
        
        # We only care about inputs that are changing from valid to invalid.
        # If they're already invalid, then we've already done what we needed to do,
        # unless force is True, in which case we continue with the invalidation.
        if force:
            invalidated_ins = names
        else:
            invalidated_ins = [n for n in names if valids[n] is True]
            if not invalidated_ins:  # no newly invalidated inputs, so no outputs change status
                return []

        if varnames is None:
            self.set_valid(invalidated_ins, False)
        else: # only invalidate *connected* inputs, because unconnected inputs
              # are always valid
            self.set_valid([n for n in invalidated_ins if n in conn_ins], False)

        outs = self._depgraph.invalidate_deps(self, ['@bin'], [invalidated_ins], force)

        if outs:
            self.set_valid(outs, False)
        return outs
    
    def exec_counts(self, compnames):
        return [getattr(self,c).exec_count for c in compnames]
    
    def calc_derivatives(self, first=False, second=False):
        """ Overides the component's version of this function. An assembly
        must initiate the call of calc_derivatives on all components in its
        driver's workflow."""
        
        self.driver.calc_derivatives(first, second)
        
    def check_derivatives(self, order, driver_inputs, driver_outputs):
        """An assembly just tells its driver to run check_derivatives on each
        element in its workflow. Note that an assembly signifies a change of
        scope, so the driver input and output lists are pared down."""
        
        # Put the driver connection lists into our local scope by removing
        # the assembly name from the dotted path.
        for j, item in enumerate(driver_inputs):
            if isinstance(item, tuple):
                
                tuple_list = []
                for tup_item in item:
                    names = tup_item.split('.',1)
                    if names[0] == self.name:
                        tuple_list.append(names[1])
                    else:
                        tuple_list.append(tup_item)
                        
                driver_inputs[j] = tuple(tuple_list)
            else:        
                names = item.split('.',1)
                if names[0] == self.name:
                    driver_inputs[j] = names[1]
        
        for j, item in enumerate(driver_outputs):
            if isinstance(item, tuple):

                tuple_list = []
                for tup_item in item:
                    names = tup_item.split('.',1)
                    if names[0] == self.name:
                        tuple_list.append(names[1])
                    else:
                        tuple_list.append(tup_item)
                        
                driver_inputs[j] = tuple(tuple_list)
            else:
                names = item.split('.',1)
                if names[0] == self.name:
                    driver_outputs[j] = names[1]
        
        self.driver.check_derivatives(order, driver_inputs, driver_outputs)
    


def dump_iteration_tree(obj):
    """Returns a text version of the iteration tree
    of an OpenMDAO object or hierarchy.  The tree
    shows which are being iterated over by which
    drivers.
    """
    def _dump_iteration_tree(obj, f, tablevel):
        if is_instance(obj, Driver):
            f.write(' '*tablevel)
            f.write(obj.get_pathname())
            f.write('\n')
            for comp in obj.workflow:
                if is_instance(comp, Driver) or is_instance(comp, Assembly):
                    _dump_iteration_tree(comp, f, tablevel+3)
                else:
                    f.write(' '*(tablevel+3))
                    f.write(comp.get_pathname())
                    f.write('\n')
        elif is_instance(obj, Assembly):
            f.write(' '*tablevel)
            f.write(obj.get_pathname())
            f.write('\n')
            _dump_iteration_tree(obj.driver, f, tablevel+3)
    f = cStringIO.StringIO()
    _dump_iteration_tree(obj, f, 0)
    return f.getvalue()


def partition_names_by_comp(names):
    """Take an iterator of names and return a tuple of the form (namelist,
    compmap) where namelist is a list of simple names (no dots) and compmap is
    a dict with component names keyed to lists of variable names.
    """
    simple = []
    compmap = {}
    for name in names:
        parts = name.split('.', 1)
        if len(parts) == 1:
            simple.append(name)
        else:
            compmap.setdefault(parts[0], []).append(parts[1])
    return (simple, compmap)


