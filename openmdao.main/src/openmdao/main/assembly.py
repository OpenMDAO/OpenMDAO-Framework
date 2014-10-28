""" Class definition for Assembly. """


#public symbols
__all__ = ['Assembly', 'set_as_top']

from fnmatch import fnmatch
import re
import sys
import threading
import traceback
from itertools import chain

from numpy import ndarray
from zope.interface import implementedBy

# pylint: disable=E0611,F0401
import networkx as nx

from openmdao.main.mpiwrap import MPI, mpiprint

from openmdao.main.exceptions import NoFlatError
from openmdao.main.interfaces import implements, IAssembly, IDriver, \
                                     IArchitecture, IComponent, IContainer, \
                                     ICaseIterator, ICaseRecorder, \
                                     IDOEgenerator, IHasParameters, IImplicitComponent
from openmdao.main.mp_support import has_interface
from openmdao.main.container import _copydict
from openmdao.main.component import Component, Container
from openmdao.main.variable import Variable
from openmdao.main.vartree import VariableTree
from openmdao.main.datatypes.api import List, Slot, Str
from openmdao.main.driver import Driver
from openmdao.main.hasparameters import HasParameters, ParameterGroup
from openmdao.main.hasconstraints import HasConstraints, HasEqConstraints, \
                                         HasIneqConstraints
from openmdao.main.hasobjective import HasObjective, HasObjectives
from openmdao.main.hasresponses import HasResponses
from openmdao.main.rbac import rbac
from openmdao.main.mp_support import is_instance
from openmdao.main.printexpr import eliminate_expr_ws
from openmdao.main.expreval import ExprEvaluator
from openmdao.main.exprmapper import ExprMapper
from openmdao.main.pseudocomp import PseudoComponent, UnitConversionPComp
from openmdao.main.array_helpers import is_differentiable_var, get_val_and_index, \
                                        get_flattened_index, \
                                        get_var_shape, flattened_size
from openmdao.main.depgraph import DependencyGraph, all_comps, \
                                   collapse_connections, prune_reduced_graph, \
                                   vars2tuples, relevant_subgraph, \
                                   map_collapsed_nodes, simple_node_iter, \
                                   reduced2component, collapse_subdrivers, \
                                   fix_state_connections, connect_subvars_to_comps, \
                                   add_boundary_comps, fix_dangling_vars, fix_duplicate_dests
from openmdao.main.systems import SerialSystem, _create_simple_sys

from openmdao.util.graph import list_deriv_vars, base_var, fix_single_tuple
from openmdao.util.log import logger
from openmdao.util.debug import strict_chk_config

_iodict = {'out': 'output', 'in': 'input'}

_missing = object()

__has_top__ = False
__toplock__ = threading.RLock()


def set_as_top(cont, first_only=False):
    """Specifies that the given Container is the top of a Container hierarchy.
    If first_only is True, then only set it as a top if a global
    top doesn't already exist.
    """
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
        old = self.get(obj, name)
        if value != old:
            self._vals[obj][name] = self._trait.validate(obj, name, value)
            obj.trait_property_changed(name, old, value)


def _find_common_interface(obj1, obj2):
    for iface in (IAssembly, IComponent, IDriver, IArchitecture, IContainer,
                  ICaseIterator, ICaseRecorder, IDOEgenerator):
        if has_interface(obj1, iface) and has_interface(obj2, iface):
            return iface
    return None


class Assembly(Component):
    """This is a container of Components. It understands how to connect inputs
    and outputs between its children.  When executed, it runs the top level
    Driver called 'driver'.
    """

    implements(IAssembly)

    driver = Slot(IDriver, allow_none=True,
                    desc="The top level Driver that manages execution of "
                    "this Assembly.")

    recorders = List(Slot(ICaseRecorder, required=False),
                     desc='Case recorders for iteration data'
                          ' (only valid at top level).')

    includes = List(Str, iotype='in', framework_var=True,
                    desc='Patterns for variables to include in the recorders'
                         ' (only valid at top level).')

    excludes = List(Str, iotype='in', framework_var=True,
                    desc='Patterns for variables to exclude from the recorders'
                         ' (only valid at top level).')

    def __init__(self):

        super(Assembly, self).__init__()

        self._pseudo_count = 0  # counter for naming pseudocomps
        self._pre_driver = None
        self._derivs_required = False
        self._unexecuted = []

        # data dependency graph. Includes edges for data
        # connections as well as for all driver parameters and
        # constraints/objectives.  This is the starting graph for
        # all later transformations.
        self._depgraph = DependencyGraph()
        self._reduced_graph = nx.DiGraph()
        self._setup_depgraph = None

        for name, trait in self.class_traits().items():
            if trait.iotype:  # input or output
                self._depgraph.add_boundary_var(self, name, iotype=trait.iotype)

        self._exprmapper = ExprMapper(self)
        self.J_input_keys = None
        self.J_output_keys = None

        # default Driver executes its workflow once
        self.add('driver', Driver())

        # we're the top Assembly only if we're the first instantiated
        set_as_top(self, first_only=True)

        # Assemblies automatically figure out their own derivatives, so
        # any boundary vars that are unconnected should be zero.
        self.missing_deriv_policy = 'assume_zero'

        self.includes = ['*']
        self.excludes = []

    @property
    def _top_driver(self):
        if self._pre_driver:
            return self._pre_driver
        return self.driver

    @rbac(('owner', 'user'))
    def set_itername(self, itername, seqno=0):
        """
        Set current 'iteration coordinates'. Overrides :class:`Component`
        to propagate to driver, and optionally set the initial count in the
        driver's workflow. Setting the initial count is typically done by
        :class:`CaseIterDriverBase` on a remote top level assembly.

        itername: string
            Iteration coordinates.

        seqno: int
            Initial execution count for driver's workflow.
        """
        super(Assembly, self).set_itername(itername)
        self._top_driver.set_itername(itername)
        if seqno:
            self._top_driver.workflow.set_initial_count(seqno)

    def find_referring_connections(self, name):
        """Returns a list of connections where the given name is referred
        to either in the source or the destination.
        """
        exprset = set(self._exprmapper.find_referring_exprs(name))
        return [(u, v) for u, v
                       in self._depgraph.list_connections(show_passthrough=True)
                                        if u in exprset or v in exprset]

    def find_in_workflows(self, name):
        """Returns a list of tuples of the form (workflow, index) for all
        workflows in the scope of this Assembly that contain the given
        component name.
        """
        wflows = []
        for item in self.list_containers():
            if item != name:
                obj = self.get(item)
                if isinstance(obj, Driver) and name in obj.workflow:
                    wflows.append((obj.workflow, obj.workflow.index(name)))
        return wflows

    def _add_after_parent_set(self, name, obj):
        if has_interface(obj, IComponent):
            self._depgraph.add_component(name, obj)
        elif has_interface(obj, IContainer) and name not in self._depgraph:
            t = self.get_trait(name)
            if t is not None:
                io = t.iotype
                if io:
                    # since we just removed this container and it was
                    # being used as an io variable, we need to put
                    # it back in the dep graph
                    self._depgraph.add_boundary_var(self, name, iotype=io)

    def add_trait(self, name, trait, refresh=True):
        """Overrides base definition of *add_trait* in order to
        update the depgraph.
        """
        super(Assembly, self).add_trait(name, trait, refresh)
        if trait.iotype and name not in self._depgraph:
            self._depgraph.add_boundary_var(self, name,
                                            iotype=trait.iotype)

    def rename(self, oldname, newname):
        """Renames a child of this object from oldname to newname."""
        self._check_rename(oldname, newname)
        conns = self.find_referring_connections(oldname)
        wflows = self.find_in_workflows(oldname)

        obj = self.remove(oldname)
        obj.name = newname
        self.add(newname, obj)

        # oldname has now been removed from workflows, but newname may be in the
        # wrong location, so force it to be at the same index as before removal
        for wflow, idx in wflows:
            wflow.remove(newname)
            wflow.add(newname, idx)

        old_rgx = re.compile(r'(\W?)%s.' % oldname)

        # recreate all of the broken connections after translating
        # oldname to newname
        for u, v in conns:
            self.connect(re.sub(old_rgx, r'\g<1>%s.' % newname, u),
                         re.sub(old_rgx, r'\g<1>%s.' % newname, v))

    def replace(self, target_name, newobj):
        """Replace one object with another, attempting to mimic the
        inputs and connections of the replaced object as much as possible.
        """
        tobj = getattr(self, target_name)

        if not tobj:
            self.add(target_name, newobj)
            return

        # Save existing driver references.
        refs = {}
        if has_interface(tobj, IComponent):
            for cname in self.list_containers():
                obj = getattr(self, cname)
                if obj is not tobj and has_interface(obj, IDriver):
                    refs[cname] = obj.get_references(target_name)
                    #obj.remove_references(target_name)

        if hasattr(newobj, 'mimic'):
            try:
                # this should copy inputs, delegates and set name
                newobj.mimic(tobj)
            except Exception:
                self.reraise_exception("Couldn't replace '%s' of type %s with"
                                       " type %s"
                                       % (target_name, type(tobj).__name__,
                                          type(newobj).__name__), sys.exc_info())

        exprconns = [(u, v) for u, v in self._exprmapper.list_connections()
                                 if '_pseudo_' not in u and '_pseudo_' not in v]
        conns = self.find_referring_connections(target_name)
        wflows = self.find_in_workflows(target_name)

        # Assemblies sometimes create inputs and outputs in their configure()
        # function, so call it early if possible
        if self._call_cpath_updated is False and isinstance(obj, Container):
            newobj.parent = self
            newobj.name = target_name
            newobj.cpath_updated()

        # check that all connected vars exist in the new object
        req_vars = set([u.split('.', 1)[1].split('[', 1)[0]
                        for u, v in conns if u.startswith(target_name+'.')])
        req_vars.update([v.split('.', 1)[1].split('[', 1)[0]
                         for u, v in conns if v.startswith(target_name+'.')])
        missing = [v for v in req_vars if not newobj.contains(v)]
        if missing:
            self._logger.warning("the following variables are connected to "
                                 "other components but are missing in "
                                 "the replacement object: %s" % missing)

        # remove expr connections
        for u, v in exprconns:
            self.disconnect(u, v)

        # remove any existing connections to replacement object
        if has_interface(newobj, IComponent):
            self.disconnect(newobj.name)

        self.add(target_name, newobj)  # this will remove the old object
                                       # and any connections to it

        # recreate old connections
        for u, v in exprconns:
            try:
                self.connect(u, v)
            except Exception as err:
                self._logger.warning("Couldn't connect '%s' to '%s': %s",
                                     u, v, err)

        # Restore driver references.
        for dname, _refs in refs.items():
            drv = getattr(self, dname)
            drv.remove_references(target_name)
            drv.restore_references(_refs)

        # add new object (if it's a Component) to any
        # workflows where target was
        if has_interface(newobj, IComponent):
            for wflow, idx in wflows:
                wflow.add(target_name, idx)

    def remove(self, name):
        """Remove the named container object from this assembly
        and remove it from its workflow(s) if it's a Component
        or pseudo component.
        """
        obj = getattr(self, name)
        if has_interface(obj, IComponent) or isinstance(obj, PseudoComponent):
            for cname in self.list_containers():
                cobj = getattr(self, cname)
                if isinstance(cobj, Driver) and cobj is not obj:
                    cobj.remove_references(name)
            self.disconnect(name)
        elif name in self.list_inputs() or name in self.list_outputs():
            self.disconnect(name)

        if has_interface(obj, IDriver):
            for pcomp in obj.list_pseudocomps():
                if pcomp in self._depgraph:
                    self._depgraph.remove(pcomp)

        if name in self._depgraph:
            self._depgraph.remove(name)

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
            self.raise_exception("'%s' already exists" % newname, KeyError)
        if len(parts) < 2:
            self.raise_exception('destination of passthrough must be a dotted'
                                 ' path', NameError)
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
            # no validate function, so just don't use trait for validation
            trait = None

        metadata = self.get_metadata(pathname)
        metadata['target'] = pathname
        metadata['default_value'] = trait.trait_type.default_value
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

        # Copy trait value according to 'copy' attribute in the trait
        val = self.get(pathname)

        ttype = trait.trait_type
        if ttype.copy:
            # Variable trees need to point to a new parent.
            # Also, let's not deepcopy the outside universe
            if isinstance(val, Container):
                val_copy = val.copy()
                val_copy.parent = self
                val = val_copy
                val.name = newname
            else:
                val = _copydict[ttype.copy](val)

        setattr(self, newname, val)

        try:
            if iotype == 'in':
                self.connect(newname, pathname)
            else:
                self.connect(pathname, newname)
        except RuntimeError:
            info = sys.exc_info()
            self.remove(newname)
            raise info[0], info[1], info[2]

        return newtrait

    def get_passthroughs(self):
        ''' Get all the inputs and outputs of the assembly's child components
        and indicate for each whether or not it is a passthrough variable.
        If it is a passthrough, provide the assembly's name for the variable.
        '''
        inputs = {}
        outputs = {}
        passthroughs = {}

        for name in self.list_inputs() + self.list_outputs():
            target = self.get_metadata(name, 'target')
            if target is not None:
                passthroughs[target] = name

        for comp in self.list_components():
            inputs[comp] = {}
            input_vars = self.get(comp).list_inputs()
            for var_name in input_vars:
                var_path = '.'.join((comp, var_name))
                if var_path in passthroughs:
                    inputs[comp][var_name] = passthroughs[var_path]
                else:
                    inputs[comp][var_name] = False

            outputs[comp] = {}
            output_vars = self.get(comp).list_outputs()
            for var_name in output_vars:
                var_path = '.'.join((comp, var_name))
                if var_path in passthroughs:
                    outputs[comp][var_name] = passthroughs[var_path]
                else:
                    outputs[comp][var_name] = False

        return {
            'inputs': inputs,
            'outputs': outputs
        }

    @rbac(('owner', 'user'))
    def check_config(self, strict=False):
        """
        Verify that this component and all of its children are properly
        configured to execute. This function is called prior the first
        component execution.  If strict is True, any warning or error
        should raise an exception.

        If you override this function to do checks specific to your class,
        you must call this function.
        """

        super(Assembly, self).check_config(strict=strict)
        self._check_input_collisions()
        self._check_unset_req_vars()
        self._check_unexecuted_comps(strict)

    def _check_input_collisions(self):
        graph = self._depgraph
        dests = set([v for u, v in self.list_connections() if 'drv_conn_ext' not in graph[u][v]])
        allbases = set([base_var(graph, v) for v in dests])
        unconnected_bases = allbases - dests
        connected_bases = allbases - unconnected_bases

        collisions = []
        for drv in chain([self._top_driver],
                          self._top_driver.subdrivers(recurse=True)):
            if has_interface(drv, IHasParameters):
                for target in drv.list_param_targets():
                    tbase = base_var(graph, target)
                    if target == tbase:  # target is a base var
                        if target in allbases:
                            collisions.append("%s in %s"
                                              % (target, drv.get_pathname()))
                    else:  # target is a sub var
                        if target in dests or tbase in connected_bases:
                            collisions.append("%s in %s"
                                              % (target, drv.get_pathname()))

        if collisions:
            self.raise_exception("The following parameters collide with"
                                 " connected inputs: %s" % ','.join(collisions),
                                 RuntimeError)

    def _check_unexecuted_comps(self, strict):
        self._unexecuted = []
        cgraph = self._depgraph.component_graph()
        wfcomps = set([c.name for c in self.driver.iteration_set()])
        wfcomps.add('driver')
        diff = set(cgraph.nodes()) - wfcomps
        self._pre_driver = None
        if diff:
            msg = "The following components are not in any workflow but " \
                  "are needed by other workflows"
            if strict_chk_config(strict):
                errfunct = self.raise_exception
            else:
                errfunct = self._logger.warning
                msg += ", so they will be executed once per execution of " \
                       "this Assembly"

            out_edges = nx.edge_boundary(cgraph, diff)
            pre = [u for u, v in out_edges]
            post = diff - set(pre)

            if pre:
                msg += ": %s" % pre
                errfunct(msg)

                ## HACK ALERT!
                ## If there are upstream comps that are not in any workflow,
                ## create a hidden top level driver called _pre_driver. That
                ## driver will be executed once per execution of the Assembly.

                # can't call add here for _pre_driver because that calls
                # config_changed...
                self._pre_driver = Driver()
                self._pre_driver.parent = self
                pre.append('driver') # run the normal top driver after running the 'pre' comps
                self._pre_driver.workflow.add(pre)
                self._pre_driver.name = '_pre_driver'
                self._depgraph.add_node('_pre_driver', comp=True, driver=True)

            if post:
                errfunct("The following components are not in any workflow"
                         " and WILL NOT EXECUTE: %s" % list(diff))
                self._unexecuted = list(post)

    def _check_unset_req_vars(self):
        """Find 'required' variables that have not been set."""
        graph = self._depgraph
        for name in chain(all_comps(graph),
                          graph.get_boundary_inputs(),
                          graph.get_boundary_outputs()):
            obj = getattr(self, name)
            if has_interface(obj, IContainer):
                for vname in obj.get_req_default(self.trait(name).required):
                    # each var must be connected, otherwise value will not
                    # be set to a non-default value
                    base = base_var(graph, vname)
                    indeg = graph.in_degree(base)
                    io = graph.node[base]['iotype']
                    if (io == 'in' and indeg < 1) or \
                       (io == 'state' and indeg < 2):
                        self.raise_exception("required variable '%s' was"
                                             " not set" % vname, RuntimeError)

    @rbac(('owner', 'user'))
    def connect(self, src, dest):
        """Connect one src expression to one destination expression. This could
        be a normal connection between variables from two internal Components,
        or it could be a passthrough connection, which connects across the scope
        boundary of this object.  When a pathname begins with 'parent.', that
        indicates it is referring to a Variable outside of this object's scope.

        src: str
            Source expression string.

        dest: str or list(str)
            Destination expression string(s).
        """
        src = eliminate_expr_ws(src)

        if isinstance(dest, basestring):
            dest = (dest,)
        for dst in dest:
            dst = eliminate_expr_ws(dst)
            try:
                self._connect(src, dst)
            except Exception:
                self.reraise_exception("Can't connect '%s' to '%s'" % (src, dst),
                                        sys.exc_info())

    def _connect(self, src, dest):
        """Handle one connection destination. This should only be called via
        the connect() function, never directly.
        """

        # Among other things, check if already connected.
        srcexpr, destexpr, pcomp_type = \
                   self._exprmapper.check_connect(src, dest, self)

        if pcomp_type is not None:
            if pcomp_type == 'units':
                pseudocomp = UnitConversionPComp(self, srcexpr, destexpr,
                                                 pseudo_type=pcomp_type)
            else:
                pseudocomp = PseudoComponent(self, srcexpr, destexpr,
                                             pseudo_type=pcomp_type)
            self.add(pseudocomp.name, pseudocomp)
            pseudocomp.make_connections(self)
        else:
            pseudocomp = None
            self._depgraph.check_connect(src, dest)
            dcomps = destexpr.get_referenced_compnames()
            scomps = srcexpr.get_referenced_compnames()
            for dname in dcomps:
                if dname in scomps:
                    self.raise_exception("Can't connect '%s' to '%s'. Both"
                                         " refer to the same component." %
                                         (src, dest), RuntimeError)
            for cname in chain(dcomps, scomps):
                comp = getattr(self, cname)
                if has_interface(comp, IComponent):
                    comp.config_changed(update_parent=False)

            for vname in chain(srcexpr.get_referenced_varpaths(copy=False),
                               destexpr.get_referenced_varpaths(copy=False)):
                if not self.contains(vname):
                    self.raise_exception("Can't find '%s'" % vname,
                                         AttributeError)

            self._depgraph.connect(self, src, dest)

        self._exprmapper.connect(srcexpr, destexpr, self, pseudocomp)

        self.config_changed(update_parent=False)

    @rbac(('owner', 'user'))
    def disconnect(self, varpath, varpath2=None):
        """If varpath2 is supplied, remove the connection between varpath and
        varpath2. Otherwise, if varpath is the name of a trait, remove all
        connections to/from varpath in the current scope. If varpath is the
        name of a Component, remove all connections from all of its inputs
        and outputs.
        """
        try:
            cnames = ExprEvaluator(varpath, self).get_referenced_compnames()
            if varpath2 is not None:
                cnames.update(ExprEvaluator(varpath2, self).get_referenced_compnames())
            boundary_vars = self.list_inputs() + self.list_outputs()
            for cname in cnames:
                if cname not in boundary_vars:
                    getattr(self, cname).config_changed(update_parent=False)

            to_remove, pcomps = self._exprmapper.disconnect(varpath, varpath2)

            graph = self._depgraph

            if to_remove:
                for u, v in graph.list_connections():
                    if (u, v) in to_remove:
                        graph.disconnect(u, v)
                        to_remove.remove((u, v))

            if to_remove:  # look for pseudocomp expression connections
                for node, data in graph.nodes_iter(data=True):
                    if 'srcexpr' in data:
                        for u, v in to_remove:
                            if data['srcexpr'] == u or data['destexpr'] == v:
                                pcomps.add(node)

            for name in pcomps:
                if '_pseudo_' not in varpath:
                    self.remove(name)
                else:
                    try:
                        self.remove_trait(name)
                    except Exception:
                        pass
                try:
                    graph.remove(name)
                except (KeyError, nx.exception.NetworkXError):
                    pass
        finally:
            self.config_changed(update_parent=False)

    def config_changed(self, update_parent=True):
        """Call this whenever the configuration of this Component changes,
        for example, children are added or removed, connections are made
        or removed, etc.
        """
        super(Assembly, self).config_changed(update_parent)

        # drivers must tell workflows that config has changed because
        # dependencies may have changed
        for name in self.list_containers():
            cont = getattr(self, name)
            if isinstance(cont, Driver):
                cont.config_changed(update_parent=False)

        self._pre_driver = None
        self.J_input_keys = self.J_output_keys = None
        self._system = None


    def _set_failed(self, path, value, index=None, force=False):
        parts = path.split('.', 1)
        if len(parts) > 1:
            obj = getattr(self, parts[0])
            if isinstance(obj, PseudoComponent):
                obj.set(parts[1], value, index, force)

    def execute(self):
        """Runs driver and updates our boundary variables."""
        for system in self._system.local_subsystems():
            system.pre_run()
        #self._system.vec['u'].set_from_scope(self)
        self._system.run(self.itername, ffd_order=self.ffd_order,
                         case_uuid=self._case_uuid)

    def configure_recording(self, includes=None, excludes=None, inputs=None):
        """Called at start of top-level run to configure case recording.
        Returns set of paths for changing inputs."""
        if self.parent is None:
            if self.recorders:
                includes = self.includes
                excludes = self.excludes
                for recorder in self.recorders:
                    recorder.startup()
            else:
                includes = excludes = None

        # Determine (changing) inputs and outputs to record
        inputs = set()
        constants = {}
        for name in self.list_containers():
            obj = getattr(self, name)
            if has_interface(obj, IDriver, IAssembly):
                inps, consts = obj.configure_recording(includes, excludes)
                inputs.update(inps)
                constants.update(consts)

        # If nothing to record, return after configuring workflows.
        if not includes:
            return (inputs, constants)

        # Locate top level assembly.
        top = self
        while top.parent:
            top = top.parent
        prefix_drop = len(top.name)+1 if top.name else 0

        # Determine constant inputs.
        objs = [self]
        objs.extend(getattr(self, name) for name in self.list_containers())
        for obj in objs:
            if has_interface(obj, IComponent):
                prefix = obj.get_pathname()[prefix_drop:]
                if prefix:
                    prefix += '.'

                in_names = obj.list_inputs()
                if obj.parent is not None:
                    conn = obj.parent.connected_inputs(obj.name)
                    for name in conn:
                        obj_name, _, in_name = name.partition('.')
                        if in_name in in_names:
                            in_names.remove(in_name)

                for name in in_names:
                    path = prefix+name
                    if path in inputs:
                        continue  # Changing input.
                    for pattern in includes:
                        if fnmatch(path, pattern):
                            break
                    else:
                        continue  # Not to be included.
                    for pattern in excludes:
                        if fnmatch(path, pattern):
                            break # Excluded.
                    else:
                        val = getattr(obj, name)
                        if isinstance(val, VariableTree):
                            for path, val in self._expand_tree(path, val):
                                constants[path] = val
                        else:
                            constants[path] = val

        # Record constant inputs.
        if self.parent is None:
            for recorder in self.recorders:
                recorder.record_constants(constants)

        return (inputs, constants)

    @rbac(('owner', 'user'))
    def connected_inputs(self, name):
        """Helper for :meth:`configure_recording`."""
        return self._depgraph.list_inputs(name, connected=True)

    def _expand_tree(self, path, tree):
        """Return list of ``(path, value)`` with :class:`VariableTree`
        expanded."""
        path += '.'
        result = []
        for name, val in tree._items(set()):
            if isinstance(val, VariableTree):
                result.extend(self._expand_tree(path+name, val))
            else:
                result.append((path+name, val))
        return result

    def stop(self):
        """Stop the calculation."""
        self._top_driver.stop()

    @rbac(('owner', 'user'))
    def child_config_changed(self, child, adding=True, removing=True):
        """A child has changed its input lists and/or output lists,
        so we need to update the graph.
        """
        # if this is called during __setstate__, self._depgraph may not
        # exist yet, so...
        if hasattr(self, '_depgraph'):
            self._depgraph.child_config_changed(child, adding=adding,
                                                removing=removing)

    def list_connections(self, show_passthrough=True,
                               visible_only=False,
                               show_expressions=False):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = self._depgraph.list_connections(show_passthrough=show_passthrough)
        if visible_only:
            newconns = []
            for u, v in conns:
                if u.startswith('_pseudo_'):
                    pcomp = getattr(self, u.split('.', 1)[0])
                    newconns.extend(pcomp.list_connections(is_hidden=True,
                                     show_expressions=show_expressions))
                elif v.startswith('_pseudo_'):
                    pcomp = getattr(self, v.split('.', 1)[0])
                    newconns.extend(pcomp.list_connections(is_hidden=True,
                                     show_expressions=show_expressions))
                else:
                    newconns.append((u, v))
            return newconns
        return conns

    @rbac(('owner', 'user'))
    def child_run_finished(self, childname, outs=None):
        """Called by a child when it completes its run() function."""
        self._depgraph.child_run_finished(childname, outs)

    def exec_counts(self, compnames):
        return [getattr(self, c).exec_count for c in compnames]

    def check_gradient(self, name=None, inputs=None, outputs=None,
                       stream=sys.stdout, mode='auto',
                       fd_form='forward', fd_step=1.0e-6,
                       fd_step_type='absolute'):

        """Compare the OpenMDAO-calculated gradient with one calculated
        by straight finite-difference. This provides the user with a way
        to validate his derivative functions (apply_deriv and provideJ.)
        Note that fake finite difference is turned off so that we are
        doing a straight comparison.

        name: (optional) str
            If provided, specifies the name of a Driver or Component to
            calculate the gradient for.  If name specifies a Driver,
            the inputs used to calculate the gradient will be generated
            from the parameters of the Driver, and the outputs will be
            generated from the constraints and objectives of the Driver.
            If name specifies a Component, the inputs and outputs of that
            Component will be used to calculate the gradient.

        inputs: (optional) iter of str or None
            Names of input variables. The calculated gradient will be
            the matrix of values of the output variables with respect
            to these input variables. If no value is provided for inputs,
            they will be determined based on the 'name' argument.
            If the inputs are not specified and name is not specified,
            then they will be generated from the parameters of
            the object named 'driver'.

        outputs: (optional) iter of str or None
            Names of output variables. The calculated gradient will be
            the matrix of values of these output variables with respect
            to the input variables. If no value is provided for outputs,
            they will be determined based on the 'name' argument.
            If the outputs are not specified and name is not specified,
            then they will be generated from the objectives and constraints
            of the object named 'driver'.

        stream: (optional) file-like object, str, or None
            Where to write to, default stdout. If a string is supplied,
            that is used as a filename. If None, no output is written.

        mode: (optional) str or None
            Set to 'forward' for forward mode, 'adjoint' for adjoint mode,
            or 'auto' to let OpenMDAO determine the correct mode.
            Defaults to 'auto'.

        fd_form: str
            Finite difference mode. Valid choices are 'forward', 'adjoint' ,
            'central'. Default is 'forward'

        fd_step: float
            Default step_size for finite difference. Default is 1.0e-6.

        fd_step_type: str
            Finite difference step type. Set to 'absolute' or 'relative'.
            Default is 'absolute'.

        Returns the finite difference gradient, the OpenMDAO-calculated
        gradient, a list of the gradient names, and a list of suspect
        inputs/outputs.
        """
        driver = self.driver
        obj = None

        # tuples cause problems.
        if inputs:
            inputs = list(inputs)
        if outputs:
            outputs = list(outputs)

        if inputs and outputs:
            if name:
                logger.warning("The 'inputs' and 'outputs' args were specified"
                               " to check_gradient, so the 'name' arg (%s) is"
                               " ignored.", name)
        elif not name:
            # we're missing either inputs or outputs, so we need a name
            name = 'driver'

        if name:
            obj = getattr(self, name, None)
            if obj is None:
                self.raise_exception("Can't find object named '%s'." % name)
            if has_interface(obj, IDriver):
                driver = obj

        # fill in missing inputs or outputs using the object specified by 'name'
        if not inputs:
            if has_interface(obj, IDriver):
                pass  # workflow.check_gradient can pull inputs from driver
            elif has_interface(obj, IAssembly):
                inputs = ['.'.join((obj.name, inp))
                          for inp in obj.list_inputs()
                                  if is_differentiable_var(inp, obj)]
                inputs = sorted(inputs)
            elif has_interface(obj, IComponent):
                inputs = ['.'.join((obj.name, inp))
                          for inp in list_deriv_vars(obj)[0]]
                inputs = sorted(inputs)
            else:
                self.raise_exception("Can't find any inputs for generating"
                                     " gradient.")
        if not outputs:
            if has_interface(obj, IDriver):
                pass  # workflow.check_gradient can pull outputs from driver
            elif has_interface(obj, IAssembly):
                outputs = ['.'.join((obj.name, out))
                           for out in obj.list_outputs()
                                   if is_differentiable_var(out, obj)]
                outputs = sorted(outputs)
            elif has_interface(obj, IComponent):
                outputs = ['.'.join((obj.name, outp))
                          for outp in list_deriv_vars(obj)[1]]
                outputs = sorted(outputs)
            else:
                self.raise_exception("Can't find any outputs for generating"
                                     " gradient.")

        if not has_interface(obj, IDriver) and (not inputs or not outputs):
            msg = 'Component %s has no analytic derivatives.' % obj.name
            self.raise_exception(msg)

        base_fd_form = driver.gradient_options.fd_form
        base_fd_step = driver.gradient_options.fd_step
        base_fd_step_type = driver.gradient_options.fd_step_type

        driver.gradient_options.fd_form = fd_form
        driver.gradient_options.fd_step = fd_step
        driver.gradient_options.fd_step_type = fd_step_type

        try:
            result = driver.workflow.check_gradient(inputs=inputs,
                                                    outputs=outputs,
                                                    stream=stream,
                                                    mode=mode)
        finally:
            driver.gradient_options.fd_form = base_fd_form
            driver.gradient_options.fd_step = base_fd_step
            driver.gradient_options.fd_step_type = base_fd_step_type

        return result

    def list_components(self):
        ''' List the components in the assembly.
        '''
        names = [name for name in self.list_containers()
                     if isinstance(self.get(name), Component)]
        return names

    def all_wflows_order(self):
        """Returns a list of component names over all workflows in an iteration
        hierarchy.  Shows the actual Assembly-wide order of execution of
        components in the Assembly.  Note that a given component will appear
        multiple times if that component is a member of multiple workflows.
        """

        def _all_wflows_order(drv):
            comps = [drv.name]
            for comp in drv.workflow:
                if has_interface(comp, IDriver):
                    comps.extend(_all_wflows_order(comp))
                else:
                    comps.append(comp.name)
            return comps

        return _all_wflows_order(self._top_driver)

    @rbac(('owner', 'user'))
    def new_pseudo_name(self):
        name = "_pseudo_%d" % self._pseudo_count
        self._pseudo_count += 1
        return name

    def get_dataflow(self):
        ''' Get a dictionary of components and the connections between them
            that make up the data flow for the assembly;
            also includes parameter, constraint, and objective flows.
        '''
        components  = []
        connections = []
        parameters  = []
        constraints = []
        objectives  = []
        responses   = []

        # list of components (name & type) in the assembly
        names = self._depgraph.order_components(all_comps(self._depgraph))

        # Bubble-up drivers ahead of their parameter targets.
        sorted_names = []
        for name in names:
            comp = self.get(name)
            if is_instance(comp, Driver) and hasattr(comp, '_delegates_'):
                driver_index = len(sorted_names)
                for dname in comp._delegates_:
                    inst = getattr(comp, dname)
                    if isinstance(inst, HasParameters):
                        refs = inst.get_referenced_compnames()
                        for ref in refs:
                            try:
                                target_index = sorted_names.index(ref)
                            except ValueError:
                                pass
                            else:
                                driver_index = min(driver_index, target_index)
                sorted_names.insert(driver_index, name)
            else:
                sorted_names.append(name)

        # Process names in new order.
        for name in sorted_names:
            comp = self.get(name)
            if is_instance(comp, Component):
                inames = [cls.__name__
                          for cls in list(implementedBy(comp.__class__))]
                components.append({
                    'name':       comp.name,
                    'pathname':   comp.get_pathname(),
                    'type':       type(comp).__name__,
                    'interfaces': inames,
                    'python_id':  id(comp)
                })

            if is_instance(comp, Driver):
                if hasattr(comp, '_delegates_'):
                    for name, dclass in comp._delegates_.items():
                        inst = getattr(comp, name)
                        if isinstance(inst, HasParameters):
                            for name, param in inst.get_parameters().items():
                                if isinstance(param, ParameterGroup):
                                    for n, p in zip(name, tuple(param.targets)):
                                        parameters.append([comp.name + '.' + n, p])
                                else:
                                    parameters.append([comp.name + '.' + name,
                                                       param.target])
                        elif isinstance(inst, (HasConstraints,
                                               HasEqConstraints,
                                               HasIneqConstraints)):
                            for path in inst.get_referenced_varpaths():
                                name, _, rest = path.partition('.')
                                constraints.append([path,
                                                    comp.name + '.' + rest])
                        elif isinstance(inst, (HasObjective,
                                               HasObjectives)):
                            for path in inst.get_referenced_varpaths():
                                name, _, rest = path.partition('.')
                                objectives.append([path,
                                                   comp.name + '.' + name])
                        elif isinstance(inst, HasResponses):
                            for path in inst.get_referenced_varpaths():
                                name, _, rest = path.partition('.')
                                responses.append([path,
                                                  comp.name + '.' + name])

        # list of connections (convert tuples to lists)
        conntuples = self.list_connections(show_passthrough=True,
                                           visible_only=True)
        for connection in conntuples:
            connections.append(list(connection))

        return {'components': components, 'connections': connections,
                'parameters': parameters, 'constraints': constraints,
                'objectives': objectives, 'responses': responses}

    def get_connectivity(self):
        ''' Get a list of all the inputs and outputs that can be
            connected in this assembly, and the connections between them.
            This includes expressions represented by PseudoComponents.
        '''

        # connectivity data
        connectivity = {
            'nodes': {},
            'edges': []
        }

        # populate input and output nodes
        for cname in self.list_containers():
            cont = self.get(cname)
            if isinstance(cont, Component):
                for vname in cont.list_outputs():
                    var = cont.get(vname)
                    vtype = type(var).__name__
                    if not '.' in vname:  # vartree vars handled separately
                        full_name = '%s.%s' % (cname, vname)
                        meta = cont.get_metadata(vname)
                        if meta and 'units' in meta:
                            units = meta['units']
                        else:
                            units = ''
                        connectivity['nodes'][full_name] = {
                            'type': vtype,
                            'units': units,
                            'io':   'output'
                        }
                    if isinstance(var, VariableTree):
                        for child_name in var.list_vars():
                            child_var = var.get(child_name)
                            full_name = '%s.%s.%s' % (cname, vname, child_name)
                            meta = var.get_metadata(child_name)
                            if meta and 'units' in meta:
                                units = meta['units']
                            else:
                                units = ''
                            connectivity['nodes'][full_name] = {
                                'type':  type(child_var).__name__,
                                'units': units,
                                'io':   'output'
                            }
                    elif vtype == 'ndarray':
                        for idx in range(0, len(var)):
                            full_name = '%s.%s[%s]' % (cname, vname, idx)
                            units = ''
                            connectivity['nodes'][full_name] = {
                                'type':  type(var[0]).__name__,
                                'units': units,
                                'io':   'output'
                            }

                for vname in cont.list_inputs():
                    var = cont.get(vname)
                    vtype = type(var).__name__
                    if not '.' in vname:  # vartree vars handled separately
                        full_name = '%s.%s' % (cname, vname)
                        meta = cont.get_metadata(vname)
                        if meta and 'units' in meta:
                            units = meta['units']
                        else:
                            units = ''
                        connectivity['nodes'][full_name] = {
                            'type': vtype,
                            'units': units,
                            'io':   'input'
                        }
                    if isinstance(var, VariableTree):
                        for child_name in var.list_vars():
                            child_var = var.get(child_name)
                            full_name = '%s.%s.%s' % (cname, vname, child_name)
                            meta = var.get_metadata(child_name)
                            if meta and 'units' in meta:
                                units = meta['units']
                            else:
                                units = ''
                            connectivity['nodes'][full_name] = {
                                'type':  type(child_var).__name__,
                                'units': units,
                                'io':   'input'
                            }
                    elif vtype == 'ndarray':
                        for idx in range(0, len(var)):
                            full_name = '%s.%s[%s]' % (cname, vname, idx)
                            units = ''
                            connectivity['nodes'][full_name] = {
                                'type':  type(var[0]).__name__,
                                'units': units,
                                'io':   'input'
                            }

        # add assembly vars, which can be input or output (due to passthroughs)
        var_names = self.list_outputs() + self.list_inputs()
        for vname in var_names:
            var = self.get(vname)
            vtype = type(var).__name__
            if not '.' in vname:  # vartree vars handled separately
                full_name = vname
                meta = self.get_metadata(vname)
                if meta and 'units' in meta:
                    units = meta['units']
                else:
                    units = ''
                connectivity['nodes'][full_name] = {
                    'type': vtype,
                    'units': units,
                    'io':   'io'
                }
            if isinstance(var, VariableTree):
                for child_name in var.list_vars():
                    child_var = var.get(child_name)
                    full_name = vname + '.' + child_name
                    meta = var.get_metadata(child_name)
                    if meta and 'units' in meta:
                        units = meta['units']
                    else:
                        units = ''
                    connectivity['nodes'][full_name] = {
                        'type':  type(child_var).__name__,
                        'units': units,
                        'io':   'io'
                    }
            elif vtype == 'ndarray':
                for idx in range(0, len(var)):
                    full_name = vname + '[' + str(idx) + ']'
                    units = ''
                    connectivity['nodes'][full_name] = {
                        'type':  type(var[0]).__name__,
                        'units': units,
                        'io':   'io'
                    }

        # populate expression nodes and edges
        for (source, target) in self.list_connections():
            if source.startswith('_pseudo_'):
                pname = source.split('.', 1)[0]
                pcomp = getattr(self, pname)
                if pcomp._pseudo_type in ['multi_var_expr', 'units']:
                    source = pcomp._orig_src  # units source will be orig var
                    if source not in connectivity['nodes'].keys():
                        units = pcomp.get_metadata(pcomp.list_outputs()[0],
                                                   'units')
                        if not units:
                            units = ''
                        connectivity['nodes'][source] = {
                            'type': 'expr',
                            'units': units,
                            'io':   'io',
                        }

            if target.startswith('_pseudo_'):
                pname = target.split('.', 1)[0]
                pcomp = getattr(self, pname)
                if pcomp._pseudo_type in ['multi_var_expr']:
                    target = pcomp._orig_src
                    if target not in connectivity['nodes'].keys():
                        units = pcomp.get_metadata(pcomp.list_outputs()[0],
                                                   'units')
                        if not units:
                            units = ''
                        connectivity['nodes'][target] = {
                            'type': 'expr',
                            'units': units,
                            'io': 'io'
                        }

            if not source.startswith('_pseudo_') and \
               not target.startswith('_pseudo_'):
                # ignore other types of PseudoComponents (objectives, etc)
                connectivity['edges'].append([source, target])

        return connectivity

    # def _repr_svg_(self):
    #     """ Returns an SVG representation of this Assembly's dependency graph
    #         Note: the graph_to_svg() function currently uses tkinter which
    #               requires a display and thus will cause an exception when
    #               running headless (e.g. during non-interactive testing)
    #     """
    #     return graph_to_svg(self._depgraph.component_graph())

    def get_depgraph(self):
        return self._depgraph

    def get_reduced_graph(self):
        return self._reduced_graph

    def get_comps(self):
        """Returns a list of all of objects contained in this
        Assembly implementing the IComponent interface.
        """
        conts = [getattr(self, n) for n in sorted(self.list_containers())]
        return [c for c in conts if has_interface(c, IComponent)]

    def get_iteration_tree(self):
        return self._top_driver.get_iteration_tree()

    def get_system(self):
        return self._system

    @rbac(('owner', 'user'))
    def setup_systems(self):
        rgraph = self._reduced_graph

        # store metadata (size, etc.) for all relevant vars
        self._var_meta = self._get_all_var_metadata(self._reduced_graph)

        # create systems for all simple components
        for node, data in rgraph.nodes_iter(data=True):
            if 'comp' in data:
                data['system'] = _create_simple_sys(self, rgraph, node)
                
        for name in self._unexecuted:
            comp = getattr(self, name)
            if name not in rgraph and has_interface(comp, IDriver) or has_interface(comp, IAssembly):
                comp.setup_systems()

        self._top_driver.setup_systems()
        
        # copy the reduced graph
        rgraph = rgraph.subgraph(rgraph.nodes_iter())

        collapse_subdrivers(rgraph, [], [self._top_driver])

        cgraph = reduced2component(rgraph)

        if len(cgraph) > 1:
            for u,v in cgraph.edges():
                if u == v:  # get rid of self cycles
                    cgraph.remove_edge(u, v)
            self._system = SerialSystem(self, rgraph, cgraph, self.name+'._inner_asm')
            self._system.set_ordering(nx.topological_sort(cgraph), {})
        else:
            # TODO: if top driver has no params/constraints, possibly
            # remove driver system entirely and just go directly to workflow
            # system...
            self._system = rgraph.node[self._top_driver.name]['system']

        # assemblies don't add systems to their parent
        # graph, so return an empty tuple
        return ()

    @rbac(('owner', 'user'))
    def get_req_cpus(self):
        """Return requested_cpus"""
        return self._top_driver.get_req_cpus()

    def setup_communicators(self, comm):
        self._system.setup_communicators(comm)

    def setup_variables(self):
        self._system.setup_variables()

    def setup_sizes(self):
        """Calculate the local sizes of all relevant variables
        and share those across all processes in the communicator.
        """
        # find all local systems
        sys_stack = [self._system]
        loc_comps = []

        while sys_stack:
            system = sys_stack.pop()
            loc_comps.extend([s.name for s in system.simple_subsystems()
                                    if s._comp is not None])
            sys_stack.extend(system.local_subsystems())

        loc_comps = set(loc_comps)
        loc_comps.add(None)

        # loop over all component inputs and boundary outputs and
        # set them to their sources so that they'll be sized properly
        for node, data in self._reduced_graph.nodes_iter(data=True):
            if 'comp' not in data:
                src = node[0]
                sval, idx = get_val_and_index(self, src)
                if isinstance(sval, ndarray):
                    dests = node[1]
                    for dest in dests:
                        dcomp = dest.split('.',1)[0] if '.' in dest else None
                        if dcomp in loc_comps:
                            dval, didx = get_val_and_index(self, dest)
                            if isinstance(dval, ndarray):
                                if sval.shape != dval.shape:
                                    self.set(dest, sval)

        # this will calculate sizes for all subsystems
        self._system.setup_sizes()

    def setup_vectors(self, arrays=None):
        """Creates vector wrapper objects to manage local and
        distributed vectors need to solve the distributed system.
        """
        self._system.setup_vectors(None)

    def setup_scatters(self):
        self._system.setup_scatters()

    def setup_depgraph(self, dgraph=None):
        # we have our own depgraph to use
        dgraph = self._depgraph.subgraph(self._depgraph.nodes_iter())
        self._setup_depgraph = dgraph

        for comp in self.get_comps_and_pseudos():
            if has_interface(comp, IDriver) or has_interface(comp, IAssembly):
                comp.setup_depgraph(dgraph)

    def setup_reduced_graph(self, inputs=None, outputs=None):
        """Create the graph we need to do the breakdown of the model
        into Systems.
        """
        dgraph = self._setup_depgraph

        # keep all states
        # FIXME: I think this should only keep states of comps that are directly relevant...
        keep = set([n for n,d in dgraph.nodes_iter(data=True)
                         if d.get('iotype')=='state'])
                         #if d.get('iotype') in ('state','residual')])

        if self.parent is not None:
            self._derivs_required = self.parent._derivs_required
        else:
            self._derivs_required = False

        # figure out the relevant subgraph based on given inputs and outputs
        if not (inputs is None and outputs is None):
            self._derivs_required = True
            dsrcs, ddests = self._top_driver.get_expr_var_depends(recurse=True)
            keep.add(self._top_driver.name)
            keep.update([c.name for c in self._top_driver.iteration_set()])

            ## keep all boundary inputs/outputs, even if they're not connected
            #keep.update(self._flat(self.list_inputs()))
            #keep.update(self._flat(self.list_outputs()))

            if inputs is None:
                inputs = list(ddests)
            else:
                # fix any single tuples
                inputs = [fix_single_tuple(i) for i in inputs]

                # identify any broadcast inputs
                ins = []
                for inp in inputs:
                    if isinstance(inp, basestring):
                        keep.add(inp)
                        ins.append(inp)
                    else:
                        keep.update(inp)
                        ins.append(inp[0])
                inputs = ins

            if outputs is None:
                outputs = dsrcs

            dgraph = self._explode_vartrees(dgraph)
            
            # add any variables requested that don't exist in the graph
            for inp in inputs:
                if inp not in dgraph:
                    base = base_var(dgraph, inp)
                    for n in chain(dgraph.successors(base), dgraph.predecessors(base)):
                        if base_var(dgraph, n) != base:
                            keep.add(base)
                    dgraph.add_connected_subvar(inp)
                    
            for out in outputs:
                if out not in dgraph:
                    base = base_var(dgraph, out)
                    for n in chain(dgraph.successors(base), dgraph.predecessors(base)):
                        if base_var(dgraph, n) != base:
                            keep.add(base)
                    dgraph.add_connected_subvar(out)
                    
            dgraph = relevant_subgraph(dgraph, inputs, outputs,
                                       keep)
            
            self._remove_vartrees(dgraph)
            
            keep.update(inputs)
            keep.update(outputs)
        else:
            dgraph = self._explode_vartrees(dgraph)
            self._remove_vartrees(dgraph)

        fix_state_connections(self, dgraph)

        add_boundary_comps(dgraph)

        connect_subvars_to_comps(dgraph)

        # get rid of fake boundary comps
        dgraph.remove_nodes_from(['#in', '#out'])

        # collapse all connections into single nodes.
        collapsed_graph = collapse_connections(dgraph)
        
        fix_duplicate_dests(collapsed_graph)

        vars2tuples(dgraph, collapsed_graph)

        self.name2collapsed = map_collapsed_nodes(collapsed_graph)

        # add InVarSystems and OutVarSystems for boundary vars
        for node, data in collapsed_graph.nodes_iter(data=True):
            if 'boundary' in data and collapsed_graph.degree(node) > 0:
                if data.get('iotype') == 'in' and collapsed_graph.in_degree(node) == 0: # input boundary node
                    collapsed_graph.add_node(node[0].split('[',1)[0], comp='invar')
                    collapsed_graph.add_edge(node[0].split('[',1)[0], node)
                elif data.get('iotype') == 'out' and collapsed_graph.out_degree(node) == 0: # output bndry node
                    collapsed_graph.add_node(node[1][0].split('[',1)[0], comp='outvar')
                    collapsed_graph.add_edge(node, node[1][0].split('[',1)[0])

        # translate kept nodes to collapsed form
        coll_keep = set([self.name2collapsed.get(k,k) for k in keep])

        # remove all vars that don't connect components
        prune_reduced_graph(self._depgraph, collapsed_graph,
                            coll_keep)

        fix_dangling_vars(collapsed_graph)

        self._reduced_graph = collapsed_graph

        for comp in self.get_comps():
            if has_interface(comp, IDriver) and comp.requires_derivs():
                self._derivs_required = True
            if has_interface(comp, IAssembly):
                comp.setup_reduced_graph(inputs=_get_scoped_inputs(comp, dgraph),
                                         outputs=_get_scoped_outputs(comp, dgraph))


    def _get_var_info(self, node):
        """Collect any variable metadata from the
        model here.
        """
        info = { 'size': 0, 'flat': True }

        base = None

        if isinstance(node, tuple):
            # use the name of the src
            name = node[0]
        else:
            name = node

        parts = name.split('.',1)
        if len(parts) > 1:
            cname, vname = parts
            child = getattr(self, cname)
        else:
            cname, vname = '', name
            child = self

        try:
            # TODO: add checking of local_size metadata...
            parts = vname.split('.')
            val, idx = get_val_and_index(child, vname)

            if hasattr(val, 'shape'):
                info['shape'] = val.shape

            if '[' in vname:  # array index into basevar
                base = vname.split('[',1)[0]
                flat_idx = get_flattened_index(idx,
                                        get_var_shape(base, child))
            else:
                base = None
                flat_idx = None

            info['size'] = flattened_size(vname, val, scope=child)
            if flat_idx is not None:
                info['flat_idx'] = flat_idx

        except NoFlatError:
            info['flat'] = False

        if base is not None:
            if cname:
                bname = '.'.join((cname, base))
            else:
                bname = base
            info['basevar'] = bname

        # get any other metadata we want
        for mname in ['deriv_ignore']:
            meta = child.get_metadata(vname, mname)
            if meta is not None:
                info[mname] = meta

        return info

    def _flat(self, lst):
        return [n for n in lst if self._get_var_info(n).get('flat')]

    def _get_all_var_metadata(self, graph):
        """Collect size, shape, etc. info for all variables referenced
        in the graph.  This info can then be used by all subsystems
        contained in this Assembly.
        """
        varmeta = {}
        for node, data in graph.nodes_iter(data=True):
            if 'comp' not in data and node not in varmeta:
                meta = self._get_var_info(node)
                varmeta[node] = meta
                for name in simple_node_iter(node):
                    if name not in varmeta:
                        varmeta[name] = meta

        # there are cases where a component will return names from
        # its list_deriv_vars that are not found in the graph, so add them
        # all here just in case
        for node, data in graph.nodes_iter(data=True):
            if 'comp' in data and '.' not in node:
                try:
                    comp = getattr(self, node)
                    ins, outs = comp.list_deriv_vars()
                except AttributeError:
                    continue
                for name in chain(ins,outs):
                    name = '.'.join((node, name))
                    if name not in varmeta:
                        varmeta[name] = self._get_var_info(name)

        return varmeta

    def _add_driver_subvar_conns(self, depgraph, collapsed):
        """Connect any var nodes with subvar sources that don't have an upstream component
        to their basevar's upstream component.
        """
        for node, data in collapsed.nodes_iter(data=True):
            if 'basevar' in data and collapsed.in_degree(node) == 0:
                base = self.name2collapsed[data['basevar']]
                if base in collapsed:
                    preds = collapsed.predecessors(base)
                    if preds:
                        collapsed.add_edge(preds[0], node)
        return collapsed

    def _remove_vartrees(self, g):
        """Remove all vartree nodes."""
        vtnodes = [n for n in g if self.contains(n) and isinstance(self.get(n), VariableTree)]
        
        for vt in vtnodes:
            succ = g.successors(vt)
            pred = g.predecessors(vt)
            
            if '.' in vt:
                # connect subs to parent comp
                cname = vt.split('.', 1)[0]
                if cname in succ:
                    for p in pred:
                        if p != cname and base_var(g, p) == vt:
                            g.add_edge(p, cname)
                elif cname in pred:
                    for s in succ:
                        if s != cname and base_var(g, s) == vt:
                            g.add_edge(cname, s)
                            
        g.remove_nodes_from(vtnodes)
                
    def _explode_vartrees(self, dgraph):
        """Given a depgraph, take all connected variable nodes corresponding
        to VariableTrees and replace them with a variable node for each
        variable in the VariableTree.
        """
        vtvars = dict([(n,None) for n in dgraph if isinstance(self.get(n), VariableTree)])        
        conns = [(u,v) for u,v in dgraph.list_connections() if u in vtvars]
        
        depgraph = dgraph.subgraph(dgraph.nodes_iter())
        
        # explode all vt nodes first
        for vt in vtvars:
            obj = self.get(vt)
            vtvars[vt] = ['.'.join((vt, n.split('.',1)[1]))
                                     for n in obj.list_all_vars()]
            for sub in vtvars[vt]:
                if sub not in depgraph:
                    depgraph.add_subvar(sub)
        
        vtconns = {}
        for u,v in conns:
            varlist = [vtvars[u], vtvars[v]]

            if len(varlist[0]) != len(varlist[1]):
                self.raise_exception("connected vartrees '%s' and '%s' do not have the same variable list" %
                                     (u, v))

            for src, dest in zip(varlist[0], varlist[1]):
                if src.split('.')[-1] != dest.split('.')[-1]:
                    self.raise_exception("variables '%s' and '%s' in vartree connection '%s' -> '%s' do not match" %
                                         (src, dest, u, v))

            vtconns[(u,v)] = varlist


        for (u,v), varlist in vtconns.items():
            ucomp = udestcomp = vcomp = None

            # see if there's a u component
            comp = u.split('.', 1)[0]
            if comp in depgraph and depgraph.node[comp].get('comp'):
                if comp in depgraph.predecessors(u):
                    ucomp = comp
                elif comp in depgraph.successors(u):  # handle input as output case
                    udestcomp = comp

            # see if there's a v component
            comp = v.split('.', 1)[0]
            if comp in depgraph and depgraph.node[comp].get('comp'):
                if comp in depgraph.successors(v):
                    vcomp = comp

            for src, dest in zip(varlist[0], varlist[1]):
                depgraph.add_edge(src, dest, conn=True)
                if ucomp:
                    depgraph.add_edge(ucomp, src)
                if udestcomp:
                    depgraph.add_edge(src, udestcomp)
                if vcomp:
                    depgraph.add_edge(dest, vcomp)

        return depgraph

    def get_comps_and_pseudos(self):
        for node, data in self._depgraph.nodes_iter(data=True):
            if 'comp' in data:
                yield getattr(self, node)

    def pre_setup(self):
        self._provideJ_bounds = None
        for comp in self.get_comps_and_pseudos():
            comp.pre_setup()

    def post_setup(self):
        for comp in self.get_comps():
            comp.post_setup()

        self._system.vec['u'].set_from_scope(self)

    def _setup(self, inputs=None, outputs=None):
        """This is called automatically on the top level Assembly
        prior to execution.  It will also be called if
        calc_gradient is called with input or output lists that
        differ from the lists of parameters or objectives/constraints
        that are inherent to the model.
        """

        if MPI:
            MPI.COMM_WORLD.Set_errhandler(MPI.ERRORS_ARE_FATAL)
            comm = MPI.COMM_WORLD
        else:
            comm = None

        self._var_meta = {}
        
        try:
            self.pre_setup()
            self.setup_depgraph()
            self.setup_reduced_graph(inputs=inputs, outputs=outputs)
            self.setup_systems()
            self.setup_communicators(comm)
            self.setup_variables()
            self.setup_sizes()
            self.setup_vectors()
            self.setup_scatters()
        except Exception:
            if MPI:
                mpiprint(traceback.format_exc())
            raise
        else:
            self.post_setup()


def dump_iteration_tree(obj, f=sys.stdout, full=True, tabsize=4, derivs=False):
    """Returns a text version of the iteration tree
    of an OpenMDAO object.  The tree shows which are being
    iterated over by which drivers.

    If full is True, show pseudocomponents as well.
    If derivs is True, include derivative input/output
    information.
    """
    def _dump_iteration_tree(obj, f, tablevel):
        tab = ' ' * tablevel
        if is_instance(obj, Driver):
            f.write("%s%s\n" % (tab, obj.name))
            if derivs:
                raise NotImplementedError("dumping of derivative inputs/outputs not supported yet.")
                    # f.write("%s*deriv inputs: %s\n"
                    #         % (' '*(tablevel+tabsize+2), inputs))
                    # f.write("%s*deriv outputs: %s\n"
                    #         % (' '*(tablevel+tabsize+2), outputs))
            names = set(obj.workflow.get_names())
            for comp in obj.workflow:
                if not full and comp.name not in names:
                    continue
                if is_instance(comp, Driver) or is_instance(comp, Assembly):
                    _dump_iteration_tree(comp, f, tablevel + tabsize)
                elif is_instance(comp, PseudoComponent):
                    f.write("%s%s  (%s)\n" %
                        (' ' * (tablevel+tabsize), comp.name, comp._orig_expr))
                else:
                    f.write("%s%s\n" % (' ' * (tablevel+tabsize), comp.name))
        elif is_instance(obj, Assembly):
            f.write("%s%s\n" % (tab, obj.name))
            _dump_iteration_tree(obj.driver, f, tablevel + tabsize)

    _dump_iteration_tree(obj, f, 0)

def _get_wflow_names(iter_tree):
    """Return a list of names with driver sub-iter-trees collapsed
    down to just the driver name, i.e., return what would be
    in driver.workflow.get_names(full=True).
    """
    names = []
    for n in iter_tree[1]:
        if isinstance(n, basestring):
            names.append(n)
        else:
            names.append(n[0])
    return names

def _get_scoped_inputs(comp, g):
    """Return a list of inputs varnames scoped to the given name."""
    cname = comp.name
    inputs = []
    for p in g.predecessors(cname):
        if p.startswith(cname+'.'):
            inputs.append(p.split('.',1)[1])

    if not inputs:
        return None

    return inputs

def _get_scoped_outputs(comp, g):
    """Return a list of outputs varnames scoped to the given name."""
    cname = comp.name
    outputs = []
    for s in g.successors(cname):
        if s.startswith(cname+'.'):
            outputs.append(s.split('.',1)[1])

    if not outputs:
        return None

    return outputs