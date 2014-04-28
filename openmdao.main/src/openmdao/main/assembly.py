""" Class definition for Assembly. """


#public symbols
__all__ = ['Assembly', 'set_as_top']

import fnmatch
import re
import sys
import threading

from zope.interface import implementedBy

# pylint: disable-msg=E0611,F0401
import networkx as nx

from openmdao.main.interfaces import implements, IAssembly, IDriver, \
                                     IArchitecture, IComponent, IContainer, \
                                     ICaseIterator, ICaseRecorder, IDOEgenerator
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
from openmdao.main.exprmapper import ExprMapper, PseudoComponent
from openmdao.main.array_helpers import is_differentiable_var
from openmdao.main.depgraph import is_comp_node, is_boundary_node

from openmdao.util.graph import list_deriv_vars  # , graph_to_svg
from openmdao.util.nameutil import partition_names_by_comp
from openmdao.util.log import logger

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
                     desc='Case recorders for iteration data.')

    # Extra variables for adding to CaseRecorders
    printvars = List(Str, iotype='in', framework_var=True,
                     desc='List of extra variables to output in the recorders.')

    def __init__(self):

        super(Assembly, self).__init__()

        self._exprmapper = ExprMapper(self)
        self._graph_loops = []
        self.J_input_keys = None
        self.J_output_keys = None

        # parent depgraph may have to invalidate us multiple times per pass
        self._invalidation_type = 'partial'

        # default Driver executes its workflow once
        self.add('driver', Driver())

        # we're the top Assembly only if we're the first instantiated
        set_as_top(self, first_only=True)

        # Assemblies automatically figure out their own derivatives, so
        # any boundary vars that are unconnected should be zero.
        self.missing_deriv_policy = 'assume_zero'

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
        self.driver.set_itername(itername)
        if seqno:
            self.driver.workflow.set_initial_count(seqno)

    def find_referring_connections(self, name):
        """Returns a list of connections where the given name is referred
        to either in the source or the destination.
        """
        exprset = set(self._exprmapper.find_referring_exprs(name))
        return [(u, v) for u, v
                       in self._depgraph.list_connections(show_passthrough=True,
                                                          show_external=True)
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

    def _cleanup_autopassthroughs(self, name):
        """Clean up any autopassthrough connections involving the given name.
        Returns a list containing a tuple for each removed connection.
        """
        old_autos = []
        if self.parent:
            old_rgx = re.compile(r'(\W?)%s.' % name)
            par_rgx = re.compile(r'(\W?)parent.')

            pattern = r'\g<1>%s.' % '.'.join((self.name, name))
            for u, v in self._depgraph.list_autopassthroughs():
                newu = re.sub(old_rgx, pattern, u)
                newv = re.sub(old_rgx, pattern, v)
                if newu != u or newv != v:
                    old_autos.append((u, v))
                    u = re.sub(par_rgx, r'\g<1>', newu)
                    v = re.sub(par_rgx, r'\g<1>', newv)
                    self.parent.disconnect(u, v)
        return old_autos

    def rename(self, oldname, newname):
        """Renames a child of this object from oldname to newname."""
        self._check_rename(oldname, newname)
        conns = self.find_referring_connections(oldname)
        wflows = self.find_in_workflows(oldname)
        old_autos = self._cleanup_autopassthroughs(oldname)

        obj = self.remove(oldname)
        obj.name = newname
        self.add(newname, obj)

        # oldname has now been removed from workflows, but newname may be in the
        # wrong location, so force it to be at the same index as before removal
        for wflow, idx in wflows:
            wflow.remove(newname)
            wflow.add(newname, idx)

        old_rgx = re.compile(r'(\W?)%s.' % oldname)
        par_rgx = re.compile(r'(\W?)parent.')

        # recreate all of the broken connections after translating
        # oldname to newname
        for u, v in conns:
            self.connect(re.sub(old_rgx, r'\g<1>%s.' % newname, u),
                         re.sub(old_rgx, r'\g<1>%s.' % newname, v))

        # recreate autopassthroughs
        if self.parent:
            pattern = r'\g<1>%s.' % '.'.join((self.name, newname))
            for u, v in old_autos:
                u = re.sub(old_rgx, pattern, u)
                v = re.sub(old_rgx, pattern, v)
                u = re.sub(par_rgx, r'\g<1>', u)
                v = re.sub(par_rgx, r'\g<1>', v)
                self.parent.connect(u, v)

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
                self.reraise_exception("Couldn't replace '%s' of type %s with type %s"
                                       % (target_name, type(tobj).__name__,
                                          type(newobj).__name__))

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
        if has_interface(obj, IComponent) or \
           isinstance(obj, PseudoComponent):
            for cname in self.list_containers():
                obj = getattr(self, cname)
                if isinstance(obj, Driver):
                    obj.remove_references(name)
            self.disconnect(name)
        elif name in self.list_inputs() or name in self.list_outputs():
            self.disconnect(name)

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
            else:
                val = _copydict[ttype.copy](val)

        setattr(self, newname, val)

        try:
            if iotype == 'in':
                self.connect(newname, pathname)
            else:
                self.connect(pathname, newname)
        except RuntimeError as err:
            self.remove(newname)
            raise err

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

    def _split_varpath(self, path):
        """Return a tuple of compname,component,varname given a path
        name of the form 'compname.varname'. If the name is of the form
        'varname', then compname will be None and comp is self.
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

        #self.config_changed(update_parent=False)

        if isinstance(dest, basestring):
            dest = (dest,)
        for dst in dest:
            dst = eliminate_expr_ws(dst)
            try:
                self._connect(src, dst)
            except Exception:
                self.reraise_exception("Can't connect '%s' to '%s'" % (src, dst))

    def _connect(self, src, dest):
        """Handle one connection destination. This should only be called via
        the connect() function, never directly.
        """

        # Among other things, check if already connected.
        srcexpr, destexpr, pcomp_type = \
                   self._exprmapper.check_connect(src, dest, self)

        # Check if dest is declared as a parameter in any driver in the assembly
        for item in self.list_containers():
            comp = self.get(item)
            if isinstance(comp, Driver) and hasattr(comp, 'list_param_targets'):
                if dest in comp.list_param_targets():
                    msg = "destination '%s' is a Parameter in " % dest
                    msg += "driver '%s'." % comp.name
                    self.raise_exception(msg, RuntimeError)

        if pcomp_type is not None:
            pseudocomp = PseudoComponent(self, srcexpr, destexpr,
                                         pseudo_type=pcomp_type)
            self.add(pseudocomp.name, pseudocomp)
            pseudocomp.make_connections(self)
        else:
            pseudocomp = None
            super(Assembly, self).connect(src, dest)

        try:
            self._exprmapper.connect(srcexpr, destexpr, self, pseudocomp)
        except Exception:
            super(Assembly, self).disconnect(src, dest)
            raise

        if not srcexpr.refs_parent():
            if not destexpr.refs_parent():
                # if it's an internal connection, could change dependencies,
                # so we have to call config_changed to notify our driver
                self.config_changed(update_parent=False)

                outs = self._depgraph.invalidate_deps(self, [dest])
                if (outs is None) or outs:
                    for cname, vnames in partition_names_by_comp(outs).items():
                        self.child_invalidated(cname, vnames)

    @rbac(('owner', 'user'))
    def disconnect(self, varpath, varpath2=None):
        """If varpath2 is supplied, remove the connection between varpath and
        varpath2. Otherwise, if varpath is the name of a trait, remove all
        connections to/from varpath in the current scope. If varpath is the
        name of a Component, remove all connections from all of its inputs
        and outputs.
        """
        try:
            if varpath2 is None and self.parent and '.' not in varpath and \
               is_boundary_node(self._depgraph, varpath):
                # boundary var. make sure it's disconnected in parent
                self.parent.disconnect('.'.join((self.name, varpath)))

            to_remove, pcomps = self._exprmapper.disconnect(varpath, varpath2)

            graph = self._depgraph

            if to_remove:
                for u, v in graph.list_connections(show_external=True):
                    if (u, v) in to_remove:
                        super(Assembly, self).disconnect(u, v)
                        to_remove.remove((u, v))

                for u, v in graph.list_autopassthroughs():
                    if (u, v) in to_remove:
                        super(Assembly, self).disconnect(u, v)
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
                    except:
                        pass
                try:
                    graph.remove(name)
                except (KeyError, nx.exception.NetworkXError):
                    pass
        finally:
            self.config_changed()

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

        # Detect and save any loops in the graph.
        self._graph_loops = None

        self.J_input_keys = self.J_output_keys = None

    def _set_failed(self, path, value, index=None, src=None, force=False):
        parts = path.split('.', 1)
        if len(parts) > 1:
            obj = getattr(self, parts[0])
            if isinstance(obj, PseudoComponent):
                obj.set(parts[1], value, index, src, force)

    def execute(self):
        """Runs driver and updates our boundary variables."""
        if self.parent is None:
            for recorder in self.recorders:
                recorder.startup()

        self.driver.run(ffd_order=self.ffd_order, case_uuid=self._case_uuid)

        self._depgraph.update_boundary_outputs(self)

    def get_case_variables(self):
        """Collect variables to be recorded by workflows."""
        inputs = []
        outputs = []
        for printvar in self.printvars:
            if '*' in printvar:
                printvars = self._get_all_varpaths(printvar)
            else:
                printvars = [printvar]

            for var in printvars:
                iotype = self.get_metadata(var, 'iotype')
                if iotype == 'in':
                    val = ExprEvaluator(var, scope=self).evaluate()
                    inputs.append((var, val))
                elif iotype == 'out':
                    val = ExprEvaluator(var, scope=self).evaluate()
                    outputs.append((var, val))
                else:
                    msg = "%s is not an input or output" % var
                    self.raise_exception(msg, ValueError)
        return (inputs, outputs)

    def _get_all_varpaths(self, pattern):
        """Return a list of all varpaths that match the specified pattern."""
        prefix = self.get_pathname()
        if prefix:
            prefix += '.'

        # Start with our settings.
        all_vars = []
        for var in self.list_vars():
            all_vars.append('%s%s' % (prefix, var))

        # Now all components.
        for name in self.list_containers():
            obj = getattr(self, name)
            if isinstance(obj, Component) and \
               not isinstance(obj, PseudoComponent):

                for var in obj.list_vars():
                    all_vars.append('%s%s.%s' % (prefix, name, var))

                # Recurse into assemblies
                if isinstance(obj, Assembly):
                    assy_vars = obj._get_all_varpaths(pattern)
                    all_vars = all_vars + assy_vars

        # Match pattern in our var names
        matched_vars = []
        if pattern == '*':
            matched_vars = all_vars
        else:
            matched_vars = fnmatch.filter(all_vars, pattern)

        return matched_vars

    def step(self):
        """Execute a single child component and return."""
        self.driver.step()

    def stop(self):
        """Stop the calculation."""
        self.driver.stop()

    @rbac(('owner', 'user'))
    def _run_terminated(self):
        """ Executed at end of top-level run. """
        super(Assembly, self)._run_terminated()
        for name in self.list_containers():
            obj = getattr(self, name)
            if has_interface(obj, IComponent):
                obj._run_terminated()

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
    def update_inputs(self, compname):
        """Transfer input data to input expressions on the specified component.
        The inputs iterator is assumed to contain strings that reference
        component variables relative to the component, e.g., 'abc[3][1]' rather
        than 'comp1.abc[3][1]'.
        """
        invalid_ins = self._depgraph.list_inputs(compname, invalid=True)
        if invalid_ins:
            self._update_invalid_dests(compname, invalid_ins)

    def update_outputs(self, outnames):
        """Execute any necessary internal or predecessor
        components in order to make the specified output
        variables valid.
        """
        data = self._depgraph.node
        invalid_dests = [n for n in outnames if data[n]['valid'] is False]
        if invalid_dests:
            self._update_invalid_dests(None, invalid_dests)

    def _update_invalid_dests(self, startcomp, invalid_dests):
        graph = self._depgraph
        invalids = set()
        for inv_dest in invalid_dests:
            invalids.update([s for s in graph.get_sources(inv_dest)
                                if not graph.node[s]['valid']])

        # if source vars are invalid, request an update
        if invalids:
            loops = graph.get_loops()

            for cname, vnames in partition_names_by_comp(invalids).items():
                if cname is None or not is_comp_node(graph, cname):
                    # boundary var
                    if self.parent:
                        self.parent.update_inputs(self.name)

                # If our start component is in a loop with us, don't
                # run it. Otherwise you have infinite recursion. It is
                # the responsibility of the solver to properly execute
                # the comps in its loop.
                elif loops:
                    for loop in loops:
                        if startcomp in loop and cname in loop:
                            break
                    else:
                        getattr(self, cname).update_outputs(vnames)
                else:
                    getattr(self, cname).update_outputs(vnames)

        try:
            for inv_dest in invalid_dests:
                self._depgraph.update_destvar(self, inv_dest)
        except Exception as err:
            self.raise_exception(str(err), type(err))

    def _input_updated(self, name, fullpath=None):
        outs = self.invalidate_deps([name])
        if self.parent:
            outs.add(name)
            self.parent.child_invalidated(self.name, outs)

    @rbac(('owner', 'user'))
    def child_invalidated(self, childname, vnames=None, iotype='out'):
        """Invalidate all variables that depend on the variable
        provided by the child that has been invalidated.
        """
        if childname not in self._depgraph:
            return []

        if vnames is None:
            vnames = [childname]
        else:
            vnames = ['.'.join((childname, n)) for n in vnames]
            if iotype == 'in':
                for name in vnames[:]:
                    vnames.extend(self._depgraph._all_child_vars(name,
                                                                 direction='in'))

        bouts = self.invalidate_deps(vnames)
        if bouts and self.parent:
            self.parent.child_invalidated(self.name, bouts)
        return bouts

    @rbac(('owner', 'user'))
    def child_run_finished(self, childname, outs=None):
        """Called by a child when it completes its run() function."""
        self._depgraph.child_run_finished(childname, outs)

    @rbac(('owner', 'user'))
    def get_valid(self, names):
        """Get the value of the validity flag for the specified variables.
        Returns a list of bools.

        names: iterator of str
            Names of variables whose validity is requested.
        """
        data = self._depgraph.node
        return [data[n]['valid'] for n in names]

    def set_valid(self, names, valid):
        """Mark the io traits with the given names as valid or invalid."""
        data = self._depgraph.node
        for name in names:
            data[name]['valid'] = valid

    def _validate(self):
        # validate boundary inputs and outputs and their subvars
        self._depgraph.validate_boundary_vars()
        super(Assembly, self)._validate()

    def has_partial_validation(self):
        return True

    def invalidate_deps(self, varnames=None):
        """Mark all Variables invalid that depend on varnames.
        Returns a list of our newly invalidated boundary outputs.

        varnames: iter of str (optional)
            An iterator of names of destination variables.
        """
        # If varnames is None, we're being called from a parent Assembly
        # as part of a higher level invalidation, so we only need to look
        # at our connected inputs
        if varnames is None:
            names = self._depgraph.get_extern_srcs()
        else:
            names = varnames

        if self._exec_state != 'INVALID':
            self._set_exec_state('INVALID')

        return self._depgraph.invalidate_deps(self, names)

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

        Returns the finite difference gradient, the OpenMDAO-calculated gradient,
        a list of the gradient names, and a list of suspect inputs/outputs.
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

        # fill in any missing inputs or outputs using the object specified by 'name'
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
                self.raise_exception("Can't find any inputs for generating gradient.")
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
                inputs = sorted(inputs)
            else:
                self.raise_exception("Can't find any outputs for generating gradient.")

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

    def provideJ(self, required_inputs, required_outputs, check_only=False):
        '''An assembly calculates its Jacobian by calling the calc_gradient
        method on its base driver. Note, derivatives are only calculated for
        floats and iterable items containing floats.'''

        # Sub-assembly sourced
        output_keys = []

        # Parent-assembly sourced
        self.J_input_keys = []
        self.J_output_keys = []
        self._provideJ_bounds = None

        depgraph = self._depgraph

        # HACK for some special CADRE runs. Remove when done.
        for item in self.list_inputs():
            meta = self.get_metadata(item)
            if 'framework_var' in meta:
                continue
            if item not in required_inputs:
                required_inputs.append(item)

        for src in required_inputs:
            varname = depgraph.base_var(src)
            target1 = [n for n in depgraph.successors(varname)
                               if not n.startswith('parent.')
                                  and depgraph.base_var(n) != varname]
            target2 = []
            if src in depgraph.node:
                target2 = [n for n in depgraph.successors(src)
                                   if not n.startswith('parent.')
                                      and depgraph.base_var(n) != varname
                                      and n not in target1]
            if len(target1) == 0 and len(target2) == 0:
                continue

            self.J_input_keys.append(src)

        for target in required_outputs:
            varname = depgraph.base_var(target)
            src = depgraph.predecessors(varname)
            if len(src) == 0:
                src = depgraph.get_sources(target)
                if len(src) == 0:
                    continue

            src = src[0]

            # If subvar, only ask the assembly to calculate the
            # elements we need.
            if target != varname:
                tail = target[len(varname):]
                src = '%s%s' % (src, tail)

            output_keys.append(src)
            self.J_output_keys.append(target)

        if check_only:
            return None

        return self.driver.calc_gradient(self.J_input_keys, output_keys)

    def list_deriv_vars(self):
        return self.J_input_keys, self.J_output_keys

    def list_components(self):
        ''' List the components in the assembly.
        '''
        names = [name for name in self.list_containers()
                     if isinstance(self.get(name), Component)]
        return names

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
        g = self._depgraph.component_graph()
        names = [name for name in nx.algorithms.dag.topological_sort(g)
                               if not name.startswith('@')]

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
                    'valid':      comp.is_valid(),
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
                        full_name = cname + '.' + vname
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
                            full_name = cname + '.' + vname + '.' + child_name
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
                            full_name = cname + '.' + vname + '[' + str(idx) + ']'
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
                        full_name = cname + '.' + vname
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
                            full_name = cname + '.' + vname + '.' + child_name
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
                            full_name = cname + '.' + vname + '[' + str(idx) + ']'
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
                        units = pcomp.get_metadata(pcomp.list_outputs()[0], 'units')
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
                        units = pcomp.get_metadata(pcomp.list_outputs()[0], 'units')
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
    #     """
    #     return graph_to_svg(self._depgraph.component_graph())


def dump_iteration_tree(obj, f=sys.stdout, full=True, tabsize=4, derivs=False):
    """Returns a text version of the iteration tree
    of an OpenMDAO object or hierarchy.  The tree
    shows which components are being iterated over by which
    drivers.

    If full is True, show pseudocomponents as well.
    If derivs is True, include derivative input/output information.
    """
    def _dump_iteration_tree(obj, f, tablevel):
        tab = ' ' * tablevel
        if is_instance(obj, Driver):
            f.write("%s%s\n" % (tab, obj.name))
            if derivs:
                try:
                    dgraph = obj.workflow.derivative_graph()
                except Exception as err:
                    f.write("%s*ERR in deriv graph: %s\n"
                            % (' '*(tablevel+tabsize+2), str(err)))
                else:
                    inputs = dgraph.graph.get('mapped_inputs',
                                              dgraph.graph.get('inputs', []))
                    outputs = dgraph.graph.get('mapped_outputs',
                                               dgraph.graph.get('outputs', []))
                    f.write("%s*deriv inputs: %s\n"
                            % (' '*(tablevel+tabsize+2), inputs))
                    f.write("%s*deriv outputs: %s\n"
                            % (' '*(tablevel+tabsize+2), outputs))
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
