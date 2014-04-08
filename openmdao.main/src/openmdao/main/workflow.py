""" Base class for all workflows. """

#from networkx.algorithms.components import strongly_connected_components

# pylint: disable-msg=E0611,F0401
from openmdao.main.exceptions import RunStopped
from openmdao.main.pseudocomp import PseudoComponent
#from openmdao.main.interfaces import IDriver
#from openmdao.main.mp_support import has_interface
from openmdao.main.mpiwrap import MPI_info, mpiprint
from openmdao.main.systems import SerialSystem, ParallelSystem, \
                                  transform_graph, collapse_subdrivers

__all__ = ['Workflow']


class Workflow(object):
    """
    A Workflow consists of a collection of Components which are to be executed
    in some order.
    """

    def __init__(self, parent=None, scope=None, members=None):
        """Create a Workflow.

        parent: Driver (optional)
            The Driver that contains this Workflow.  This option is normally
            passed instead of scope because scope usually isn't known at
            initialization time.  If scope is not provided, it will be
            set to parent.parent, which should be the Assembly that contains
            the parent Driver.

        scope: Component (optional)
            The scope can be explicitly specified here, but this is not
            typically known at initialization time.

        members: list of str (optional)
            A list of names of Components to add to this workflow.
        """
        self._iterator = None
        self._stop = False
        self._parent = parent
        self._scope = scope
        self._exec_count = 0     # Workflow executions since reset.
        self._initial_count = 0  # Value to reset to (typically zero).
        self._comp_count = 0     # Component index in workflow.
        self._drv_graph = None
        self._wf_comp_graph = None
        self._subsystem = None
        if members:
            for member in members:
                if not isinstance(member, basestring):
                    raise TypeError("Components must be added to a workflow by name.")
                self.add(member)

        self.mpi = MPI_info()

    @property
    def scope(self):
        """The scoping Component that is used to resolve the Component names in
        this Workflow.
        """
        if self._scope is None and self._parent is not None:
            self._scope = self._parent.get_expr_scope()
        if self._scope is None:
            raise RuntimeError("workflow has no scope!")
        return self._scope

    @scope.setter
    def scope(self, scope):
        self._scope = scope
        self.config_changed()

    @property
    def itername(self):
        return self._iterbase('')

    def check_config(self):
        """Perform any checks that we need prior to run. Specific workflows
        should override this."""
        pass

    def set_initial_count(self, count):
        """
        Set initial value for execution count.  Only needed if the iteration
        coordinates must be initialized, such as for CaseIterDriverBase.

        count: int
            Initial value for workflow execution count.
        """
        self._initial_count = count - 1  # run() and step() will increment.

    def reset(self):
        """ Reset execution count. """
        self._exec_count = self._initial_count

    def run(self, ffd_order=0, case_id=''):
        """ Run the Components in this Workflow. """
        if self._subsystem is not None:
            return self._subsystem.run(self.scope, 
                                       ffd_order, 
                                       case_id, self._iterbase(case_id))

        self._stop = False
        self._iterator = self.__iter__()
        self._exec_count += 1
        self._comp_count = 0
        iterbase = self._iterbase(case_id)

        for comp in self._iterator:
            if isinstance(comp, PseudoComponent):
                comp.run(ffd_order=ffd_order, case_id=case_id)
            else:
                self._comp_count += 1
                comp.set_itername('%s-%d' % (iterbase, self._comp_count))
                comp.run(ffd_order=ffd_order, case_id=case_id)
            if self._stop:
                raise RunStopped('Stop requested')
        self._iterator = None

    def _iterbase(self, case_id):
        """ Return base for 'iteration coordinates'. """
        if self._parent is None:
            return str(self._exec_count)  # An unusual case.
        else:
            prefix = self._parent.get_itername()
            if not prefix:
                prefix = case_id
            if prefix:
                prefix += '.'
            return '%s%d' % (prefix, self._exec_count)

    def step(self, ffd_order=0, case_id=''):
        """Run a single component in this Workflow."""
        if self._iterator is None:
            self._iterator = self.__iter__()
            self._exec_count += 1
            self._comp_count = 0

        comp = self._iterator.next()
        self._comp_count += 1
        iterbase = self._iterbase(case_id)
        comp.set_itername('%s-%d' % (iterbase, self._comp_count))
        try:
            comp.run(ffd_order=ffd_order, case_id=case_id)
        except StopIteration, err:
            self._iterator = None
            raise err
        raise RunStopped('Step complete')

    def stop(self):
        """
        Stop all Components in this Workflow.
        We assume it's OK to to call stop() on something that isn't running.
        """
        for comp in self.get_components(full=True):
            comp.stop()
        self._stop = True

    def add(self, compnames, index=None, check=False):
        """ Add new component(s) to the workflow by name."""
        raise NotImplementedError("This Workflow has no 'add' function")

    def config_changed(self):
        """Notifies the Workflow that workflow configuration
        (dependencies, etc.) has changed.
        """
        self._drv_graph
        self._wf_comp_graph = None
        self._subsystem = None

    def remove(self, comp):
        """Remove a component from this Workflow by name."""
        raise NotImplementedError("This Workflow has no 'remove' function")

    def get_names(self, full=False):
        """Return a list of component names in this workflow."""
        raise NotImplementedError("This Workflow has no 'get_names' function")

    def get_components(self, full=False):
        """Returns a list of all component objects in the workflow. No ordering
        is assumed.
        """
        scope = self.scope
        return [getattr(scope, name) for name in self.get_names(full)]

    def __iter__(self):
        """Returns an iterator over the components in the workflow in
        some order.
        """
        raise NotImplementedError("This Workflow has no '__iter__' function")

    def __len__(self):
        raise NotImplementedError("This Workflow has no '__len__' function")

    def get_driver_graph(self):
        """Returns the subgraph of the full depgraph that is
        relevant to this workflow and this workflow's driver (and
        all subdrivers).
        """
        if self._drv_graph is None:
            # make a copy of the depgraph we can modify
            depgraph = self.scope._depgraph
            graph = depgraph.subgraph(depgraph.nodes_iter())
            # add all driver related 'connections'
            self._parent.add_driver_connections(graph, recurse=True)

            # mpiprint("before pruning:")
            # for u,v,data in graph.edges(data=True):
            #     mpiprint("(%s,%s): %s" % (u,v,data.get("var_edges")))

            # remove all unconnected variables and components, 
            # and only what's relevant remains
            graph.prune_unconnected()
            self._drv_graph = graph
        return self._drv_graph

    def get_comp_graph(self):
        """Returns the subgraph of the component graph that contains
        the components in this workflow, including additional connections
        needed due to subdriver iterations.
        """
        if self._wf_comp_graph is None:
            cgraph = self.scope._depgraph.component_graph().copy()
            self._wf_comp_graph = cgraph.subgraph(self.get_names(full=True))
        return self._wf_comp_graph

    # def add_driver_connections(self, g):
    #     """Add edges in the workflow component graph to represent dependencies
    #     between a comp and a driver if that comp is connected to any comps 
    #     inside of the driver's iteration set.
    #     """
    #     scope = self.scope
    #     dconns = []
    #     for node in g:
    #         comp = getattr(scope, node)
    #         if has_interface(comp, IDriver):
    #             iterset = [c.name for c in comp.iteration_set()]
    #             for itercomp in iterset:
    #                 for u,v in g.edges_iter(itercomp):
    #                     if v not in iterset:
    #                         dconns.append((node, v))
    #                 for u,v in g.in_edges_iter(itercomp):
    #                     if u not in iterset:
    #                         dconns.append((u, node))

    #     g.add_edges_from(dconns)

    #     # now add metadata to indicate which vars are set or retrieved from
    #     # our parent driver
    #     srcset, destset = self._parent.get_expr_var_depends()

    #     # now add variables that are connected inputs or outputs
    #     wfgraph = self.get_graph()

    #     conns = wfgraph.list_connections()

    #     srcset.update([src for src,dest in conns])
    #     destset.update([dest for src,dest in conns])

    #     for name in srcset:
    #         parts = name.split('.', 1)
    #         if len(parts) > 1 and parts[0] in g:
    #             g.node[parts[0]].setdefault('inputs', set()).add(parts[1])
                
    #     for name in destset:
    #         parts = name.split('.', 1)
    #         if len(parts) > 1 and parts[0] in g:
    #             g.node[parts[0]].setdefault('outputs', set()).add(parts[1])
     
    ## MPI stuff ##

    def _get_subsystem(self):
        """Get the serial/parallel subsystem for this workflow. Each
        subsystem contains a subgraph of this workflow's component 
        graph, which contains components and/or other subsystems.
        """
        if self._subsystem is None:
            scope = self.scope

            drvgraph = self.get_driver_graph()

            # first, get the component subgraph that is limited to 
            # the components in this workflow.
            cgraph = drvgraph.component_graph(driver=True)

            collapse_subdrivers(cgraph, self._parent)
            cgraph.remove_node(self._parent.name)

            #mpiprint("**** %s: cgraph edges (pre-xform) = %s" % (self._parent.name,cgraph.edges()))

            # collapse the graph (recursively) with the parallel
            # branches collapsed into single nodes with tuple names
            transform_graph(cgraph, scope)

            #mpiprint("**** %s: cgraph nodes (post-xform) = %s" % (self._parent.name,cgraph.nodes()))
            #mpiprint("**** %s: cgraph edges (post-xform) = %s" % (self._parent.name,cgraph.edges()))
            if len(cgraph) > 1:
                if len(cgraph.edges()) > 0:
                    #mpiprint("creating serial top: %s" % cgraph.nodes())
                    self._subsystem = SerialSystem(cgraph, scope)
                else:
                    #mpiprint("creating parallel top: %s" % cgraph.nodes())
                    self._subsystem = ParallelSystem(cgraph, scope)
            else:
                self._subsystem = cgraph.node[cgraph.nodes()[0]]['system']

        #mpiprint("got subsystem %s" % self._subsystem.name)
        return self._subsystem

    def get_req_cpus(self):
        """Return requested_cpus"""
        return self._get_subsystem().get_req_cpus()

    # def get_vector_vars(self):
    #     """Assemble an ordereddict of names of variables needed by this
    #     workflow, which includes any that its parent driver(s) reference
    #     in parameters, constraints, or objectives, as well as any used to 
    #     connect any components in this workflow, AND any returned from
    #     calling get_vector_vars on any subsystems in this workflow.
    #     """
    #     return self._get_subsystem().get_vector_vars(self.scope)

    def setup_sizes(self, variables):
        self._get_subsystem().setup_sizes(variables)

    def setup_communicators(self, comm, scope):
        """Allocate communicators from here down to all of our
        child Components.
        """
        self.mpi.comm = comm
        self._get_subsystem().setup_communicators(comm, scope)
