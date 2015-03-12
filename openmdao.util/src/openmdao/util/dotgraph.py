
import os
import sys
import webbrowser

import networkx as nx
from openmdao.main.interfaces import IDriver, IAssembly
from openmdao.main.depgraph import DependencyGraph
from openmdao.main.problem_formulation import ArchitectureAssembly
from openmdao.main.systems import AssemblySystem, SerialSystem, ParallelSystem, \
                                  VarSystem, SolverSystem, \
                                  FiniteDiffDriverSystem, TransparentDriverSystem, OpaqueSystem
from openmdao.util.graph import base_var

_cluster_count = 0

def write_driver_cluster(f, G, driver, indent, counts, alledges, excludes=()):
    global _cluster_count
    show = driver.name not in excludes
    comps = list(driver.workflow)
    subG = G.subgraph([c.name for c in comps])
    tab = ' '*indent

    if show:
        f.write('%ssubgraph cluster%s {\n' % (tab, _cluster_count))
        _cluster_count += 1
        indent += 3
        tab = ' '*indent

        f.write('%s%s [shape=invhouse];\n' % (tab, driver.name))

    if len(comps) > 0:
        dcount = 1
        for comp in comps:
            if IDriver.providedBy(comp):
                write_driver_cluster(f, G, comp, indent, counts, alledges)
                f.write("%s%s -> %s [style=dashed, label=%d];\n" % (tab, driver.name, comp.name, dcount))
                alledges.add((driver.name,comp.name))
                dcount += 1

        subG = G.subgraph([c.name for c in comps])
        edges = subG.edges()

        # now remove subdrivers
        subG.remove_nodes_from([c.name for c in comps if IDriver.providedBy(c)])

        write_nodes(f, subG, indent, counts, driver.name)

        for u,v in edges:
            alledges.add((u,v))
            if counts[u] > 0:
                u = '"%s@%s"' % (u, driver.name)
            if counts[v] > 0:
                v = '"%s@%s"' % (v, driver.name)
            f.write('%s%s -> %s;\n' % (tab, u, v))

        for pname in driver.list_pseudocomps():
            f.write('%s%s -> %s [style=dotted];\n' % (tab, pname, driver.name))
            alledges.add((pname, driver.name))

    f.write('%s}\n' % tab)

def write_nodes(f, G, indent, counts, parent):
    for node, data in G.nodes_iter(data=True):
        counts[node] -= 1
        if counts[node] > 0:
            data['label'] = node
            node = '%s@%s' % (node, parent)
            data['style'] = 'filled'
            data['fillcolor'] = 'gray'

        write_node(f, G.node[node], node, indent)

_meta_excludes = set([
    'inputs',
    'outputs',
    'system',
])

def write_node(f, meta, node, indent):
    assigns = ['%s=%s' % (k,v) for k,v in meta.items()
                    if k not in _meta_excludes]
    f.write('%s"%s" [%s];\n' % (' '*indent, node, ','.join(assigns)))

def _get_comp_counts(drv, counts):
    for cname in drv._ordering:
        comp = getattr(drv.parent, cname)
        counts[cname] += 1
        if IDriver.providedBy(comp):
            _get_comp_counts(comp, counts)

def write_workflow_dot(G, dotfile, scope=None, excludes=()):

    if scope is None:
        raise RuntimeError("if workflow is True, scope must be specified")

    with open(dotfile, 'w') as f:
        indent = 3

        f.write("strict digraph {\n")

        driver = getattr(scope, 'driver')

        f.write("   driver [shape=invhouse, margin=0.0];\n")

        # find any components that appear in multiple workflows
        counts = dict([(n,0) for n in G.nodes()])
        _get_comp_counts(driver, counts)

        if scope and isinstance(scope, ArchitectureAssembly):
            for pcomp in scope.list_pseudocomps():
                write_node(f, G.node[pcomp], pcomp, indent)

        alledges = set()
        write_driver_cluster(f, G, driver, indent, counts, alledges, excludes=excludes)

        #write_nodes(f, G, 3, counts, 'driver')

        # now include any cross workflow connections
        for u,v in G.edges_iter():
            if (u,v) not in alledges:
                f.write("   %s -> %s;\n" % (u, v))

        f.write("}\n")

def _update_graph_metadata(G, scope):
    nmeta = G.node
    conns = []
    for node, data in G.nodes_iter(data=True):
        data = data.copy() # don't corrupt metadata of other subgraphs/parent graphs
        nmeta[node] = data
        if 'pseudo' in data:
            data['label'] = node.replace('_pseudo_', '_p_') # shorten all pseudocomp names
            data['shape'] = 'diamond'
        elif 'driver' in data:
            data['shape'] = 'invhouse'
            if scope:
                driver = getattr(scope, node)
                for pcomp in driver.list_pseudocomps():
                    conns.append((pcomp, node))
                if hasattr(driver, 'list_param_targets'):
                    conns.extend([(node, p) for p in driver.list_param_targets() if p in G])
        elif 'comp' in data:
            data['shape'] = 'box'
        elif 'var' in data or 'basevar' in data: # var node
            try:
                parts = node.split('.', 1)
            except AttributeError:
                pass
            else:
                if hasattr(G, 'base_var'):
                    base = base_var(G, node)
                    if G.node[base].get('iotype') == 'state':
                        data['shape'] = 'doubleoctagon'
                    else:
                        data['shape'] = 'ellipse'
                else:
                    data['shape'] = 'ellipse'
        data['margin'] = '0.0'

    G.add_edges_from(conns, style='dotted')

def _map_systems(system, dct):
    """in our dotfile, all graph nodes must have unique names,
    but not all openmdao systems have unique names, so just
    create a map where every system is mapped to a unique
    name.
    """
    if len(dct) == 0:
        dct[id(system)] = "%s!0" % system.name
    for s in _get_subsystems(system):
        dct[id(s)] = "%s!%d" % (s.name, len(dct))
        _map_systems(s, dct)
    return dct

def write_system_dot(system, dotfile):
    # first, create a mapping of unique names to each system

    sysmap = _map_systems(system, {})

    with open(dotfile, 'w') as f:
        indent = 3

        f.write("strict digraph {\n")

        meta = {
            'shape': _dot_shape(system),
            'label': '"' + system.name + '"'
        }
        write_node(f, meta, sysmap[id(system)], indent)

        _sys_dot(system, indent, f, sysmap)

        f.write("}\n")

def _dot_shape(system):
    if isinstance(system, AssemblySystem):
        return "box3d"
    elif isinstance(system, FiniteDiffDriverSystem):
        return "invhouse"
    elif isinstance(system, TransparentDriverSystem):
        return "invhouse"
    elif isinstance(system, SolverSystem):
        return "house"
    elif isinstance(system, SerialSystem):
        return "rectangle"
    elif isinstance(system, ParallelSystem):
        return "parallelogram"
    elif isinstance(system, VarSystem):
        return "ellipse"

    return "box"

def _get_subsystems(system):
    """Returns immediate subsystems, even for opaque systems."""
    if isinstance(system, OpaqueSystem):
        return [system._inner_system]
    elif isinstance(system, AssemblySystem):
        return [system._comp._system]
    else:
        return system.subsystems()

def _sys_dot(system, indent, f, sysmap):

    for i, s in enumerate(_get_subsystems(system)):
        meta = {
            'shape': _dot_shape(s),
            'label': '"' + s.name + '"'
        }
        write_node(f, meta, sysmap[id(s)], indent)
        f.write('%s"%s" -> "%s" [label="%d"];\n' %
                        (' '*indent, sysmap[id(system)], sysmap[id(s)], i))
        _sys_dot(s, indent+3, f, sysmap)

def plot_system_tree(system, outfile=None, fmt='pdf'):
    if outfile is None:
        outfile = 'system_graph.'+fmt

    dotfile = os.path.splitext(outfile)[0]+'.dot'

    write_system_dot(system, dotfile)

    os.system("dot -T%s -o %s %s" % (fmt, outfile, dotfile))

    if sys.platform == 'darwin':
        os.system('open %s' % outfile)
    else:
        webbrowser.get().open(outfile)

    try:
        os.remove(dotfile)
    except:
        pass


def plot_graph(G, outfile=None, fmt='pdf', pseudos=True, workflow=False, scope=None,
               excludes=(), prune=False):
    """Create a plot of the given graph"""

    G = G.subgraph(G.nodes_iter())

    if isinstance(G, DependencyGraph):
        if workflow:
            G = G.component_graph()
        else:
            if scope:
                for cname in scope.list_containers():
                    comp = getattr(scope, cname)
                    if hasattr(comp, 'list_param_targets'):
                        for target in comp.list_param_targets():
                            G.node[target]['param'] = True
            if prune:
                G.prune_unconnected_vars()

    if not pseudos:
        nodes = [n for n in G.nodes() if '_pseudo_' in n]
        G.remove_nodes_from(nodes)

    _update_graph_metadata(G, scope)

    if outfile is None:
        outfile = 'graph.'+fmt

    dotfile = os.path.splitext(outfile)[0]+'.dot'

    if workflow:
        write_workflow_dot(G, dotfile, scope, excludes)
    else: # just show data connections
        nx.write_dot(G, dotfile)

    os.system("dot -T%s -o %s %s" % (fmt, outfile, dotfile))

    if sys.platform == 'darwin':
        os.system('open %s' % outfile)
    else:
        webbrowser.get().open(outfile)

    os.remove(dotfile)

def _driver_plots(obj, recurse=True, fmt='pdf', pseudos=True,
                  workflow=False, sysdone=False, prefix=''):
    # driver graph
    try:
        plot_graph(obj.get_reduced_graph(), fmt=fmt,
                   outfile=prefix+obj.get_pathname()+'_reduced'+'.'+fmt, pseudos=pseudos)
    except Exception as err:
        print "Can't plot reduced graph of '%s': %s" % (obj.get_pathname(), str(err))

    # workflow graph
    try:
        plot_graph(obj.workflow._reduced_graph, fmt=fmt,
                   outfile=prefix+obj.get_pathname()+'_wflow_reduced'+'.'+fmt, pseudos=pseudos)
    except Exception as err:
        print "Can't plot reduced graph of '%s' workflow: %s" % (obj.get_pathname(), str(err))

    if recurse:
        for comp in obj.workflow:
            if IAssembly.providedBy(comp):
                plot_graphs(comp, recurse, fmt=fmt, pseudos=pseudos,
                            workflow=workflow, sysdone=True, prefix=prefix)
            elif IDriver.providedBy(comp):
                _driver_plots(comp, recurse, fmt=fmt, pseudos=pseudos,
                              workflow=workflow, sysdone=True, prefix=prefix)

def plot_graphs(obj, recurse=True, fmt='pdf', pseudos=True,
                workflow=False, sysdone=False, prefix=''):
    if IAssembly.providedBy(obj):
        name = 'top' if obj.get_pathname()=='' else obj.get_pathname()

        try:
            plot_graph(obj._depgraph.get_pruned(), fmt=fmt,
                       outfile=prefix+name+'_depgraph'+'.'+fmt, pseudos=pseudos)
        except Exception as err:
            print "Can't plot depgraph of '%s': %s" % (name, str(err))
        try:
            plot_graph(obj._reduced_graph, fmt=fmt,
                       outfile=prefix+name+'_reduced'+'.'+fmt, pseudos=pseudos)
        except Exception as err:
            print "Can't plot reduced graph of '%s': %s" % (name, str(err))

        if not sysdone:
            try:
                plot_system_tree(obj._system, fmt=fmt,
                           outfile=prefix+name+'_system_tree'+'.'+fmt)
            except Exception as err:
                print "Can't plot system graph of '%s': %s" % (name, str(err))

        try:
            plot_graph(obj._reduced_graph.component_graph(),
                       fmt=fmt, outfile=prefix+name+'_compgraph'+'.'+fmt,
                       pseudos=pseudos, workflow=workflow, scope=obj)
        except Exception as err:
            print "Can't plot component_graph of '%s': %s" % (name, str(err))

        _driver_plots(obj._top_driver, recurse=recurse, fmt=fmt,
                      pseudos=pseudos, workflow=workflow, sysdone=True,
                      prefix=prefix)


def main():
    from argparse import ArgumentParser
    import inspect

    from openmdao.main.assembly import Assembly, set_as_top

    parser = ArgumentParser()
    parser.add_argument('-m', '--module', action='store', dest='module',
                        metavar='MODULE',
                        help='name of module that contains the class to be instantiated and graphed')
    parser.add_argument('-c', '--class', action='store', dest='klass',
                        help='specify class in module to plot graphs for')
    parser.add_argument('-f', '--fmt', action='store', dest='fmt', default='pdf',
                        help='specify output format')
    parser.add_argument('-r', '--recurse', action='store_true', dest='recurse',
                        help='if set, recurse down and plot all dependency, component,  and derivative graphs')
    parser.add_argument('-p', '--pseudos', action='store_false', dest='pseudos',
                        help='if set, include pseudo components in graphs')
    parser.add_argument('-w', '--workflow', action='store_true', dest='workflow',
                        help='if set, group graph components into workflows')


    options = parser.parse_args()

    if options.module is None:
        parser.print_help()
        sys.exit(-1)

    __import__(options.module)

    mod = sys.modules[options.module]

    if options.klass:
        obj = getattr(mod, options.klass)()
    else:
        def isasm(obj):
            try:
                return issubclass(obj, Assembly) and obj is not Assembly
            except:
                return False

        klasses = inspect.getmembers(mod, isasm)
        if len(klasses) > 1:
            print "found %d Assembly classes. you must specify 1" % len(klasses)
            for i, (cname, klass) in enumerate(klasses):
                print "%d) %s" % (i, cname)
            var = raw_input("\nEnter a number: ")
            obj = klasses[int(var)][1]()
            sys.exit(-1)
        elif klasses:
            obj = klasses[0][1]()
        else:
            print "No classes found"

    set_as_top(obj)
    if not obj.get_pathname():
        obj.name = 'top'

    plot_graphs(obj, recurse=options.recurse, fmt=options.fmt, pseudos=options.pseudos,
                workflow=options.workflow)


if __name__ == '__main__':
    main()
