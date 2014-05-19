
import os
import sys
import webbrowser

import networkx as nx
from openmdao.main.interfaces import IDriver
from openmdao.main.depgraph import DependencyGraph, is_var_node

_cluster_count = 0

def write_driver_cluster(f, G, driver, indent):
    global _cluster_count, IDriver
    comps = list(driver.workflow)
    tab = ' '*indent
    f.write('%ssubgraph cluster%s {\n' % (tab, _cluster_count))
    _cluster_count += 1
    indent += 3
    tab = ' '*indent

    f.write('%s%s;\n' % (tab, driver.name))
    if len(comps) > 0:
        dcount = 1
        for comp in comps:
            if IDriver.providedBy(comp):
                write_driver_cluster(f, G, comp, indent)
                f.write("%s%s -> %s [style=dashed, label=%d];\n" % (tab, driver.name, comp.name, dcount))
                dcount += 1

        subG = G.subgraph([c.name for c in comps])

        write_nodes(f, subG, indent)

        for u,v in subG.edges():
            f.write('%s%s -> %s;\n' % (tab, u, v))

    f.write('%s}\n' % tab)

def write_nodes(f, G, indent):
    tab = ' '*indent
    for node, data in G.nodes_iter(data=True):
        if 'driver' in data:
            data['shape'] = 'invhouse'
        elif 'pseudo' in data:
            data['shape'] = 'diamond'
            data['label'] = node.replace('_pseudo_', '_p_')
        elif 'comp' in data:
            data['shape'] = 'box'
        else: # var node
            data['shape'] = 'ellipse'
            parts = node.split('.', 1)
            if len(parts) > 1 and not is_var_node(G, parts[0]):
                data['label'] = parts[1]
            base = G.base_var(node)
            if G.node[base].get('iotype') == 'state':
                data['shape'] = 'doubleoctagon'
        data['margin'] = '0.0,0.0'
        f.write('%s"%s" [' % (tab, node))
        for i,(key,val) in enumerate(data.items()):
            if i > 0:
                f.write(", ")
            f.write("%s=%s" % (key,val))
        f.write("];\n")

def write_dot(G, dotfile, scope=None):

    if scope is None:
        raise RuntimeError("if workflow is True, scope must be specified")

    with open(dotfile, 'w') as f:
        f.write("strict digraph {\n")
        #f.write("rankdir=RL;\n")
        driver = getattr(scope, 'driver')
        write_driver_cluster(f, G, driver, 3)

        write_nodes(f, G, 3)

        # now include any cross workflow connections
        for u,v in G.edges_iter():
            f.write("   %s -> %s;\n" % (u, v))

        f.write("}\n")

def plot_graph(G, fmt='pdf', outfile=None, pseudos=False,
               workflow=False, scope=None):
    """Create a plot of the given graph"""

    G = G.copy()

    if isinstance(G, DependencyGraph):
        G.prune_unconnected_vars()

    if not pseudos:
        nodes = [n for n in G.nodes() if '_pseudo_' in n]
        G.remove_nodes_from(nodes)

    if outfile is None:
        outfile = 'graph.'+fmt

    dotfile = os.path.splitext(outfile)[0]+'.dot'

    if workflow:
        write_dot(G, dotfile, scope)
    else: # just show data connections
        for node, data in G.nodes_iter(data=True):
            if 'driver' in data:
                data['shape'] = 'invhouse'
            elif 'pseudo' in data:
                data['shape'] = 'diamond'
                data['label'] = node.replace('_pseudo_', '_p_')
            elif 'comp' in data:
                data['shape'] = 'box'
            else: # var node
                data['shape'] = 'ellipse'
                parts = node.split('.', 1)
                if len(parts) > 1 and not is_var_node(G, parts[0]):
                    data['label'] = parts[1]
                base = G.base_var(node)
                if G.node[base].get('iotype') == 'state':
                    data['shape'] = 'doubleoctagon'
            data['margin'] = '0.0,0.0'
        nx.write_dot(G, dotfile)

    os.system("dot -T%s -o %s %s" % (fmt, outfile, dotfile))

    webbrowser.get().open(outfile)

    #os.remove(dotfile)

def plot_graphs(obj, recurse=True, fmt='pdf', pseudos=False, workflow=False):
    from openmdao.main.assembly import Assembly
    from openmdao.main.driver import Driver

    print "plot_graphs for %s" % obj.name

    if isinstance(obj, Assembly):
        if obj.name == '':
            obj.name = 'top'
        try:
            plot_graph(obj._depgraph, fmt=fmt, outfile=obj.name+'_depgraph'+'.'+fmt, pseudos=pseudos)
        except Exception as err:
            print "Can't plot depgraph of '%s': %s" % (obj.name, str(err))
        try:
            plot_graph(obj._depgraph.component_graph(),
                       fmt=fmt, outfile=obj.name+'_compgraph'+'.'+fmt,
                       pseudos=pseudos, workflow=workflow, scope=obj)
        except Exception as err:
            print "Can't plot component_graph of '%s': %s" % (obj.name, str(err))
        if recurse:
            plot_graphs(obj.driver, recurse, fmt=fmt, pseudos=pseudos, workflow=workflow)
    elif isinstance(obj, Driver):
        try:
            plot_graph(obj.workflow.derivative_graph(),
                       fmt=fmt, outfile=obj.name+"_derivgraph"+'.'+fmt,
                       pseudos=pseudos, workflow=workflow, scope=obj.parent)
        except Exception as err:
            print "Can't plot deriv graph of '%s': %s" % (obj.name, str(err))

        if recurse:
            for comp in obj.iteration_set():
                if isinstance(comp, Assembly) or isinstance(comp, Driver):
                    plot_graphs(comp, recurse, fmt=fmt, pseudos=pseudos, workflow=workflow)



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
