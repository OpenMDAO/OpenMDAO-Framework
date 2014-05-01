
import os
import sys
import webbrowser

import networkx as nx

def plot_graph(G, fmt='pdf', outfile=None, pseudos=False):
    """Create a plot of the given graph"""

    if not pseudos:
        nodes = [n for n in G.nodes() if '_pseudo_' in n]
        G.remove_nodes_from(nodes)

    if outfile is None:
        outfile = 'graph.'+fmt

    dotfile = os.path.splitext(outfile)[0]+'.dot'

    nx.write_dot(G, dotfile)

    os.system("dot -T%s -o %s %s" % (fmt, outfile, dotfile))

    webbrowser.get().open(outfile)

    os.remove(dotfile)


def plot_graphs(obj, recurse=False, fmt='pdf', pseudos=False):
    from openmdao.main.assembly import Assembly
    from openmdao.main.driver import Driver

    if isinstance(obj, Assembly):
        if obj.name == '':
            obj.name = 'top'
        try:
            plot_graph(obj._depgraph, fmt=fmt, outfile=obj.name+'_depgraph', pseudos=pseudos)
        except Exception as err:
            print "Can't plot depgraph of '%s': %s" % (obj.name, str(err))
        try:
            plot_graph(obj._depgraph.component_graph(), fmt=fmt, outfile=obj.name+'_compgraph', pseudos=pseudos)
        except Exception as err:
            print "Can't plot component_graph of '%s': %s" % (obj.name, str(err))
        if recurse:
            plot_graphs(obj.driver, recurse, fmt=fmt, pseudos=pseudos)
    elif isinstance(obj, Driver):
        try:
            plot_graph(obj.workflow.derivative_graph(), fmt=fmt, outfile=obj.name+"_derivgraph", pseudos=pseudos)
        except Exception as err:
            print "Can't plot deriv graph of '%s': %s" % (obj.name, str(err))
        if recurse:
            for comp in obj.iteration_set():
                if isinstance(comp, Assembly) or isinstance(comp, Driver):
                    plot_graphs(comp, recurse, fmt=fmt, pseudos=pseudos)



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
    parser.add_argument('-p', '--pseudos', action='store_true', dest='pseudos',
                        help='if set, include pseudo components in graphs')


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
                return issubclass(obj, Assembly)
            except:
                return False

        klasses = inspect.getmembers(mod, isasm)
        if len(klasses) > 1:
            print "found %d Assembly classes. you must specify 1" % len(klasses)
            # for i, klass in enumerate(klasses):
            #     print "%d) %s" % (i, klass.__name__)
            # var = raw_input("\nEnter a number: ")
            # obj = klasses[int(var)]
            sys.exit(-1)

    set_as_top(obj)
    if not obj.get_pathname():
        obj.name = 'top'

    plot_graphs(obj, recurse=options.recurse, fmt=options.fmt, pseudos=options.pseudos)


if __name__ == '__main__':
    main()
