import os
import sys
import time
import shutil
import json
import tempfile
import webbrowser


from networkx.readwrite.json_graph import node_link_data

_excluded_nodes = set([
    'force_execute',
    'directory',
    'external_files',
    'log_level',
    'exec_count',
    'derivative_exec_count',
    'itername',
    'create_instance_dir',
    'printvars',
])

_excluded_node_data = set([
    'pa_object',
])

_excluded_link_data = set([
    'sexpr',
    'dexpr',
])

def _to_id(name):
    """Convert a given name to a valid html id, replacing
    dots with hyphens."""
    return name.replace('.', '-')

def _clean_graph(graph):
    """Return a cleaned version of the graph. Note that this
    should not be used for really large graphs because it 
    copies the entire graph.
    """
    # copy the graph since we're changing node/edge metadata
    graph = graph.subgraph(graph.nodes_iter())

    conns = graph.list_connections()

    conn_nodes = set([u.split('[',1)[0] for u,v in conns])
    conn_nodes.update([v.split('[',1)[0] for u,v in conns])

    nodes_to_remove = []
    for node, data in graph.nodes_iter(data=True):
        parts = node.split('.',1)
        if len(parts) == 1:
            name = node
        else:
            name = parts[1]
            if '[' not in node and node not in conn_nodes:
                nodes_to_remove.append(node)
                continue

        if name in _excluded_nodes:
            nodes_to_remove.append(node)
        else: # update node metadata
            newdata = data
            for meta in _excluded_node_data:
                if meta in newdata:
                    if newdata is data:
                        newdata = dict(data) # make a copy of metadata since we're changing it
                        graph.node[node] = newdata
                    del newdata[meta]

    graph.remove_nodes_from(nodes_to_remove)

    for u,v,data in graph.edges_iter(data=True):
        newdata = data
        for meta in _excluded_link_data:
            if meta in newdata:
                if newdata is data:
                    newdata = dict(data)
                    graph.edge[u][v] = newdata
                del newdata[meta]

    try:
        for i,comp in enumerate(graph.component_graph()):
            graph.node[comp]['color_idx'] = i
    except AttributeError:
        pass

    # add some extra metadata to make things easier on the
    # javascript side
    for node, data in graph.nodes_iter(data=True):
        parts = node.split('.', 1)
        if len(parts) == 1:
            data['short'] = node
        else:
            data['short'] = parts[1]
            try:
                data['color_idx'] = graph.node[parts[0]]['color_idx']
            except KeyError:
                pass

    return graph

def plot_graph(graph, d3page='fixedforce.html'):
    """Open up a display of the graph in a browser window."""

    tmpdir = tempfile.mkdtemp()
    fdir = os.path.dirname(os.path.abspath(__file__))
    shutil.copy(os.path.join(fdir, 'd3.js'), tmpdir)
    shutil.copy(os.path.join(fdir, d3page), tmpdir)

    graph = _clean_graph(graph)
    data = node_link_data(graph)

    startdir = os.getcwd()
    os.chdir(tmpdir)
    try:
        # write out the json as a javascript var
        # so we we're not forced to start our own webserver
        # to avoid cross-site issues
        with open('__graph.js', 'w') as f:
            f.write("__mygraph__json = ")
            json.dump(data, f)
            f.write(";\n")

        # open URL in web browser
        wb = webbrowser.get()
        wb.open('file://'+os.path.join(tmpdir, d3page))
    finally:
        os.chdir(startdir)
        print "\nwaiting to remove temp directory '%s'... " % tmpdir
        time.sleep(5) # sleep to give browser time
                       # to read files before we remove them
        shutil.rmtree(tmpdir)
        print "temp directory removed"


def main():
    from argparse import ArgumentParser
    import inspect

    from openmdao.main.assembly import Assembly, set_as_top
    from openmdao.main.driver import Driver

    parser = ArgumentParser()
    parser.add_argument('-m', '--module', action='store', dest='module',
                        metavar='MODULE',
                        help='name of module that contains the class to be instantiated and graphed')
    parser.add_argument('-c', '--class', action='store', dest='klass',
                        help='boolean expression to filter hosts')
    parser.add_argument('-r', '--recurse', action='store_true', dest='recurse',
                        help='if set, recurse down and plot all dependency and derivative graphs')

    options = parser.parse_args()
    
    if options.module is None:
        parser.print_help()
        sys.exit(-1)

    def get_graphs(obj):
        graphs = []
        if isinstance(obj, Assembly):
            graphs.append((obj.name, obj._depgraph))
            graphs.extend(get_graphs(obj.driver))
        elif isinstance(obj, Driver):
            graphs.append((obj.get_pathname(), obj.workflow.derivative_graph()))
            for comp in obj.iteration_set():
                if isinstance(comp, Assembly) or isinstance(comp, Driver):
                    graphs.extend(get_graphs(comp))
        return graphs
                
    __import__(options.module)

    mod = sys.modules[options.module]

    if options.klass:
        obj = getattr(mod, options.klass)()
    else:
        def isasm(obj):
            return issubclass(obj, Assembly)

        klasses = inspect.getmembers(mod, isasm)
        if len(klasses) > 1:
            print "found %d Assembly classes.  pick one" % len(klasses)
            for i, klass in enumerate(klasses):
                print "%d) %s" % (i, klass.__name__)
            var = raw_input("\nEnter a number: ")
            obj = klasses[int(var)]

    set_as_top(obj)

    if options.recurse:
        graphs = get_graphs(obj)
    else:
        graphs = [('top', obj._depgraph)]

    for name, graph in graphs:
        plot_graph(graph)


if __name__ == '__main__':
    main()




