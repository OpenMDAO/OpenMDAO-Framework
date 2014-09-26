import os
import tempfile
from jinja2 import Environment, FileSystemLoader
import networkx as nx

from openmdao.lib.casehandlers.api import CaseDataset

def caseset_query_to_html(query, filename='cases.html'):

    # Write query results to JSON file
    # Wish I could write to a string or JSON object
    tmpfile = tempfile.NamedTemporaryFile('w')
    query.write(tmpfile.name)
    with open(tmpfile.name, 'rt') as f:
        case_data = f.read()

    # Need to get at the depgraph data in this CaseDataSet file
    cds = CaseDataset(tmpfile.name, 'json')
    tmpfile = tempfile.NamedTemporaryFile('w')
    with open(tmpfile.name, "w") as f:
        f.write(cds.simulation_info['graph'])
    with open(tmpfile.name) as f:
        G = nx.readwrite.json_graph.load(f)

    # depgraph is supposed to be openmdao.main.depgraph.DependencyGraph
    # But what comes back from json_graph is networkx.classes.digraph.DiGraph
    # So cannot directly call this
    #plot_graph( depgraph, fmt='plain' )

    # Write the DiGraph to a dot file so we can get at the layout info
    dotfile = 'g.dot'
    dot_output_plain_formatted_file = 'g.plain'
    fmt = 'plain'
    nx.write_dot(G, dotfile)
    # TODO: Set the size of the output. See this
    #         http://stackoverflow.com/questions/14784405/how-to-set-the-output-size-in-graphviz-for-the-dot-format
    os.system("dot -T%s -o %s %s" % (fmt, dot_output_plain_formatted_file, dotfile))

    # read the graphviz layout info from the plain text file
    with open(dot_output_plain_formatted_file, "r") as f:
      node_content = ''
      spline_data = []
      for i, line in enumerate(f):
        # graph scale width height
        if line.startswith( "graph"):
            dummy, graph_scale, graph_width, graph_height = line.split()
        elif line.startswith( "node"):
            # Format of this line is
            #     node name x y width height label style shape color fillcolor
            # shape can be ellipse, diamond, box or invhouse
            dummy, name, x, y, width, height, label, style, shape, color, fillcolor = line.split()
            if name.startswith('"'):
                name = name[1:-1]
            if label.startswith('"'):
                label = label[1:-1]
            node_content += '{ name: "%s", id:"%s", x:%s, y:%s, shape:"%s", fixed:true},\n' % ( label, name, x, y, shape )
        elif line.startswith( "edge"):
            # Format of this line is
            #     edge tail head n x1 y1 .. xn yn [label xl yl] style color
            dummy, tail, head, n = line.split()[:4]
            if tail.startswith('"'):
                tail = tail[1:-1]
            if head.startswith('"'):
                head = head[1:-1]
            control_points = []
            for i in range(int(n)):
                control_points.append( (line.split()[4+2*i], line.split()[5+2*i]) )
            spline_data.append( control_points )

    # clean up
    for graphviz_files in (dotfile, dot_output_plain_formatted_file):
        if os.path.exists(graphviz_files):
            os.remove(graphviz_files)


    # using the info on http://www.graphviz.org/content/how-convert-b-spline-bezier
    # also, using http://www.d3noob.org/2013/03/d3js-force-directed-graph-example-basic.html to add the arrowheads
    svg_path_edges = ''
    for control_points in spline_data:
        svg_path_edges += '''svg.append("svg:path")
                    .attr("d","'''

        svg_path_edges += 'M " + xscale(%s) + " " + yscale(%s) + " "' % control_points[0]
        num_bezier = ( len(control_points) - 1 ) / 3
        for ib in range(num_bezier):
            if ib == 0 :
                svg_path_edges += ' + "C " + xscale(%s) + " " + yscale(%s) + " " + xscale(%s) + " " + yscale(%s) + " " + xscale(%s) + " " + yscale(%s) ' % ( control_points[ 1 + 3 * ib ] + control_points[ 2 + 3 * ib ] + control_points[ 3 + 3 * ib ] )
            else:
                svg_path_edges += ' + " " + xscale(%s) + " " + yscale(%s) + " " + xscale(%s) + " " + yscale(%s) + " " + xscale(%s) + " " + yscale(%s) ' % ( control_points[ 1 + 3 * ib ] + control_points[ 2 + 3 * ib ] + control_points[ 3 + 3 * ib ] )
        svg_path_edges += ''')
                    .style("stroke-width", 2)
                    .style("stroke", "black")
                    .style("fill", "none")
                    .attr("class", "link")
                    .attr("marker-end", "url(#end)")
                    ;'''


    # Setup Jinja2 templating
    cds_visualizer_dir_path = os.path.join(
        os.path.dirname(__file__), 'visual_post_processing')
    env = Environment(loader=FileSystemLoader(cds_visualizer_dir_path))
    template = env.get_template('case_dataset_visualizer.html')

    if os.path.isfile(filename):
        os.remove(filename)

    # use the existing template to write out the html file
    outputText = template.render( case_data = case_data,
                        node_content=node_content,
                        svg_path_edges=svg_path_edges,
                        graph_width = graph_width,
                        graph_height = graph_height,
                         )
    with open(filename, "wb") as fh:
        fh.write(outputText)

if __name__ == "__main__":
    import sys

    if len(sys.argv) < 3:
        sys.exit('Usage: %s case_records_json_file output_html_file' % sys.argv[0])

    if not os.path.exists(sys.argv[1]):
        sys.exit('ERROR: Case records JSON file %s was not found!' % sys.argv[1])


    cds = CaseDataset(sys.argv[1], 'json')
    data = cds.data # results

    caseset_query_to_html(data, filename=sys.argv[2])
