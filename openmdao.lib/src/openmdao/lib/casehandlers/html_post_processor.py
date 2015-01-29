import os
from jinja2 import Environment, FileSystemLoader
import networkx as nx

from openmdao.lib.casehandlers.query import CaseDataset

import StringIO
import json
import webbrowser


def caseset_query_to_html(query, filename='cases.html'):

    # get case data as serialized json string
    q_str = StringIO.StringIO()
    query.write(q_str)
    case_data = q_str.getvalue()
    json_data = json.loads(case_data)

    # get dependency graph from case data and render it to SVG
    graph = json.loads(json_data['simulation_info']['graph'])
    G = nx.readwrite.json_graph.node_link_graph(graph)
    agraph = nx.to_agraph(G)
    svg_dep_graph = agraph.draw(format='svg', prog='dot')

    # get component graph from case data and render it to SVG
    if 'comp_graph' in json_data['simulation_info'].keys():
        graph = json.loads(json_data['simulation_info']['comp_graph'])
        G = nx.readwrite.json_graph.node_link_graph(graph)
        agraph = nx.to_agraph(G)
        svg_comp_graph = agraph.draw(format='svg', prog='dot')
    else:
        # old case data file may not have comp_graph
        svg_comp_graph = '<svg></svg>'

    # Setup Jinja2 templating
    cds_visualizer_dir_path = os.path.join(os.path.dirname(__file__), 'visual_post_processing')
    env = Environment(loader=FileSystemLoader(cds_visualizer_dir_path))
    template = env.get_template('case_dataset_visualizer.html')

    if os.path.isfile(filename):
        os.remove(filename)

    # use the existing template to write out the html file
    outputText = template.render(
        case_data=case_data,
        svg_dep_graph=svg_dep_graph,
        svg_comp_graph=svg_comp_graph
    )
    with open(filename, "wb") as fh:
        fh.write(outputText)

def run(parser=None, options=None, args=None):
    """ Convert JSON to HTML and launch browser.
    """
    if not os.path.exists(options.json_file):
        sys.exit('ERROR: Case records JSON file %s was not found!' % options.json_file)

    data = CaseDataset(options.json_file, 'json').data

    html_file = options.json_file+'.html'
    caseset_query_to_html(data, filename=html_file)

    browser = webbrowser.get()
    browser.open(html_file, 1, True)


if __name__ == "__main__":
    import sys

    if len(sys.argv) == 3:
        json_file = sys.argv[1]
        html_file = sys.argv[2]
    elif len(sys.argv) == 2:
        json_file = sys.argv[1]
        html_file = sys.argv[1]+'.html'
    else:
        sys.exit('Usage: %s case_records_json_file output_html_file' % sys.argv[0])

    if not os.path.exists(json_file):
        sys.exit('ERROR: Case records JSON file %s was not found!' % sys.argv[1])

    cds = CaseDataset(json_file, 'json')
    data = cds.data  # results

    caseset_query_to_html(data, filename=html_file)

    browser = webbrowser.get()
    browser.open(html_file, 1, True)
