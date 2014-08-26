import StringIO

def query_to_html(query, filename='cases.html'):
    case_data = StringIO.StringIO()
    query.write(case_data)

    css = gen_void_element(
        'link',
        gen_attributes({
            'rel'  : "stylesheet",
            'type' : "text/css",
            'href' : "http://w2ui.com/src/w2ui-1.4.1.min.css",
        })
    )

    jquery = gen_normal_element(
        'script',
        attributes = gen_attributes({
            'src' : "http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js",
        })
    )

    w2ui = gen_normal_element(
        'script',
        attributes = gen_attributes({
            'type' : "text/javascript",
            'src'  : "http://w2ui.com/src/w2ui-1.4.1.min.js",
        })
    )

    case_visualizer =
    '''
    //global variable for reference case data
    case_data = JSON.parse(document.getElementById('case-data').innerHTML);
    // recursive function to build up the JavaScript structure that w2ui needs for
    //   display of a sidebar
    function get_nodes( root ) {
        var nodes = [] ;
        var children = root.children ;
        var child ;

        for (var i=0; i < children.length; i++) {
            child = children[i] ;
            if ( child.children.length ) {
                child_nodes = get_nodes( child ) ;
                nodes[nodes.length] = {id: child.case_number.toString(), text: child.case_number.toString(), nodes: child_nodes } ;
            }
            else {
                nodes[nodes.length] = {id: child.case_number.toString(), text: child.case_number.toString() } ;
            }
        }
        return nodes ;
    }

    // widget configuration
    var config = {
        layout: {
            name: 'layout',
            padding: 0,
            panels: [
                { type: 'left', size: 200, resizable: true, minSize: 120 },
                { type: 'main',  minSize: 550, overflow: 'hidden' }
            ]
        },
    }



    $( document ).ready(function() {

            var cases = [];
            var it_case_key = "iteration_case_" ;
            var simulation_info_key = "simulation_info" ;
            var case_objects = new Object();
            var simulation_info_id = "" ;
            // var driver_info_key = "driver_info_" ;
            // var driver_names = new Object();

            // get all the cases in an associative array with the id as the key
            $.each( data, function( key , val ) {

               // need to add simulation info object for top of tree of cases
               if (key.lastIndexOf(simulation_info_key, 0) === 0) {
                    val[ 'children'] = [] ;
                    case_objects[ val.uuid ] = val ;
                    simulation_info_id = val.uuid ; // need to remember this since it is special since it is not really a case
                }

                // Make a structure to hold all of the case info
                if (key.lastIndexOf(it_case_key, 0) === 0) {
                    case_number = key.substring(it_case_key.length);
                    val[ 'children'] = [] ;
                    val[ 'case_number'] = case_number ;
                    case_objects[ val._id ] = val ;
                }
            }); // end each

            // Loop over the case_objects and connect parents to children and vice versa
            Object.keys(case_objects).forEach(function(key) {
                var case_object, parent_id, parent_case_object, parent_case_object_children ;
                if ( key != simulation_info_id )
                {
                    case_object = case_objects[key];
                    parent_id = case_object._parent_id ;
                    parent_case_object = case_objects[parent_id] ;
                    parent_case_object_children = parent_case_object.children ;
                    parent_case_object_children[ parent_case_object_children.length ] = case_object ;
                }
            });

            // Starting from the top "case_object", which is really the sim info data,
            // Walk down the children leaves, creating the value for nodes in the w2ui call below
            simulation_info_object = case_objects[simulation_info_id] ;
            node_tree = get_nodes(simulation_info_object);


    $(function () {
         // initialization
        $('#main').w2layout(config.layout);
        w2ui.layout.content('left', $().w2sidebar(
            {
                name: 'cases',
                nodes: node_tree ,
                onClick: function(event) {
                    w2ui.layout.content('main', '<div style="padding: 10px">case ' + event.target + ' clicked</div>' );
                }  ,
            }
        ));
    });


    }); // end ready
    '''
    case_data = gen_normal_element(
        'script'
        attributes=gen_attributes({
            'type' : "application/json",
            'id'   : "case-data",
        }),
        text=case_data.write(),
    )

    case_visualizer = gen_normal_element(
        'script',
        attributes=gen_attributes({
            'type' : 'text/javascript',
        }),
        text=case_visualizer
    )

    head = gen_normal_element(
        'head',
        text = css + w2ui + jquery
    )

    body = gen_normal_element(
        'body',
        text=case_data + case_visualizer
    )

    html = gen_normal_element(
        'html',
        text=head + body
    )

    with open(filename, 'w') as fg:
        fh.write(html)

def gen_attributes(attributes):
    attr_string = ['{attr}={value}'.format(attr=attr, value=value) for attr,value in attributes.iteritems()]
    attr_string = attr_string.join(' ')

    return attr_string

def gen_normal_element(element, text='', attributes=''):
    if attributes:
        element = "<{tag} {attributes}>{text}</{tag}>"
        element = element.format(
            tag=tag,
            attributes=attributes,
            text=text)

    else:
        element = "<{tag}>{text}</{tag}>"
        element = element.format(
            tag=tag,
            text=text)

    return element

def gen_void_element(element, attributes):

    element = "<{tag} {attributes}/ >"
    element = element.format(
        tag=tag,
        attributes=attributes)
