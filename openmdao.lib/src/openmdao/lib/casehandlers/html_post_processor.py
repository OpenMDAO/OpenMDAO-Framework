def caseset_query_to_html(query, filename='cases.html'):

    with open(filename, 'w') as fh:
        fh.write("<html>")
        fh.write('''
        <head>
        <link rel='stylesheet' type='text/css' href='http://w2ui.com/src/w2ui-1.4.1.min.css' />
        <script src='http://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js'></script>
        <script type='text/javascript' src='http://w2ui.com/src/w2ui-1.4.1.min.js'></script>
        </head>
        ''')
        fh.write("<body>")
        fh.write('''<div id='main' style='width: 100%; height: 400px;'></div>''')
        fh.write('''<script type='application/json' id='case-data'>''')

        #Write JSON data to file
        query.write(fh)

    #Have to reopen because query.write closes the file
    with open(filename, 'a') as fh:
        #Closes script tag for JSON data
        fh.write("</script>")
        fh.write('''
        <script type='text/javascript'>
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



        jQuery( document ).ready(function() {

                var cases = [];
                var it_case_key = "iteration_case_" ;
                var simulation_info_key = "simulation_info" ;
                var case_objects = new Object();
                var simulation_info_id = "" ;
                // var driver_info_key = "driver_info_" ;
                // var driver_names = new Object();

                // get all the cases in an associative array with the id as the key
                jQuery.each( case_data, function( key , val ) {

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


        jQuery(function () {
             // initialization
            jQuery('#main').w2layout(config.layout);
            w2ui.layout.content('left', jQuery().w2sidebar(
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
        </script>
        ''')

        #Close remaining tags
        fh.write("</body>")
        fh.write("</html>")
