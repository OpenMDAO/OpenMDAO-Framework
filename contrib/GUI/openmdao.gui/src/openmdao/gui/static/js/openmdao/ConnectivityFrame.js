/**
 *  ConnectivityFrame: a frame for viewing/editing connections in an assembly
 *
 *  (Experimental version using dagre-d3.js)
 *
 *  Source (output) and destination (input) variables are rendered as SVG
 *  figures with curves between them representing connections. Dragging from
 *  one variable figure to another will connect them if they are eligible to
 *  be connected.
 *
 *  Arguments:
 *      project:  object that provides access to the openmdao project
 *      pathname: the pathname of the assembly
 **/

var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.ConnectivityFrame = function(project, pathname) {
    var id = ('ConnectivityFrame-'+pathname).replace(/\./g,'-');
    openmdao.ConnectivityFrame.prototype.init.call(this, id,
        'Connections: '+openmdao.Util.getName(pathname));

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        connectionsCSS = 'background:grey; position:relative; top:0px; width:100%;',
        connectionsDiv = jQuery('<div id="'+id+'-connections" style="'+connectionsCSS+'">')
            .appendTo(self.elm),
        connectionsSVG = jQuery('<svg width=1024 height=720>')
            .appendTo(connectionsDiv),
        line, line_start, line_end;

    self.elm.css({'position':'relative', 'height':'100%',
                  'overflow':'auto', 'max-height': 0.8*jQuery(window).height()});

    self.pathname = null;

    // prevent selection
    connectionsSVG.attr('unselectable', 'on')
                  .css('user-select', 'none')
                  .on('selectstart', false);

    /** populate connections and variable selectors with source and dest variables */
    function loadConnectionData(data) {
        // Create a new directed graph
        var g = new dagreD3.Digraph();

        var connected = [];
        jQuery.each(data.edges, function(idx, connection) {
            connected.push(connection[0]);
            connected.push(connection[1]);
        });

        jQuery.each(data.nodes, function(name, attr) {
            g.addNode(name, {
                'label': name,
                'type':  attr.type,
                'units': attr.units,
                'io':    attr.io,
                'connected': jQuery.inArray(name, connected) >= 0
            });
        });

        jQuery.each(data.edges, function(idx, connection) {
            g.addEdge(null, connection[0], connection[1]);
        });

        var svg = d3.select(connectionsSVG[0])
                    .on("mouseup", mouseup);

        function mouseover(d) {
            var node = d3.select(this);
            node.attr('fill', 'steelblue');
            if (line_start) {
                line_end = node;
            }
        }

        function mouseout(d) {
            var node = d3.select(this);
            node.attr('fill', 'white');
            line_end = undefined;
        }

        function mousedown(d) {
            var node = d3.select(this);
            var m = d3.mouse(connectionsSVG[0]);
            line_start = node;
            line = svg.append('line')
                .attr('x1', m[0])
                .attr('y1', m[1])
                .attr('x2', m[0])
                .attr('y2', m[1])
                .attr('stroke', '#333');
            svg.on('mousemove', mousemove);
        }

        function mousemove(d) {
            var m = d3.mouse(connectionsSVG[0]);
            line.attr('x2', m[0])
                .attr('y2', m[1]);
        }

        function mouseup(d) {
            var m = d3.mouse(connectionsSVG[0]);
            svg.on('mousemove', null);
            svg.select('line').remove();
            if (line_start && line_end) {
                connect(line_start, line_end);
                line_start = undefined;
                line_end = undefined;
            }
            else {
                line_start = undefined;
            }
        }

        var renderer = new dagreD3.Renderer();

        var oldDrawNodes = renderer.drawNodes();
        renderer.drawNodes(function(graph, root) {
            var svgNodes = oldDrawNodes(graph, root);
            svgNodes.each(function(u) {
                var node = d3.select(this);
                node.classed({
                    'input'    : graph.node(u).io === 'input',
                    'output'   : graph.node(u).io === 'output',
                    'expr'     : graph.node(u).io === 'expr',
                    'connected': graph.node(u).connected
                });
            });
            return svgNodes;
        });

        var layout = dagreD3.layout()
                      .nodeSep(20)
                      .rankDir('LR');

        layout = renderer.layout(layout).run(g, svg);

        svg.selectAll('.node')
            .attr('fill', '#fff')
            .attr('stroke', '#333')
            .attr('stroke-width', '1px')
            .on('mouseover', mouseover)
            .on('mouseout', mouseout)
            .on('mousedown', mousedown);

        svg.selectAll('.edgePath')
            .attr('fill', 'none')
            .attr('stroke', '#333')
            .attr('stroke-width', '1.5px');

        svg.selectAll('.output').style('stroke', 'blue');
        svg.selectAll('.input').style('stroke', 'green');
        svg.selectAll('.input.connected').style('stroke', 'red');
        svg.selectAll('.expr').style('fill', 'gray');

        svg.attr('width', layout.graph().width + 40)
           .attr('height', layout.graph().height + 40);
    }

    function connect(source, target) {
        debug.info('connecting', source, target);
        var cmd = self.pathname + '.connect("'
                + source.data()[0] + '", "'
                + target.data()[0] + '")';
        debug.info('cmd', cmd);
        project.issueCommand(cmd);
    }

    /** handle message containing the assembly connection data (dataflow) */
    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid component data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            loadConnectionData(message[1]);
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** if there is an object loaded, update it from the project */
    this.update = function() {
        if (self.pathname && self.pathname.length > 0) {
            self.editAssembly(self.pathname,self.src_comp,self.dst_comp);
        }
    };

    /** populate frame with connection data for the specified assembly */
    this.editAssembly = function(path) {
        if (self.pathname !== path) {
           if (self.pathname !== null) {
                project.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            project.addListener(self.pathname, handleMessage);
        }

        project.getConnectivity(path)
            .done(loadConnectionData)
            .fail(function(jqXHR, textStatus, errorThrown) {
                debug.warn('ConnectivityFrame.editAssembly() Error:',
                            jqXHR, textStatus, errorThrown);
                // assume component has been deleted, so close frame
                self.close();
            });
    };

    this.destructor = function() {
        if (self.pathname && self.pathname.length>0) {
            project.removeListener(self.pathname, handleMessage);
        }
    };

    this.editAssembly(pathname);
};

/** set prototype */
openmdao.ConnectivityFrame.prototype = new openmdao.BaseFrame();
openmdao.ConnectivityFrame.prototype.constructor = openmdao.ConnectivityFrame;
