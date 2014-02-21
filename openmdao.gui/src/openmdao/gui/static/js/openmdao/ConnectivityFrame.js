/**
 *  ConnectivityFrame: a frame for viewing/editing connections in an assembly
 *
 *  Source and destination components are selected via input boxes at the top
 *  of the frame. Source (output) and destination (input) variables are
 *  rendered as SVG figures with curves between them representing connections.
 *  Source and destination variables can be selected via input boxes at the
 *  bottom of the frame and connected by clicking on the 'connect' button.
 *  Alternatively, dragging from one variable figure to another will connect
 *  them if they are eligible to be connected. Input variables can be
 *  disconnected by right clicking on them and choosing 'Disconnect' from the
 *  context menu.
 *
 *  Arguments:
 *      project:  object that provides access to the openmdao project
 *      pathname: the pathname of the assembly
 *      src_comp: (optional) the source component to select initially
 *      dst_comp: (optional) the destination component to select initially
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
        connectionsCSS = 'background:grey; position:relative; top:0px; width:100%; overflow-x:hidden; overflow-y:auto;',
        connectionsDiv = jQuery('<div id="'+id+'-connections" style="'+connectionsCSS+'">')
            .appendTo(self.elm)
            .attr('unselectable', 'on')
            .css('user-select', 'none')
            .on('selectstart', false),
        connectionsSVG = jQuery('<svg width=1000 height=1000>')
            .appendTo(connectionsDiv),
        line, line_start, line_end;

    self.elm.css({'position':'relative', 'height':'100%',
                  'overflow':'hidden', 'max-height': 0.8*jQuery(window).height()});

    self.pathname = null;

    /** populate connections and variable selectors with source and dest variables */
    function loadConnectionData(data) {
        // Create a new directed graph
        var g = new dagreD3.Digraph();

        jQuery.each(data.sources, function(name, source) {
            g.addNode(name, {
                label: name,
                type: source.type,
                units: source.units,
                connected: (source.connected && source.connected.length > 0),
                source: true,
                target: (source.type == 'expr' ? true : false)
            });
        });

        jQuery.each(data.targets, function(name, target) {
            if (! g.hasNode(name)) {
                g.addNode(name, {
                    label: name,
                    type: target.type,
                    units: target.units,
                    connected: (target.connected),
                    source: (target.type == 'expr' ? true : false),
                    target: true
                });
            }
        });

        jQuery.each(data.sources, function(name, source) {
            if (source.connected) {
                for (var i=0; i<source.connected.length; i++) {
                    g.addEdge(null, name, source.connected[i]);
                }
            }
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

        var layout = dagreD3.layout()
                      .nodeSep(20)
                      .rankDir('LR');

        var renderer = new dagreD3.Renderer();

        var oldDrawNodes = renderer.drawNodes();
        renderer.drawNodes(function(graph, root) {
            var svgNodes = oldDrawNodes(graph, root);
            svgNodes.each(function(u) {
                var node = d3.select(this);
                node.classed({
                    'connected' : graph.node(u).connected,
                    'source'    : graph.node(u).source,
                    'target'    : graph.node(u).target
                });
            });
            return svgNodes;
        });

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

        svg.selectAll('.target .connected')
            .attr('fill', 'gray');

        svg.attr('width', layout.graph().width + 40)
           .attr('height', layout.graph().height + 40);

    }

    function connect(source, target) {
        debug.info('connecting', source, target);
        var cmd = self.pathname + '.connect("'
                + source.data()[0] + '","'
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
