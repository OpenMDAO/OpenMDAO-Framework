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
        figures = {},
        expand_nodes = {},    // expanded state for expandable nodes (arrays and vartrees)
        connection_data = {},   // cache of most recently fetched connection data
        showAllVariables = true;  // show all vars vs only connected vars

    self.elm.css({'position':'relative', 'height':'100%',
                  'overflow':'hidden', 'max-height': 0.8*jQuery(window).height()});

    self.pathname = null;


    /** populate connections and variable selectors with source and dest variables */
    function loadConnectionData(data) {
        var graph = new joint.dia.Graph();

        var paper = new joint.dia.Paper({
            el: jQuery(self.id),
            width: 600,
            height: 200,
            model: graph
        });

        var rect = new joint.shapes.basic.Rect({
            position: { x: 100, y: 30 },
            size: { width: 100, height: 30 },
            attrs: { rect: { fill: 'blue' }, text: { text: 'my box', fill: 'white' } }
        });

        var rect2 = rect.clone();
        rect2.translate(300);

        var link = new joint.dia.Link({
            source: { id: rect.id },
            target: { id: rect2.id }
        });

        graph.addCells([rect, rect2, link]);
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
