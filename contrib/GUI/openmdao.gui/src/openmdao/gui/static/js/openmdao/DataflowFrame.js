
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.DataflowFrame = function(id, project, pathname, prop_fn) {
    openmdao.DataflowFrame.prototype.init.call(this,id,'Dataflow: '+pathname,[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        pane = new openmdao.DataflowPane(jQuery('#'+id), project, pathname, 'Dataflow', prop_fn);

    self.pathname = false;

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid dataflow data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            if (message[1].hasOwnProperty('Dataflow')) {
                var dataflow = message[1].Dataflow;
                if (typeof dataflow === 'string') {
                    dataflow = jQuery.parseJSON(dataflow);
                }
                pane.loadData(dataflow);
            }
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the schematic with data from the project */
    this.update = function() {
        pane.update();
    };

    /** set the pathname of the object for which to display the dataflow */
    this.showDataflow = function(path) {
        // if not already showing dataflow for this pathname
        if (path !== self.pathname) {
            if (self.pathname !== false) {
                project.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            self.setTitle('Dataflow: '+path);
            pane.showDataflow(path);
            project.addListener(path, handleMessage);
        }
    };

    /** get the pathname for the current dataflow */
    this.getPathname = function() {
        return self.pathname;
    };

    project.project_ready.always(function() {
        self.showDataflow(pathname);
    });
};

/** set prototype */
openmdao.DataflowFrame.prototype = new openmdao.BaseFrame();
openmdao.DataflowFrame.prototype.constructor = openmdao.DataflowFrame;
