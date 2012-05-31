
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFrame = function(id,model,pathname) {
    openmdao.WorkflowFrame.prototype.init.call(this,id,'Workflow: '+pathname,[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        pane = new openmdao.WorkflowPane(jQuery('#'+id),model,pathname,'Workflow');

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid component data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            if (message[1].hasOwnProperty('Workflow')) {
                pane.loadData(message[1].Workflow);
            }
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the schematic with data from the model */
    this.update = function() {
        pane.showWorkflow(self.pathname);
    };

    /** set the pathname of the object for which to display the workflow */
    this.showWorkflow = function(path) {
        if (path !== self.pathname) {
           if (self.pathname) {
                model.removeListener(self.pathname, handleMessage);
            }
            // if not already showing workflow for this pathname
            self.pathname = path;
            self.setTitle('Workflow: '+path);
            pane.showWorkflow(path);
            model.addListener(path,handleMessage);
        }
    };

    /** get the pathname for the current workflow */
    this.getPathname = function() {
        return self.pathname;
    };

    if (pathname && pathname.length > 0) {
        this.showWorkflow(pathname);
    }

};

/** set prototype */
openmdao.WorkflowFrame.prototype = new openmdao.BaseFrame();
openmdao.WorkflowFrame.prototype.constructor = openmdao.WorkflowFrame;
