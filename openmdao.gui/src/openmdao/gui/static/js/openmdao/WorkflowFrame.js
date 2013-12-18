
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.WorkflowFrame = function(id, project, pathname) {
    openmdao.WorkflowFrame.prototype.init.call(this,id,'Workflow: '+pathname,[]);

    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var self = this,
        pane = new openmdao.WorkflowPane(jQuery('#'+id),project,pathname,'Workflow');

    self.pathname = false;

    function handleMessage(message) {
        if (message.length !== 2 || message[0] !== self.pathname) {
            debug.warn('Invalid component data for:',self.pathname,message);
            debug.warn('message length',message.length,'topic',message[0]);
        }
        else {
            if (message[1].hasOwnProperty('Workflow')) {
                var workflow = message[1].Workflow;
                if (typeof workflow === 'string') {
                    workflow = jQuery.parseJSON(workflow);
                }
                pane.loadData(workflow);
            }
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** update the schematic with data from the project */
    this.update = function() {
        pane.showWorkflow(self.pathname);
    };

    /** set the pathname of the object for which to display the workflow */
    this.showWorkflow = function(path) {
        // if not already showing workflow for this pathname
        if (path !== self.pathname) {
            if (self.pathname !== false) {
                project.removeListener(self.pathname, handleMessage);
            }
            self.pathname = path;
            self.setTitle('Workflow: '+path);
            pane.showWorkflow(path);
            project.addListener(path,handleMessage);
        }
    };

    /** get the pathname for the current workflow */
    this.getPathname = function() {
        return self.pathname;
    };

    project.project_ready.always(function() {
        self.showWorkflow(pathname);
    });
};

/** set prototype */
openmdao.WorkflowFrame.prototype = new openmdao.BaseFrame();
openmdao.WorkflowFrame.prototype.constructor = openmdao.WorkflowFrame;
